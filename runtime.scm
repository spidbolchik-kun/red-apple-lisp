(include "dt.scm")


(define (map-over ls) (lambda (fn) (map fn ls)))
(define (filter-over ls) (lambda (fn) (filter fn ls)))


(define-structure mspm-error path data)


(define EXN #!void)
(define EXN2 #!void)

(define (ra::display-in-red string #!key (stderr #t))
  (print port: (if stderr (current-error-port) (current-output-port))
         "\033[91m"
         string
         "\033[0m"))


(define (ra::display-err string) (display string (current-error-port)))


(define (ra::display-error-head error-message-symbol)
  (define message
    (string-append
      "(˵ ͡° ͜ʖ ͡°˵) "
      (string-map (lambda (c) (if (equal? c #\-) #\space c))
                  (symbol->string error-message-symbol))
      "\n"))
  (define border (make-string (- (string-length message) 2) #\=))
  (string-set! border (- (string-length border) 1) #\newline)
  (ra::display-in-red
    (string-append border message border)))


(define (ra::display-error-code code range)
  (define line-start (car range))
  (define line-end (caddr range))
  
  ;; Check if line numbers are negative (no source location)
  (if (or (< line-start 0) (< line-end 0))
    ;; No source location - just display the code without line numbers
    (begin
      (display code (current-error-port))
      (display "\n\n" (current-error-port)))
    ;; Normal case - display code with line numbers
    (let ()
      (define line-numbers
        ((map-over (iota (+ (- line-end line-start) 1) line-start))
         (lambda (n)
           (define str (number->string n))
           (string-append
             (make-string
               (- (string-length (number->string line-end))
                  (string-length str))
               #\space)
             str
             " "))))

      (define (enumerate-lines char-list numbers)
        (if (null? numbers)
          (list char-list)
          (if (equal? (car char-list) #\newline)
            (cons (cons #\newline (string->list (car numbers)))
                  (enumerate-lines (cdr char-list) (cdr numbers)))
            (cons (list (car char-list))
                  (enumerate-lines (cdr char-list) numbers)))))

      (display
        (list->string
          (apply append
            (enumerate-lines
              (append (string->list (car line-numbers))
                      (string->list code))
              (cdr line-numbers))))
        (current-error-port))

      (display "\n\n" (current-error-port)))))


(define (ra::handle-crash-fn thunk)
  (with-exception-catcher
    (lambda (e)
      (set! EXN e)
      (if (and (error-exception? e) (mspm-error? (error-exception-message e)))
        (with-exception-catcher
          (lambda (er) (set! e er))
          (lambda ()
            (apply error (mspm-error-data (error-exception-message e))))))

      (if (error-exception? e)
        (if (symbol? (error-exception-message e))
          (ra::display-error e)
          (raise e))
        (raise e)))
    thunk
))

(define (ra::display-error e)
  (define (display-param param)
    (ra::display-err param)
    (ra::display-err "\n"))
  
  (define (display-key-value key value)
    (ra::display-err key)
    (ra::display-err " = ")
    (ra::display-err value)
    (ra::display-err "\n"))
  
  (define (display-macro-expansion-info error-params)
    (define macro-info (car error-params))
    (if (not (equal? macro-info #!void))
      (let ()
        (define error-code-range (car macro-info))
        (define error-expr (cadr macro-info))
        (ra::display-err (string-append "\033[96mthe result of macro expansion at\033[0m\n"))
        (ra::display-error-code error-expr error-code-range))))
  
  (define (show-runtime-error error-params)
    (define error-module-path (cadr error-params))
    (define error-code-range (caddr error-params))
    (define error-expr (cadddr error-params))
    (ra::display-err (string-append "\033[96m" error-module-path "\033[0m\n"))
    (ra::display-error-code error-expr error-code-range))
  
  (define (display-with-context message params)
    (show-runtime-error (last params))
    (if (not (null? (cdr params)))
      (display-param (car params))))
  
  (ra::display-error-head (error-exception-message e))

  (case (error-exception-message e)
    ((contract-violation)
     (let ((params (error-exception-parameters e)))
       (define callee-params (last params))
       (define callee-module-path (cadr callee-params))
       (define callee-code-range (caddr callee-params))
       (define callee-expr (cadddr callee-params))
       (define callee-vars (car params))
       
       (display-macro-expansion-info callee-params)
       (if (null? (cdr params))
         ;; No caller - direct assertion failure at top level
         (show-runtime-error callee-params)
         ;; Has caller - show full call stack
         (let ((caller-params (cadr params)))
           (define caller-module-path (cadr caller-params))
           (define caller-code-range (caddr caller-params))
           (define caller-expr (cadddr caller-params))
           (define callee-name (last caller-params))

           (ra::display-err (string-append "\033[96m" caller-module-path "\033[0m\n"))
           (ra::display-error-code caller-expr caller-code-range)
           (ra::display-err "called \033[93m")
           (ra::display-err callee-name)
           (ra::display-err " \033[0m")
           (ra::display-err "violating\n\n")
           (ra::display-err (string-append "\033[96m" callee-module-path "\033[93m\n"))
           (ra::display-error-code callee-expr callee-code-range)
           (if (not (null? (ra::dictionary-alist callee-vars)))
             (begin
               (ra::display-err "\033[0mwhere:\n")
               ((map-over (ra::dictionary-alist callee-vars))
                (lambda (kv)
                  (ra::display-err (car kv))
                  (ra::display-err " = ")
                  (ra::print (cdr kv)))))))))
    )

    ((duplicate-definitions)
     (let ((error-params (cdar (error-exception-parameters e))))
       (display-macro-expansion-info error-params)
       (let ((error-module-path (cadr error-params))
             (error-code-range (caddr (caddr error-params)))
             (error-expr (cadddr error-params)))
         (ra::display-err (string-append "\033[96m" error-module-path "\033[0m\n"))
         (ra::display-error-code error-expr error-code-range)))
    )
    
    ((not-a-callable-value)
     (let ((value (car (error-exception-parameters e))))
       (display-macro-expansion-info (last (error-exception-parameters e)))
       (show-runtime-error (last (error-exception-parameters e)))
       (display-param value))
    )
    
    ((required-args-not-passed)
     (let ((not-passed (car (error-exception-parameters e))))
       (display-macro-expansion-info (last (error-exception-parameters e)))
       (show-runtime-error (last (error-exception-parameters e)))
       (ra::display-err
         (apply string-append
           (map (lambda (a) (string-append (car a) " "))
                (filter (lambda (a) (equal? 'list-ref (caadr a)))
                        (car not-passed)))))
       (ra::display-err "\n")
       (if (not (null? (cadr not-passed)))
         (ra::display-err (cadr not-passed))))
    )
    
    ((default-passed-from-outside unexpected-positional-arg)
     (display-macro-expansion-info (last (error-exception-parameters e)))
     (show-runtime-error (last (error-exception-parameters e)))
     (display-param (car (error-exception-parameters e)))
    )
    
    ((unbound-variable)
     (let ((params (error-exception-parameters e)))
       (if (null? params)
         (display-param "Unknown variable")
         (begin
           (display-macro-expansion-info (last params))
           (show-runtime-error (last params)))))
    )
    
    ((attempted-to-use-unbound-values-in-macro)
     (display-macro-expansion-info (last (error-exception-parameters e)))
     (show-runtime-error (last (error-exception-parameters e)))
    )
    
    ((unexpected-keyword-arg arg-passed-twice)
     (let ((params (error-exception-parameters e)))
       (display-macro-expansion-info (last params))
       (display-with-context (error-exception-message e) params)
       (let ((arg (car params)))
         (display-key-value (car arg) (cadr arg))))
    )
    
    ((not-a-list-or-dict positional-after-key)
     (let ((params (error-exception-parameters e)))
       (display-macro-expansion-info (last params))
       (display-with-context (error-exception-message e) params))
    )
    
     ((get-operation-not-supported)
      (let ((params (error-exception-parameters e)))
        (define call-info (car params))
        (define structure (cadr params))
        (define itemgetter-path (caddr params))
        
        ;; Display macro expansion info if available
        (display-macro-expansion-info call-info)
        ;; Display the code segment from call-info
        (show-runtime-error call-info)
        
        (ra::display-err "trying to get from: ")
        (ra::print structure)
        
        (ra::display-err "item: ")
        (ra::print itemgetter-path))
     )
    
    ((either-group-already-passed)
     (let ((params (error-exception-parameters e)))
       (display-macro-expansion-info (last params))
       (show-runtime-error (last params))
       (let ((var-info (car params)))
         (display-key-value (car var-info) (last var-info))
         (ra::display-err "conflicts with ")
         (ra::display-in-red (cadr var-info))
         (ra::display-err "\n")))
    )
    
    ((no-such-file)
     (display-param (car (error-exception-parameters e))))
    
    (else
      (let ((error-params (cdar (error-exception-parameters e))))
        (display-macro-expansion-info error-params)
        (let ((error-module-path (cadr error-params))
              (error-code-range (cadr (caddr error-params)))
              (error-expr (cadddr error-params)))
          (ra::display-err (string-append "\033[96m" error-module-path "\033[0m\n"))
          (ra::display-error-code error-expr error-code-range))))))


(define-syntax ra::handle-crash
  (syntax-rules ()
    ((_ e ...)
     (ra::handle-crash-fn (lambda () e ...)))))


(define (ra::alist-set alist key value)
  (if (null? alist)
    (list (cons key value))
    (if (equal? (caar alist) key)
      (cons (cons key value) (cdr alist))
      (cons (car alist)
            (ra::alist-set (cdr alist) key value)))))


(define (ra::prepare-call-args args call-info)
  (define (check-rest args)
    (if (null? args)
      '()
      (if (and (equal? (caar args) "&")
               (not (or (list? (ra::erase-all-labels (cadar args)))
                        (ra::dictionary? (ra::erase-all-labels (cadar args))))))
        (error 'not-a-list-or-dict (cadar args) call-info)
        (cons (car args) (check-rest (cdr args))))))

  (define (separate-pos-key args #!key (pos-cont identity))
    (if (null? args)
      (cons (pos-cont '()) '())
      (if (or (equal? (caar args) 'keyword)
              (and (equal? (caar args) "&")
                   (ra::dictionary? (ra::erase-all-labels (cadar args)))))
        (cons (pos-cont '()) args)
        (separate-pos-key (cdr args)
          pos-cont: (lambda (acc) (pos-cont (cons (car args) acc)))))))

  (define (check-no-pos args)
    (define (positional? arg)
      (or (equal? 'positional (car arg))
          (and (equal? (car arg) "&")
               (list? (ra::erase-all-labels (cadr arg))))))
    (let ((res (filter positional? args)))
      (if (not (= (length res) 0))
        (error 'positional-after-key (cadar res) call-info)
        args)))

  (define (flatten-pos args)
    (if (null? args)
      '()
      ((if (equal? "&" (caar args)) append cons)
       (cadar args)
       (flatten-pos (cdr args)))))

  (define (flatten-kw args)
    (if (null? args)
      '()
      (if (equal? "&" (caar args))
        (append (ra::dictionary-alist (ra::erase-all-labels (cadar args)))
                (flatten-kw (cdr args)))
        (cons (cons (cadar args) (caddar args))
              (flatten-kw (cdr args))))))

  (let ((separated (separate-pos-key (check-rest args))))
    (check-no-pos (cdr separated))
    (cons (flatten-pos (car separated))
          (flatten-kw (cdr separated)))))


(define-structure ra::position line col)

(define-structure ra::call-info path pos-start pos-end sexp)

(define-structure ra::delayed-rest arg-ls rest-decl)

(define-structure ra::callable-meta decl pa ns either-forbidden already-passed delayed-rest called kw-rest name)


(define (ra::init-callable-meta ns decl name)
  (make-ra::callable-meta
    decl #f ns ra::empty-dictionary '() #!void #f ra::empty-dictionary name))


(define (ra::wrap-callable-in-meta callable meta)
  (make-ra::procedure callable meta ra::empty-dictionary))


(define (ra::list-slice ls start end)
  (let ((dropped (drop ls (min (length ls) start))))
    (if (equal? end +inf.0)
      dropped
      (take dropped (max 0 (- end start))))))


(define (ra::ns-ref ref-fn-or-ls code)
  (let loop ((ref-ls (if (list? ref-fn-or-ls)
                       ref-fn-or-ls
                       (list ref-fn-or-ls))))
    (let ((res ((car ref-ls))))
      (if (equal? res #!unbound)
        (error 'unbound-variable code)
        (if (eq? res ra::unbound)
          (if (= (length ref-ls) 1)
            (error 'unbound-variable code)
            (loop (cdr ref-ls)))
          (if (eq? res ra::me-hole)
            (error 'attempted-to-use-unbound-values-in-macro code)
            res))))))


(define-structure ra::ns current parent prefix)


(define ra::macro-prefixes '())
(define ra::scheme-code-cont identity)


(define (ra::push-scheme-statement! statement)
  (define cont ra::scheme-code-cont)
  (set! ra::scheme-code-cont
    (lambda (acc) (cont (cons statement acc)))))


(define (ra::ns-set-var ns sym val #!key mut)
  ((if mut ra::ns-current-set! ra::ns-current-set)
   ns
   (make-ra::dictionary
     (let ((gensym-counter (ra::get-label sym 'gensym-counter #!void)))
       (cons (cons (if (equal? gensym-counter #!void) sym gensym-counter) val)
             (ra::dictionary-alist (ra::ns-current ns)))))))


(define (ra::ns-merge ns dict #!key mut)
  ((if mut ra::ns-current-set! ra::ns-current-set)
   ns
   (ra::dictionary-merge/left-overrides dict (ra::ns-current ns))))


(define (ra::assignment->ns assignments val)
  (define (get-node val node)
    (case (car node)
      ((list-slice) (ra::list-slice val (cadr node) (cddr node)))
      ((kv-pick) (ra::dictionary-pick val (cdr node)))
      ((kv-diff) (ra::dictionary-delete-many val (cdr node)))
      (else (ra::get* val (cdr node)))))

  (define (getter val nodes)
    (if (null? nodes)
      val
      (getter (get-node val (car nodes)) (cdr nodes))))

  ((map-over assignments)
   (lambda (assignment)
     (cons (car assignment)
           (getter val (cdr assignment))))))


(define (ra::remove-fst-getter assignments)
  ((map-over assignments)
   (lambda (assignment)
     (cons (car assignment) (cddr assignment)))))


(define (ra::passable-as-kw assignments)
  (map car
    ((filter-over assignments)
     (lambda (a) (= (length (cdr a)) 1)))))


(define (ra::push-to-ns meta assignments data)
  (ra::callable-meta-already-passed-set
    (ra::callable-meta-ns-set
      meta
      (ra::dictionary-merge/left-overrides
        (make-ra::dictionary (ra::assignment->ns assignments data))
        (ra::callable-meta-ns meta)))
    (append (ra::callable-meta-already-passed meta)
            (ra::passable-as-kw assignments))))


(define (ra::push-alist-to-ns meta alist)
  (ra::callable-meta-ns-set
    meta
    (ra::dictionary-merge/left-overrides
      (make-ra::dictionary
        (map (lambda (kv) (cons (car kv) (cdr kv))) alist))
      (ra::callable-meta-ns meta))))


(define (ra::push-kw-rest-to-ns meta)
  (ra::callable-meta-kw-rest-set
    (ra::callable-meta-ns-set
      meta
      (ra::dictionary-merge/left-overrides
        (make-ra::dictionary
          (ra::assignment->ns
            (cdr (assoc "#key-rest" (ra::callable-meta-decl meta)))
            (ra::callable-meta-kw-rest meta)))
        (ra::callable-meta-ns meta)))
    ra::empty-dictionary))


(define (ra::push-unpassed-default-and-key arg-alist ns)
  (if (null? arg-alist)
    ns
    (ra::push-unpassed-default-and-key
      (cdr arg-alist)
      (ra::ns-set-var
        ns
        (caar arg-alist)
        ((cdar arg-alist) ns)))))


(define (ra::separate pred ls)
  (cons (filter pred ls)
        (filter (lambda (x) (not (pred x))) ls)))


(define (ra::call call-info callable args)
  (let* ((callable (force callable))
         (callable* (if (ra::procedure? callable)
                      (ra::procedure-code callable)
                      callable))
         (prepared-args (ra::prepare-call-args args call-info))
         (positional (car prepared-args))
         (kw (cdr prepared-args))
         (meta (if (ra::procedure? callable) (ra::procedure-meta callable) #!void)))

    (define (pos-pop pos-decl item-spec)
      (define (list-slice? item-spec)
        (equal? (car item-spec) 'list-slice))

      (define to-sub
        (if (list-slice? item-spec)
          (- (cddr item-spec) (cadr item-spec))
          1))

      (define (shift item-spec)
        (if (list-slice? item-spec)
          (cons 'list-slice
            (cons (- (cadr item-spec) to-sub)
                  (- (cddr item-spec) to-sub)))
          (cons 'list-ref (- (cdr item-spec) to-sub))))

      (define (item-spec-lt? i1 i2)
        (< ((if (list-slice? i1) cadr cdr) i1)
           ((if (list-slice? i2) cadr cdr) i2)))

      (cons
        ((filter-over pos-decl)
         (lambda (item) (equal? (cadr item) item-spec)))
        ((map-over
           ((filter-over pos-decl)
            (lambda (item) (not (equal? (cadr item) item-spec)))))
         (lambda (item)
           (if (item-spec-lt? (cadr item) item-spec)
             item
             (cons (car item) (cons (shift (cadr item)) (cddr item))))))))

    (define (push-delayed-rest meta)
      (define delayed-rest (ra::callable-meta-delayed-rest meta))
      (if (equal? #!void delayed-rest)
        meta
        (ra::callable-meta-delayed-rest-set
          (ra::push-to-ns meta (cdr delayed-rest) (car delayed-rest))
          #!void)))

    (define (pass-as-pos meta arg-val)
      (define (redo meta) (pass-as-pos meta arg-val))
      (define decl (ra::callable-meta-decl meta))
      (define delayed-rest (ra::callable-meta-delayed-rest meta))

      (if (not (equal? #!void delayed-rest))
        (let ((new-arg-ls (append (car delayed-rest) (list arg-val))))
          (if (equal? (cddr (cadadr delayed-rest)) (length new-arg-ls))
            (ra::callable-meta-delayed-rest-set
              (ra::push-to-ns meta (cdr delayed-rest) new-arg-ls)
              #!void)
            (ra::callable-meta-delayed-rest-set
              meta
              (cons new-arg-ls (cdr delayed-rest)))))
        (let ((pos (cdr (assoc "#positional" decl)))
              (kw (cdr (assoc "#key" decl))))
          (if (not (null? pos))
            (let ((popped (pos-pop pos (cadar pos))))
              (if (equal? (car (cadaar popped)) 'list-slice)
                (redo
                  (ra::callable-meta-decl-set
                    (ra::callable-meta-delayed-rest-set
                      meta
                      (cons '() (car popped)))
                    (ra::alist-set decl "#positional" (cdr popped))))
                (ra::callable-meta-decl-set
                  (ra::push-to-ns meta (ra::remove-fst-getter (car popped)) arg-val)
                  (ra::alist-set decl "#positional" (cdr popped)))))
            (if (null? kw)
              (error 'unexpected-positional-arg arg-val call-info)
              (ra::push-alist-to-ns
                (ra::callable-meta-decl-set
                  (ra::callable-meta-already-passed-set
                    meta
                    (cons (caar kw) (ra::callable-meta-already-passed meta)))
                  (ra::alist-set decl "#key" (cdr kw)))
                (list (cons (caar kw) arg-val))))))))

    (define (item-spec-by-kw decl kw)
      (if (null? decl)
        #f
        (if (and (= (length (cdar decl)) 1)
                 (equal? (caar decl) kw))
          (cadar decl)
          (item-spec-by-kw (cdr decl) kw))))

    (define (kw-pop decl kw allow-default #!key (cont identity))
      (define (next)
        (kw-pop (cdr decl) kw allow-default
          cont: (lambda (acc) (cont (cons (car decl) acc)))))
      (if (null? decl)
        (list #f #f (cont '()))
        (if (equal? (caar decl) "#key-rest")
          (next)
          (let ((res
            (if (equal? (caar decl) "#positional")
              (let ((spec (item-spec-by-kw (cdar decl) kw)))
                (if (not spec)
                  (cons '() (cdar decl))
                  (pos-pop (cdar decl) spec)))
              (ra::separate
                (if (equal? (caar decl) "#either")
                  (lambda (a) (member kw a))
                  (lambda (a) (equal? kw (car a))))
                (cdar decl)))))
            (if (null? (car res))
              (next)
              (if (and (not allow-default) (equal? (caar decl) "#default"))
                (error 'default-passed-from-outside kw call-info)
                (let ((new-arg-grp (cons (caar decl) (cdr res))))
                  (list (caar decl) (car res) (cont (cons new-arg-grp (cdr decl)))))))))))

    (define (pass-as-kw meta kw arg-val)
      (define is-rec (ra::callable-meta-called meta))
      (if (member kw (ra::callable-meta-already-passed meta))
        (error 'arg-passed-twice (list kw arg-val) call-info))

      (let ((res (ra::get* (ra::callable-meta-either-forbidden meta) kw)))
        (if (not (equal? #!void res))
          (error 'either-group-already-passed (list kw res arg-val) call-info)))

      (if (and is-rec
               (> (string-length kw) 0)
               (equal? (string-ref kw 0) #\$))
        (let* ((kw* (substring kw 1 (string-length kw)))
               (arg-val* (force (ra::get* (ra::callable-meta-ns meta) kw*))))
          (with-exception-catcher
            (lambda (e)
              (apply error
                (append (list "during $-application" kw)
                        (list (error-exception-message e))
                        (error-exception-parameters e))))
            (lambda ()
              (pass-as-kw meta kw*
                (ra::call #f arg-val (list (list 'positional arg-val*)))))))
        (ra::callable-meta-already-passed-set
          (let ((res (kw-pop (ra::callable-meta-decl meta) kw is-rec)))
            (if (not (car res))
              (if (null? (cdr (assoc "#key-rest" (ra::callable-meta-decl meta))))
                (error 'unexpected-keyword-arg (list kw arg-val) call-info)
                (ra::callable-meta-kw-rest-set
                  meta
                  (ra::dictionary-set
                    (ra::callable-meta-kw-rest meta) kw arg-val)))
              (ra::callable-meta-decl-set
                (if (equal? (car res) "#positional")
                  (ra::push-to-ns
                    meta
                    (if (equal? (caadar (cadr res)) 'list-ref)
                      (ra::remove-fst-getter (cadr res))
                      (cadr res))
                    arg-val)
                  (if (equal? (car res) "#either")
                    (ra::callable-meta-either-forbidden-set
                      (ra::push-alist-to-ns
                        (ra::push-alist-to-ns
                          meta
                          (map (lambda (a) (cons a #!void)) (caadr res)))
                        (list (cons kw arg-val)))
                      (ra::dictionary-merge/left-overrides
                        (make-ra::dictionary
                          (map (lambda (a) (cons a kw))
                               (filter (lambda (a) (not (equal? a kw))) (caadr res))))
                        (ra::callable-meta-either-forbidden meta)))
                    (ra::push-alist-to-ns meta (list (cons kw arg-val)))))
                (caddr res))))
          (cons kw (ra::callable-meta-already-passed meta)))))

    (define (pass-all-pos meta all-pos)
      (if (null? all-pos)
        meta
        (pass-all-pos (pass-as-pos meta (car all-pos))
                      (cdr all-pos))))

    (define (pass-all-kw meta all-kw)
      (if (null? all-kw)
        meta
        (pass-all-kw (pass-as-kw meta (caar all-kw) (cdar all-kw))
                     (cdr all-kw))))

    (define (perform-call meta)
      (define new-meta
        (ra::push-kw-rest-to-ns
          (pass-all-kw (pass-all-pos meta positional) kw)))

      (define (some-are-required? pos)
        (and (not (null? pos)) (equal? 'list-ref (caadar pos))))

      (define new-decl (ra::callable-meta-decl new-meta))
      (define new-pos (cdr (assoc "#positional" new-decl)))
      (define new-either (cdr (assoc "#either" new-decl)))

      (define (call)
        (define ns
          (make-ra::dictionary
            (list-sort
              (lambda (x y) (string<=? (car x) (car y)))
              (ra::dictionary->list
                (ra::callable-meta-ns
                 ((if (ra::callable-meta-called meta)
                    identity
                    (lambda (meta) (ra::push-to-ns meta new-pos '())))
                  (push-delayed-rest new-meta)))))))

        (with-exception-catcher
          (lambda (e)
            (set! EXN2 e)
            (apply error
              `(,(error-exception-message e)
                ,(ra::dictionary-pick ns (last call-info))
                ,(append call-info (list (ra::callable-meta-name new-meta)))
                ,@(error-exception-parameters e))))
          (lambda () (apply callable* (map cdr (ra::dictionary-alist ns))))))

        (if (ra::callable-meta-called meta)
          (call)
          (if (or (some-are-required? new-pos) (not (null? new-either)))
            (if (ra::callable-meta-pa new-meta)
              (error 'required-args-not-passed (list new-pos new-either) call-info)
              (ra::procedure-meta-set
                callable
                (ra::callable-meta-pa-set new-meta #t)))
            (call))))

      (if (procedure? callable*)
        (perform-call meta)
        (if (or (list? callable*) (ra::dictionary? callable*) (string? callable*))
          (with-exception-catcher
            (lambda (e)
              (if (and (error-exception? e)
                       (equal? (error-exception-message e) 'unsupported-data))
                (apply error
                  `(get-operation-not-supported
                    ,call-info
                    ,@(error-exception-parameters e)))
                (raise e)))
            (lambda ()
              (ra::get callable* positional
                stop-on-void: (if (assoc "stop-on-nil" kw) #t #f))))
          (error 'not-a-callable-value callable* call-info)))))

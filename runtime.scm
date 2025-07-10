(define (map-over ls) (lambda (fn) (map fn ls)))
(define (filter-over ls) (lambda (fn) (filter fn ls)))


(define-structure mspm-error path data)


(define-structure ra::obj data hidden-meta visible-meta)


(define EXN #!void)


(define (ra::display-in-red-with-newline string #!key (stderr #t))
  (print port: (if stderr (current-error-port) (current-output-port))
         "\033[91m"
         string
         "\033[0m\n"))


(define (ra::handle-crash-fn thunk)
  (with-exception-catcher
    (lambda (e)
      (define (string-strip-beginning-spaces str #!key (i 0))
        (if (equal? (string-length str) i)
          ""
          (if (not (equal? (string-ref str i) #\space))
            (substring str i (string-length str))
            (string-strip-beginning-spaces str i: (+ i 1)))))

      (set! EXN e)

      (if (error-exception? e)
        (if (equal? (error-exception-message e) 'contract-violation)
          (let ()
            (define callee-params (last (error-exception-parameters e)))
            (define callee-module-path (car callee-params))
            (define callee-code-range (cadr callee-params))
            (define callee-expr (caddr callee-params))

            (define caller-params (cadr (error-exception-parameters e)))
            (define caller-module-path (car caller-params))
            (define caller-code-range (cadr caller-params))
            (define caller-expr (caddr caller-params))

            (define callee-name (last caller-params))

            (define callee-vars (car (error-exception-parameters e)))

            (ra::display-in-red-with-newline
              (string-append
                "=====================================\n"
                "(˵ ͡° ͜ʖ ͡°˵) contract violation error\n"
                "====================================="))

            (display
              (string-append
                caller-expr
                "\n\nfrom "
                caller-module-path
                "@"
                caller-code-range
                "\n")
              (current-error-port))

            (display "called \033[93m" (current-error-port))
            (display callee-name (current-error-port))
            (display "\n\033[0m" (current-error-port))
            (display "violating \033[93m" (current-error-port))
            (display (string-strip-beginning-spaces callee-expr) (current-error-port))
            (display "\033[0m\n" (current-error-port))
            (display
              (string-append
                "in "
                callee-module-path
                "@"
                callee-code-range
                "\n")
              (current-error-port))
            (display "\033[0m" (current-error-port))


            (display "\n\033[0mwhere:\n" (current-error-port))

            ((map-over (ra::dictionary-alist callee-vars))
             (lambda (kv)
               (display (car kv))
               (display " = ")
               (pp (cdr kv))))

            (exit))
          (error e))
        (error e)))
    thunk
))


(define-syntax ra::handle-crash
  (syntax-rules ()
    ((_ e ...)
     (ra::handle-crash-fn (lambda () e ...)))))


(define (ra::data obj-or-scheme-val)
  (if (ra::obj? obj-or-scheme-val)
    (ra::obj-data obj-or-scheme-val)
    obj-or-scheme-val))


(define (ra::over-data f obj-or-scheme-val)
  (if (ra::obj? obj-or-scheme-val)
    (ra::obj-data-set
      obj-or-scheme-val
      (f (ra::obj-data obj-or-scheme-val)))
    (f obj-or-scheme-val)))


(define-structure ra::dictionary alist)

(define ra::empty-dictionary (make-ra::dictionary '()))


(define (ra::init-obj obj)
  (make-ra::obj obj ra::empty-dictionary ra::empty-dictionary))


(define (ra::dictionary-delete dictionary key)
  (ra::dictionary-alist-set dictionary
    (filter (lambda (kv)
              (equal? (ra::data (car kv)) (ra::data key)))
            (ra::dictionary-alist dictionary))))


(define (ra::dictionary-delete-many dictionary keys)
  (if (null? keys)
    dictionary
    (ra::dictionary-delete-many
      (ra::dictionary-delete dictionary (car keys))
      (cdr keys))))


(define (ra::alist-set alist key value)
  (if (null? alist)
    (list (cons key value))
    (if (equal? (ra::data (caar alist)) (ra::data key))
      (cons (cons key value) (cdr alist))
      (cons (car alist)
            (ra::alist-set (cdr alist) key value)))))


(define (ra::dictionary-set dictionary key value)
  (make-ra::dictionary
    (ra::alist-set (ra::dictionary-alist dictionary) key value)))


(define (ra::dictionary-empty? dict)
  (null? (ra::dictionary-alist dict)))


(define (ra::dictionary-merge d1 d2)
  (if (ra::dictionary-empty? d1)
    d2
    (ra::dictionary-merge
      (make-ra::dictionary (cdr (ra::dictionary-alist d1)))
      (let ((d1-head (car (ra::dictionary-alist d1))))
        (ra::dictionary-set d2 (car d1-head) (cdr d1-head))))))


(define (ra::alist-get key alist #!key error-on-null)
  (if (null? alist)
    (if error-on-null
      (error "no such key" key)
      #!void)
    (if (equal? (ra::data (caar alist)) (ra::data key))
      (cdar alist)
      (ra::alist-get key (cdr alist) error-on-null: error-on-null))))


(define (ra::dictionary-has? dictionary key)
  (let ((alist (ra::dictionary-alist dictionary)))
    (with-exception-catcher
      (lambda (e) #f)
      (lambda ()
        (ra::alist-get key alist error-on-null: #t)
        #t))))


(define (ra::dictionary-pick dictionary keys)
  (define alist (ra::dictionary-alist dictionary))
  (define (inner keys)
    (if (null? keys)
      '()
      (if (ra::dictionary-has? dictionary (car keys))
        (cons (cons (car keys) (ra::alist-get (car keys) alist))
              (inner (cdr keys)))
        (inner (cdr keys)))))
  (make-ra::dictionary (inner keys)))


(define (ra::get structure args #!key stop-on-void)
  (define (list-ref-or-void ls i)
    (if (and (<= 0 i) (< i (length ls)))
      (list-ref ls i)
      #!void))

  (define (inner structure args)
    (if (or (null? args) (and stop-on-void (equal? (ra::data structure) #!void)))
      structure
      (let ((structure (ra::data structure)))
        (let ((new-structure
          (cond ((ra::dictionary? structure)
                 (ra::alist-get (car args) (ra::dictionary-alist structure)))
                ((list? structure)
                 (list-ref-or-void structure (ra::data (car args))))
                (else (error "unsupported type" structure)))))
          (inner new-structure (cdr args))))))

  (if (null? args)
    (error "empty itemlist" structure args)
    (inner structure args)))


(define (ra::get* structure . args)
  (ra::get structure args))


(define (ra::set-label obj key val)
  (let ((new-obj (if (ra::obj? obj)
                   obj
                   (make-ra::obj obj #!void ra::empty-dictionary))))
    (ra::obj-visible-meta-set obj
      (ra::dictionary-set (ra::obj-visible-meta obj) key val))))


(define (ra::del-label obj key)
  (if (not (ra::obj? obj))
    obj
    (let ((new-visible
            (ra::dictionary-delete (ra::obj-visible-meta obj) key)))
      (if (or (not (equal? (ra::obj-hidden-meta obj) #!void))
              (not (ra::dictionary-empty? new-visible)))
        (ra::obj-visible-meta-set obj new-visible)
        (ra::obj-data obj)))))


(define (ra::get-label obj key)
  (if (not (ra::obj? obj))
    #!void
    (ra::get* (ra::obj-hidden-meta obj) key)))


(define (ra::prepare-call-args args)
  (define (check-rest args)
    (if (null? args)
      '()
      (if (and (equal? (caar args) "&")
               (not (or (list? (ra::data (cadar args)))
                        (ra::dictionary? (ra::data (cadar args))))))
        (error 'not-a-list-or-dict (car args))
        (cons (car args) (check-rest (cdr args))))))

  (define (separate-pos-key args #!key (pos-cont identity))
    (if (null? args)
      (cons (pos-cont '()) '())
      (if (or (equal? (caar args) 'keyword)
              (and (equal? (caar args) "&")
                   (ra::dictionary? (ra::data (cadar args)))))
        (cons (pos-cont '()) args)
        (separate-pos-key (cdr args)
          pos-cont: (lambda (acc) (pos-cont (cons (car args) acc)))))))

  (define (check-no-pos args)
    (define (positional? arg)
      (or (equal? 'positional (car arg))
          (and (equal? (car arg) "&")
               (list? (ra::data (cadr arg))))))
    (let ((res (filter positional? args)))
      (if (not (= (length res) 0))
        (error 'positional-after-key res)
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
        (append (ra::dictionary-alist (ra::data (cadar args)))
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


(define (ra::init-callable-meta decl name)
  (make-ra::callable-meta
    decl #f ra::empty-dictionary ra::empty-dictionary '() #!void #f ra::empty-dictionary name))


(define (ra::wrap-callable-in-meta callable meta)
  (make-ra::obj callable meta ra::empty-dictionary))


(define (ra::list*? obj) (list? (ra::data obj)))
(define (ra::dictionary*? obj) (ra::dictionary? (ra::data obj)))


(define (ra::list-slice ls start end)
  (let ((dropped (drop ls (min (length ls) start))))
    (if (equal? end +inf.0)
      dropped
      (take dropped (max 0 (- end start))))))


(define-structure ra::ns current parent prefix)


(define ra::modules '())
(define ra::macro-prefixes '())
(define ra::scheme-code-cont identity)


(define (ra::push-scheme-statement! statement)
  (define cont ra::scheme-code-cont)
  (set! ra::scheme-code-cont
    (lambda (acc) (cont (cons statement acc)))))


(define (ra::add-module! path)
  (define new-modules
    (cons (make-ra::ns ra::empty-dictionary #!void (list path))
          ra::modules))
  (set! ra::modules new-modules))


(define (ra::get-module path)
  (define (inner modules)
    (if (null? modules)
      (error "unknown module" path)
      (if (equal? path (car (ra::ns-prefix (car modules))))
        (car modules)
        (inner (cdr modules)))))
  (inner ra::modules))


(define (ra::ns-ref ns sym)
  (define gensym-counter (ra::get-label sym 'gensym-counter))

  (if (equal? ns #!void)
    (error "unbound variable" sym)
    (let ((to-get (if (equal? gensym-counter #!void) sym gensym-counter)))
      (if (ra::dictionary-has? (ra::ns-current ns) to-get)
        (force (ra::get* (ra::ns-current ns) to-get))
        (ra::ns-ref (ra::ns-parent ns) sym)))))


(define (ra::mod-ref mod-name sym)
  (ra::ns-ref (ra::get-module mod-name) sym))


(define (ra::ref ns sym)
  (define pref (ra::get-label sym 'prefix))

  (define (up ns pref)
    (if (equal? ns #!void)
      (error "no namespace with such prefix" pref)
      (if (equal? pref (ra::ns-prefix ns))
        ns
        (up (ra::ns-parent ns) pref))))

  (if (equal? pref #!void)
    (ra::ns-ref ns (ra::data sym))
    (if (= 1 (length pref))
      (ra::mod-ref (car pref) (ra::data sym))
      (ra::ns-ref (up ns pref) (ra::data sym)))))


(define (ra::get-ns-prefix-by-sym ns sym)
  (define probably-fn
    (with-exception-catcher (lambda (e) #f) (ra::ns-ref ns sym)))

  (if (not probably-fn)
    #f
    (let ((probably-m-prefix (assq probably-fn ra::macro-prefixes)))
      (if (not probably-m-prefix)
        #f
        (cdr probably-m-prefix)))))


(define (ra::add-macro-prefix! macro pref)
  (if (not (procedure? (ra::data macro)))
    (error "macro should be a procedure" macro)
    (let ((new-mp (cons (cons macro pref) ra::macro-prefixes)))
      (set! ra::macro-prefixes new-mp))))


(define (ra::ns-set-var ns sym val #!key mut)
  ((if mut ra::ns-current-set! ra::ns-current-set)
   ns
   (make-ra::dictionary
     (let ((gensym-counter (ra::get-label sym 'gensym-counter)))
       (cons (cons (if (equal? gensym-counter #!void) sym gensym-counter) val)
             (ra::dictionary-alist (ra::ns-current ns)))))))


(define (ra::ns-merge ns dict #!key mut)
  ((if mut ra::ns-current-set! ra::ns-current-set)
   ns
   (ra::dictionary-merge dict (ra::ns-current ns))))


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
      (ra::dictionary-merge
        (make-ra::dictionary (ra::assignment->ns assignments data))
        (ra::callable-meta-ns meta)))
    (append (ra::callable-meta-already-passed meta)
            (ra::passable-as-kw assignments))))


(define (ra::push-alist-to-ns meta alist)
  (ra::callable-meta-ns-set
    meta
    (ra::dictionary-merge
      (make-ra::dictionary
        (map (lambda (kv) (cons (car kv) (cdr kv))) alist))
      (ra::callable-meta-ns meta))))


(define (ra::push-kw-rest-to-ns meta)
  (ra::callable-meta-kw-rest-set
    (ra::callable-meta-ns-set
      meta
      (ra::dictionary-merge
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
         (callable* (ra::data callable))
         (prepared-args (ra::prepare-call-args args))
         (positional (car prepared-args))
         (kw (cdr prepared-args))
         (meta (if (ra::obj? callable) (ra::obj-hidden-meta callable) #!void)))

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
        (let ((pos (ra::alist-get "#positional" decl))
              (kw (ra::alist-get "#key" decl)))
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
              (error "unexpected positional arg" arg-val call-info)
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
                (error "default passed from outside" kw call-info)
                (let ((new-arg-grp (cons (caar decl) (cdr res))))
                  (list (caar decl) (car res) (cont (cons new-arg-grp (cdr decl)))))))))))

    (define (pass-as-kw meta kw arg-val)
      (define is-rec (ra::callable-meta-called meta))
      (if (member kw (ra::callable-meta-already-passed meta))
        (error "passed twice" kw arg-val call-info))

      (let ((res (ra::get* (ra::callable-meta-either-forbidden meta) kw)))
        (if (not (equal? #!void res))
          (error "either group already passed" kw res arg-val call-info)))

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
              (if (null? (assoc "#key-rest" (ra::callable-meta-decl meta)))
                (error "nonexistent keyword arg" kw arg-val call-info)
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
                          (map (lambda (a) (cons a #!void)) (cadr res)))
                        (list (cons kw arg-val)))
                      (ra::dictionary-merge
                        (map
                          (lambda (a) (cons a kw))
                          (filter (lambda (a) (not (equal? a kw))) (cadr res)))
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
          (ra::callable-meta-ns
            ((if (ra::callable-meta-called meta)
               identity
               (lambda (meta) (ra::push-to-ns meta new-pos '())))
             (push-delayed-rest new-meta))))

        (with-exception-catcher
          (lambda (e)
            (apply error
              `(,(error-exception-message e)
                ,(ra::dictionary-pick ns (last call-info))
                ,(append call-info (list (ra::callable-meta-name new-meta)))
                ,@(error-exception-parameters e))))
          (lambda ()
            (callable*
              (if (ra::callable-meta-called meta)
                '()
                (map car (append (cdr (assoc "#default" new-decl))
                                 (cdr (assoc "#key" new-decl)))))
              ns))))

        (if (ra::callable-meta-called meta)
          (call)
          (if (or (some-are-required? new-pos) (not (null? new-either)))
            (if (ra::callable-meta-pa new-meta)
              (error "required args not passed" (list new-pos new-either) call-info)
              (ra::obj-hidden-meta-set
                callable
                (ra::callable-meta-pa-set new-meta #t)))
            (call))))

      (if (procedure? callable*)
        (perform-call meta)
        (if (or (list? callable*) (ra::dictionary? callable*))
          (ra::get callable* positional
            stop-on-void:
              (let ((stop-on-void (assoc "stop-on-nil" kw)))
                (and stop-on-void ((cdr stop-on-void) #!void))))
          (error "not a callable value" callable* call-info)))))

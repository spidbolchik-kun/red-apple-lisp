; module system and primitive macros


(include "utils.scm")
(include "parser.scm")
(include "runtime.scm")


(define library-path "~/.local/share/red-apple-lisp/")


(define-structure macro-expansion original processed)
(define-structure module full-path ast-tree)


(define (crash err sexp)
  (error (make-mspm-error #f (list err sexp))))


(define (read-and-parse-module full-path)
  (print "read-and-parse-module: " full-path "\n")
  (let ((result
    (with-exception-catcher
      identity
      (lambda () (parse (read-file-string full-path))))))
    (if (or (no-such-file-or-directory-exception? result)
            (parsing-error? result))
      (error (make-mspm-error full-path result))
      (make-module full-path result))))


(define statements '("define" "define-macro" "do" "assign" "import" "assert"))


(define primitive-forms
  (append statements '("fn" "if" "let" "quote" "kv-quote" "and" "or")))


(define forbidden-refs (append primitive-forms '("_" "&" "=")))


(define (assert-not-forbidden-ref sexp)
  (if (member sexp forbidden-refs)
    (crash 'forbidden-ref sexp)
    sexp))


(define (assert-no-forbidden-refs-in-dest dest)
  (for-each (lambda (v) (assert-not-forbidden-ref (car v))) dest)
  dest)


(define (wrap-into-ast-obj type data)
  (if (ast-obj? data) data (make-ast-obj type data #f)))


(define (ast-obj->sexp obj)
  (define wrap
    (fn-if
      (not (one-of? (ast-obj-type obj) '(unquoted-symbol #\()))
      (wrap-into-list
        (if (equal? (ast-obj-type obj) #\{) "kv-quote" "quote"))))
  (wrap
    (if (list? (ast-obj-data obj))
      (map ast-obj->sexp (ast-obj-data obj))
      (ast-obj-data obj))))

  ;(if (list? (ast-obj-data obj))
  ;  (let* ((result-data (map ast-obj->sexp (ast-obj-data obj)))
  ;         (alist (apply append (map car result-data)))
  ;         (wrapped (wrap (map cdr result-data))))
  ;    (cons (cons (cons obj wrapped) alist) wrapped))
  ;  (let ((wrapped (wrap (ast-obj-data obj))))
  ;    (cons (list (cons obj wrapped)) wrapped))))


(define (sexp->ast-obj sexp #!key (ast-alist '()))
  (define maybe-obj (assq sexp ast-alist))
  (define obj (and maybe-obj (cdr maybe-obj)))
  (cond (maybe-obj obj)
        ((list? sexp)
         (wrap-into-ast-obj #\( (map sexp->ast-obj sexp)))
        (else (wrap-into-ast-obj 'unquoted-symbol sexp))))


(define (prefixize-head-=-chain sexp-list #!key wrap-standalone)
  (define (assignment? ident) (equal? ident "="))
  (print "prefixize-head-=-chain...\n")

  (if (assignment? (car sexp-list))
    (crash 'bad-= (car sexp-list))
    (let ((head-is-assignment
            (and (nonempty-list? (car sexp-list))
                 (equal? (caar sexp-list) "assign"))))
      (if (or (< (length sexp-list) 3)
              (not (assignment? (cadr sexp-list))))
        (cons
          (fn-if (and wrap-standalone (not head-is-assignment))
                 (wrap-into-list "assign")
                 (car sexp-list))
          (cdr sexp-list))
        (prefixize-head-=-chain
          (cons
            (if head-is-assignment
              (append (car sexp-list) (list (caddr sexp-list)))
              (list "assign" (car sexp-list) (caddr sexp-list)))
            (cdddr sexp-list)))))))


(define (prefixize-= sexp-list #!key wrap-standalone)
  (print "prefixize-=: " sexp-list "\n")
  (if (null? sexp-list)
    '()
    (let ((res (prefixize-head-=-chain sexp-list
                 wrap-standalone: wrap-standalone)))
      (cons (car res) (prefixize-= (cdr res) wrap-standalone: #t)))))


(define (bad-quotes-and-refs sexp)
  (if (list? sexp)
    (if (null? sexp)
      '()
      (if (one-of? (car sexp) '("quote" "kv-quote"))
        (if (or (not (= (length sexp) 2))
                (and (equal? (car sexp) "kv-quote")
                     (not (list? (cadr sexp)))))
          (list sexp)
          (apply append (map bad-quotes-and-refs (cadr sexp))))
        (apply append (map bad-quotes-and-refs sexp))))
    (if (one-of? sexp primitive-forms)
      (list sexp)
      '())))


(define (empty-quotes sexp)
  (if (or (not (list? sexp)) (null? sexp))
    '()
    (if (and (one-of? (car sexp) '("quote" "kv-quote"))
             (null? cadr))
      (list sexp)
      (apply append (map empty-quotes sexp)))))


(define (deduplicate-quotes sexp)
  (if (or (not (list? sexp)) (null? sexp))
    sexp
    (if (one-of? (car sexp) '("quote" "kv-quote"))
      (if (or (not (list? (cadr sexp)))
              (null? (cadr sexp)))
        (if (or (boolean? (cadr sexp)) (number? (cadr sexp)))
          (cadr sexp)
          sexp)
        (if (one-of? (caadr sexp) '("quote" "kv-quote"))
          (if (not (equal? (car sexp) (caadr sexp)))
            (crash 'incompatible-double-quotation sexp)
            (deduplicate-quotes (cadr sexp)))
          (cons (car sexp) (map deduplicate-quotes (cadr sexp)))))
      (map deduplicate-quotes sexp))))


(define (quoted-list? sexp)
  (and (nonempty-list? sexp)
       (equal? (car sexp) "quote")
       (list? (cadr sexp))))


(define (quoted-kv? sexp)
  (and (nonempty-list? sexp)
       (equal? (car sexp) "kv-quote")))


(define (quoted-string? sexp)
  (and (nonempty-list? sexp)
       (equal? (car sexp) "quote")
       (string? (cadr sexp))))


(define (quoted-structure? sexp)
  (or (quoted-list? sexp) (quoted-kv? sexp)))


(define (unquoted-list? sexp)
  (and (list? sexp)
       (not (or (quoted-list? sexp) (quoted-kv? sexp)))))


(define (get-duplicates-by fn ls)
  (filter (lambda (x) (> (length x) 1))
          (partition (equal-by fn) ls)))


(define (check-duplicate-vars var-ls)
  (let ((dups (get-duplicates-by car var-ls)))
    (if (null? dups)
      var-ls
      (crash 'duplicate-definitions
             (map (map-with car) dups)))))


(define (destructuring-identifiers sexp)
  (define (inner sexp #!key acc-or-false rest)
    (define acc
      (or acc-or-false
        (cond ((quoted-list? sexp) 0)
              ((quoted-kv? sexp) '())
              (else (crash 'not-a-quoted-structure sexp)))))
    (define body (cadr sexp))

    (define (make-level op varname arg)
      (cons varname (list (cons op arg))))

    (define (merge-levels base branches)
      (define (merge branch)
        (cons (car branch) (append (cdr base) (cdr branch))))
      (cons base (map merge branches)))

    (if (and rest
             (not (null? body))
             (not (equal? "&" (car body))))
      (crash 'expected-rest-marker body))

    (print "destructuring-identifiers..." "\n")

    (if (null? body)
      '()
      (if (equal? (car body) "&")
        (if (null? (cdr body))
          (crash 'null-rest (car body))
          (let* ((=-chain-and-rest
                  (prefixize-head-=-chain (cdr body) wrap-standalone: #t))
                 (=-chain (cdar =-chain-and-rest))
                 (rest (cdr =-chain-and-rest)))
            (cond ((and (> (length =-chain) 1) (not (string? (car =-chain))))
                   (crash 'not-an-unquoted-symbol (car =-chain)))
                  ((and (= (length =-chain) 1)
                        (not (null? rest)))
                   (crash 'wrong-rest-argcount (cdr body)))
                  ((> (length =-chain) 2)
                   (crash '=-chain-too-long))
                  ((and (= (length =-chain) 2)
                        (or (and (quoted-list? sexp)
                                 (not (quoted-list? (cadr =-chain))))
                            (and (quoted-kv? sexp)
                                 (not (quoted-kv? (cadr =-chain))))))
                   (crash 'wrong-destructuring-type (cadr =-chain)))
                  ((and (quoted-structure? (last =-chain))
                     (let ((marker
                             (find-one (equal-to "&") (cadr (last =-chain))
                               on-failure: #f)))
                       (if marker
                         (crash 'unexpected-rest-marker marker)
                         #f))))
              (else
                (let* ((next-level
                         (if (and (= (length =-chain) 1) (string? (car =-chain)))
                           '()
                           (inner (last =-chain))))
                       (getter-ls (unique (map cdadr next-level)))
                       (next-acc
                         (if (quoted-list? sexp)
                           (if (and (null? rest) (string? (car =-chain)))
                             +inf.0
                             (+ acc 1 (apply max (cons -1 getter-ls))))
                           (append getter-ls acc))))
                  (append
                    (merge-levels
                      (if (quoted-list? sexp)
                        (make-level 'list-slice (car =-chain) (cons acc next-acc))
                        (if (null? rest)
                          (make-level 'kv-diff (car =-chain) next-acc)
                          (make-level 'kv-pick (car =-chain) getter-ls)))
                      next-level)
                    (inner (list (car sexp) rest)
                      acc-or-false: next-acc
                      rest: #t)))))))
        (let* ((=-chain-and-rest
                 (prefixize-head-=-chain body wrap-standalone: #t))
               (=-chain (cdar =-chain-and-rest))
               (rest (cdr =-chain-and-rest)))
          (cond
            ((null? (length =-chain))
             (crash '=-chain-null-length =-chain))
            ((or (> (length =-chain) 3)
                 (and (quoted-list? sexp) (> (length =-chain) 2)))
             (crash '=-chain-too-long =-chain))
            (else
              (let*
                ((result
                   (if (quoted-structure? (last =-chain))
                     (cons (but-last =-chain) (inner (last =-chain)))
                     (if (unquoted-list? (last =-chain))
                       (crash 'unexpected-unquoted-list (last =-chain))
                       (cons =-chain '()))))
                 (=-chain* (car result))
                 (next-level (cdr result)))
                (let
                  ((lvl-args
                     (case (length =-chain*)
                       ((0) (if (quoted-kv? sexp)
                              (crash 'no-key =-chain*)
                              (list #f acc)))
                       ((1) (if (and (not (string? (car =-chain*)))
                                     (quoted-list? sexp))
                              (crash 'not-an-unquoted-symbol (car =-chain*))
                              (list (car =-chain*)
                                    (if (quoted-list? sexp) acc (car =-chain*)))))
                       ((2) (if (not (string? (car =-chain*)))
                              (crash 'not-an-unquoted-symbol (car =-chain*))
                              (list (car =-chain*) (cadr =-chain*)))))))
                  (append
                    (merge-levels
                      (apply make-level
                             (if (quoted-list? sexp) 'list-ref 'kv-ref)
                             lvl-args)
                      next-level)
                    (inner (list (car sexp) rest)
                      acc-or-false: (if (quoted-list? sexp)
                                      (+ acc 1)
                                      (cons (cadr lvl-args) acc))))))))))))

  (check-duplicate-vars
    (assert-no-forbidden-refs-in-dest
      (filter
        (lambda (v) (and (string? (car v)) (not (equal? (car v) "_"))))
        (inner sexp)))))


(define (fn-arguments args)
  (define kinds '("#positional" "#key" "#key-rest" "#either" "#default"))
  (define (kind-marker? arg) (one-of? arg kinds))

  (define (get-one-group args #!key (kont identity))
    (if (or (null? args) (kind-marker? (car args)))
      (cons (kont '()) args)
      (get-one-group
        (cdr args)
        kont: (lambda (acc) (kont (cons (car args) acc))))))


  ;(comp (split at: (one-of? kinds) keeping-markers: 'on-right)
  ;      (kv-from-list with-getters: [head tail])
  ;      (split-key '#key into: ['#key '#key-rest]
  ;                       with: (split once: at: (== '&) keeping-markers: 'on-right)))


  (define (separate args)
    (let ((kind (car args))
          (group-and-next (get-one-group (cdr args))))
      (cons
        (cons kind (car group-and-next))
        (if (null? (cdr group-and-next))
          '()
          (separate (cdr group-and-next))))))

  (define (check-order parsed-groups #!key (possible-kinds kinds))
    (define (new-possible-kinds kind kinds)
      (if (null? kinds)
        '()
        (if (equal? kind (car kinds))
          (cdr kinds)
          (new-possible-kinds kind (cdr kinds)))))

    (if (null? parsed-groups)
      '()
      (if (not (one-of? (caar parsed-groups) possible-kinds))
        (crash 'wrong-arg-kind-order (caar parsed-groups))
        (cons (car parsed-groups)
              (check-order (cdr parsed-groups)
                possible-kinds:
                  (new-possible-kinds (caar parsed-groups) possible-kinds))))))

  (define (assignment-length-checker wrong-len-pred)
    (lambda (assignment)
      (if (wrong-len-pred assignment)
        (crash 'wrong-assignment-chain-length assignment)
        (if (not (string? (car assignment)))
          (crash 'not-an-unquoted-symbol (car assignment))
          (if (= (length assignment) 1)
            (append assignment (list #!void))
            assignment)))))

  (define check-default
    (assignment-length-checker (lambda (a) (not (= (length a) 2)))))

  (define check-key
    (assignment-length-checker (lambda (a) (> (length a) 2))))

  (define (prefixize-=-unwrap group)
    (map cdr (prefixize-= group wrap-standalone: #t)))

  (define (check-either arg)
    (if (not (unquoted-list? arg))
      (crash 'not-an-unquoted-list arg)
      ((map-over arg)
       (map-with
         (lambda (var)
           (if (not (string? var))
             (crash 'not-an-unquoted-symbol var)
             var))))))

  (define (split-kw-rest* group #!key (kont identity))
    (if (or (null? group) (equal? (car group) "&"))
      (list (cons "#key" (kont '())) (cons "#key-rest" group))
      (split-kw-rest* (cdr group)
        kont: (lambda (acc) (kont (cons (car group) acc))))))

  (define (split-kw-rest groups)
    (if (null? groups)
      '()
      (if (equal? (caar groups) "#key")
        (append (split-kw-rest* (cdar groups)) (cdr groups))
        (cons (car groups) (split-kw-rest (cdr groups))))))

  (define (parse-group group)
    (case (string->symbol (substring (car group) 1 (string-length (car group))))
      ((positional) (destructuring-identifiers (list "quote" (cdr group))))
      ((either) (check-either (cdr group)))
      ((key) (map check-key (prefixize-=-unwrap (cdr group))))
      ((key-rest) (destructuring-identifiers (list "kv-quote" (cdr group))))
      ((default) (map check-default (prefixize-=-unwrap (cdr group))))))


  (define (one-of-kinds? obj) (one-of? obj kinds))
  (display "fn-arguments: ")
  (display args)
  (display "\n")

  (let ((dups (get-duplicates-by
                identity
                (filter one-of-kinds? args))))
    (if (not (null? dups))
      (crash 'duplicate-argument-markers dups)
      (let* ((unparsed-groups (split-kw-rest (separate (cons "#positional" args))))
             (groups
               (check-order
                 ((map-over unparsed-groups)
                  (lambda (group)
                    (cons (car group) (parse-group group))))))
             (groups ((map-over kinds)
                      (lambda (kind)
                        (let ((group (assoc kind groups)))
                          (if group group (list kind))))))
             (get-group (lambda (group) (cdr (assoc group groups)))))
        (let ((dups (get-duplicates-by
                      identity
                      (append (map car (get-group "#positional"))
                              (map car (get-group "#key"))
                              (map car (get-group "#key-rest"))
                              (apply append (get-group "#either"))
                              (map car (get-group "#default"))))))
          (if (not (null? dups))
            (crash 'duplicate-argument-variables dups)
            groups))))))


(define (parse-fn-call sexp)
  (define (keyword? obj)
    (and (string? obj) (string-suffix? ":" obj)))

  (define (parse-pairs sexp)
    (if (null? sexp)
      '()
      (cond
        ((equal? (car sexp) "&")
         (cond ((null? (cdr sexp))
                (crash 'unexpected-rest-marker (car sexp)))
               ((equal? "&" (cadr sexp))
                (crash 'unexpected-rest-marker (take sexp 2)))
               (else (cons (list (car sexp) (cadr sexp))
                           (parse-pairs (cddr sexp))))))
        ((keyword? (car sexp))
         (if (or (null? (cdr sexp))
                 (keyword? (cadr sexp))
                 (equal? "&" (cadr sexp)))
           (cons (list 'keyword (string-but-last (car sexp)) #t)
                 (parse-pairs (cdr sexp)))
           (cons (list 'keyword (string-but-last (car sexp)) (cadr sexp))
                 (parse-pairs (cddr sexp)))))
        (else
          (cons (list 'positional (car sexp))
                (parse-pairs (cdr sexp)))))))

  (define (check-order args #!key (can-be-positional #t))
    (if (null? args)
      '()
      (if (and (not can-be-positional)
               (or (equal? (caar args) 'positional))
                   (and (equal? (caar args) "&")
                        (quoted-list? (cadar args))))
        (crash 'positional-after-key (car args))
        (cons (car args)
              (check-order (cdr args)
                can-be-positional:
                  (and can-be-positional
                       (or (equal? (caar args) 'positional)
                           (and (equal? (caar args) "&")
                                (not (quoted-kv? (cadar args)))))))))))

  (check-order (parse-pairs sexp)))


(define modules-code '())
(define gensym-counter 0)


(define (inject sexp)
  (if (not (list? (ra::data sexp)))
    (ra::set-label sexp 'injected #t)
    (map inject (ra::data sexp))))


(define (set-ns-pref sexp pref)
  (if (not (list? (ra::data sexp)))
    (let ((prefix (ra::get-label sexp 'prefix))
          (injected (ra::get-label sexp 'injected)))
      (ra::del-label
        (if (and (string? (ra::data sexp))
                 (not (member (ra::data sexp) forbidden-refs))
                 (equal? prefix #!void)
                 (or (equal? injected #!void) (not injected)))
          (ra::set-label sexp 'prefix pref)
          sexp)
        'injected))
    (map (lambda (sexp2) (set-ns-pref sexp2 pref))
         (ra::data sexp))))


(define (prefixed-in-destructuring sexp)
  (let ((data (ra::data sexp))
        (prefix (ra::get-label sexp 'prefix)))
    (if (and (string? data) (not (equal? #!void prefix)))
      (list sexp)
      (if (and (list? data)
               (= (length data) 2)
               (one-of? (car data) '("quote" "kv-quote"))
               (list? (ra::data (cadr data))))
        (apply append (map prefixed-in-destructuring (ra::data (cadr data))))
        '()))))


;(define (read-and-parse-module! full-path)
;  (if (equal? #!void (find-one (equal-by full-path) modules-code))
;    (let ((parsed (read-and-parse-module full-path))
;          (sexp-code
;            (cons "do" (cadr (ast-obj->sexp (module-ast-tree parsed))))))
;      (set! modules-code (cons parsed modules-code))
;      (expand! (ast-obj->sexp (module-ast-tree parsed))
;               (ra::ns ra::empty-dictionary #!void (list full-path))
;               'statement-list
;               #!void))))


(define (assert-is-value sexp)
  (assert-not-forbidden-ref sexp)
  (if (null? sexp)
    (crash 'empty-call sexp)
    (if (and (list? sexp) (member (car sexp) statements))
      (crash 'statement-instead-of-value sexp)
      sexp)))


(define (check-ns-assignment sexp)
  (define args (cdr sexp))

  (if (< (length args) 2)
    (crash 'empty-assignment sexp)
    (if (> (length args) 3)
      (crash 'assignment-too-many-args sexp)
      (let ((args2 (but-last args)))
        (assert-is-value (last args))
        (if (string? (car args2))
          (assert-not-forbidden-ref (car args2)))
        (case (length args2)
          ((1) (if (and (not (quoted-structure? (car args2)))
                        (not (string? (car args2))))
                 (crash 'not-varname-or-destructuring sexp)
                 sexp))
          ((2) (if (not (string? (car args2)))
                 (crash 'not-varname sexp)
                 (if (not (quoted-structure? (last args2)))
                   (crash 'not-destructuring sexp)
                   sexp))))))))


(define (prepare-definition sexp)
  (define args (cdr sexp))

  (if (< (length args) 2)
    (crash 'empty-definition sexp)
    (if (list? (car args))
      (if (quoted-structure? (car args))
        (crash 'quoted-structure-in-definition sexp)
        (if (null? (car args))
          (crash 'empty-fn-definition sexp)
          (list "assign" (caar args)
            `("fn" ,(cdar args) ,@(cdr args)))))
      (if (> (length args) 3)
        (crash 'definition-too-many-args sexp)
        (cons "assign" args)))))


(define (prepare-first-statement sexp)
  (if (null? sexp)
    '()
    (let ((sexp (prefixize-head-=-chain sexp)))
      (if (nonempty-list? (car sexp))
        (if (equal? (caar sexp) "define")
          (prepare-first-statement
            (cons (prepare-definition (car sexp)) (cdr sexp)))
          (if (equal? (caar sexp) "assign")
            (let ()
              (check-ns-assignment (car sexp))
              (if (= (length (car sexp)) 4)
                `(,(list "assign" (cadr (car sexp)) (last (car sexp)))
                  ,(list "assign" (caddr (car sexp)) (cadr (car sexp)))
                  ,(cdr sexp))
                sexp))
            sexp))
        (cons (assert-is-value (car sexp)) (cdr sexp))))))


(define-structure codegen-info path is-top-level assignment-to)


(define (ns-ref-from-ci ci)
  (if (codegen-info-is-top-level ci)
    `(ra::get-module ,(codegen-info-path ci))
    'namespace))


(define (var-setter ci lval rval)
  (define ns (ns-ref-from-ci ci))
  (if (not (quoted-structure? lval))
    (let ((ci (codegen-info-assignment-to-set ci lval)))
      `(ra::ns-set-var ,ns ,lval ,(val->scheme rval ci) mut: #t))
    `(for-each
       (lambda (p)
         (ra::ns-set-var ,ns (car p) (cdr p) mut: #t))
       (ra::assignment->ns (quote ,(destructuring-identifiers lval))
                           ,(val->scheme rval ci)))))


(define (var-getter ci val)
  `(ra::ns-ref ,(ns-ref-from-ci ci) ,val))


(define (get-module-full-path path #!key ci)
  (print "get-module-full-path: " path "\n")
  (path-normalize
    (if (string-prefix? "/" path)
      path
      (if (string-prefix? "./" path)
        (string-append
          (dir-from-path (if (not ci) (current-directory) (codegen-info-path ci)))
          (substring path 2 (string-length path)))
        (string-append library-path path)))))


(define (read-and-parse-module-by-path! full-path)
  (print "read-and-parse-module-by-path!: " full-path "\n")
  (if (equal? #!void (find-one (equal-by full-path) modules-code))
    (let* ((parsed (read-and-parse-module full-path))
           (sexp-code
             (fn-if (not (equal? full-path (get-module-full-path "builtins.ra")))
                    (lambda (code) (cons '("import" ("quote" "builtins.ra")) code))
                    (cadr (ast-obj->sexp (module-ast-tree parsed))))))
      (set! modules-code (cons sexp-code modules-code))
      (let ((maybe-error
        (with-exception-catcher
          (lambda (e)
            (if (error-exception? e)
              (error-exception-message e)
              e))
          (lambda ()
            (for-each
              ra::push-scheme-statement!
              (ns->scheme
                sexp-code
                (make-codegen-info full-path #t #!void)))))))
        (if (mspm-error? maybe-error)
          (error (mspm-error-path-set maybe-error full-path))
          (if (equal? maybe-error #!void)
            #!void
            (error maybe-error)))))))


(define (import-module! sexp ci)
  (print "import-module:" (cadadr sexp) "\n")
  (if (or (not (= (length sexp) 2))
          (not (list? (cadr sexp)))
          (not (equal? (caadr sexp) "quote"))
          (not (= (length (cadr sexp)) 2))
          (not (string? (cadadr sexp))))
    (crash 'invalid-import-format sexp)
    (let ()
      (define full-path (get-module-full-path (cadadr sexp) ci: ci))
      (read-and-parse-module-by-path! full-path)
      `(ra::ns-merge
         ,(ns-ref-from-ci ci)
         (ra::ns-current (ra::get-module ,full-path))
         mut: #t))))


(define (ns->scheme sexp ci)
  (print "ns->scheme..." "\n")
  (if (null? sexp)
    '()
    (let ((sexp (prepare-first-statement sexp)))
      (cons 
        (if (and (list? (car sexp))
                 (not (null? (car sexp))))
          (if (equal? "assign" (caar sexp))
            (var-setter ci (cadar sexp) (caddar sexp))
            (if (equal? "import" (caar sexp))
              (import-module! (car sexp) ci)
              (if (equal? "assert" (caar sexp))
                (let ((expr (val->scheme `("and" ,@(cdar sexp)) ci)))
                  `(if (not ,expr)
                     (error 'contract-violation (quote ,(cdar sexp)))))
                (val->scheme (car sexp) ci))))
          (val->scheme (car sexp) ci))
        (ns->scheme (cdr sexp) ci)))))


(define (qs->scheme sexp ci #!key unpack (cont identity))
  (define (next new-body unpack cont)
    (qs->scheme (list (car sexp) new-body) ci unpack: unpack cont: cont))

  (define (push value)
    (cont-cons (fn-if (not unpack) (wrap-into-list 'list) value) cont))

  (print "qs->scheme..." "\n")

  (if (not (or (quoted-list? sexp) (quoted-kv? sexp)))
    (crash 'not-a-quoted-structure sexp)
    (let ((body (cadr sexp)))
      (if (null? body)
        (if unpack
          (crash 'unexpected-rest-marker unpack)
          (if (quoted-list? sexp)
            `(apply append (quote ,(cont '())))
            `(make-ra::dictionary (quote ,(reverse (cont '()))))))
        (if (equal? "&" (car body))
          (if unpack
            (crash 'unexpected-rest-marker (car body))
            (next (cdr body) (car body) cont))
          (if (quoted-list? sexp)
            (next (cdr body) #f (push (val->scheme (car body) ci)))
            (let* ((=-chain-and-rest
                     (prefixize-head-=-chain body wrap-standalone: #t))
                   (=-chain (cdar =-chain-and-rest))
                   (rest (cdr =-chain-and-rest)))
              (if (> (length =-chain) 2)
                (crash '=-chain-too-long =-chain)
                (if (= (length =-chain) 1)
                  (if (not (string? (car =-chain)))
                    (crash 'not-an-unquoted-symbol (car =-chain))
                    (next rest #f (push (cons (car =-chain) (val->scheme (car =-chain))))))
                  (next rest #f
                    (push (cons (val->scheme (car =-chain)) (val->scheme (cadr =-chain))))))))))))))


(define (make-namespace ci inside #!key with-vars)
  (define vars (or with-vars 'ra::empty-dictionary))
  (define ns-obj
    `(make-ra::ns ,vars ,(ns-ref-from-ci ci) #!void))
  `(let ((namespace ,ns-obj)) ,@inside))


(define (val->scheme sexp ci)
  (define new-ci (codegen-info-assignment-to-set ci #f))
  (define (continue ls)
    (map (lambda (a) (val->scheme a new-ci)) ls))

  (display "val->scheme: ")
  (display (list sexp ci))
  (display "\n")

  (assert-is-value sexp)

  (if (quoted-structure? sexp)
    (qs->scheme sexp new-ci)
    (if (string? sexp)
      (if (or (string-prefix? "##" sexp) (string-prefix? "ra::" sexp))
        (string->symbol sexp)
        (var-getter ci sexp))
      (if (not (list? sexp))
        sexp
        (cond
          ((equal? (car sexp) "quote") (cadr sexp))

          ((equal? (car sexp) "if")
           (if (< (length (cdr sexp)) 2)
             (crash 'empty-if-branch sexp))
           (if (> (length (cdr sexp)) 3)
             (crash 'if-branch-too-many-args sexp))
           `(if ,@(continue (cdr sexp))))

          ((equal? (car sexp) "let")
           (if (null? (cdr sexp))
             (crash 'empty-let-block sexp))
           (make-namespace new-ci (ns->scheme (cdr sexp) (codegen-info-is-top-level-set new-ci #f))))

          ((member (car sexp) '("and" "or"))
           `(,(string->symbol (car sexp)) ,@(continue (cdr sexp))))

          ((equal? (car sexp) "fn")
           (if (< (length (cdr sexp)) 2)
             (crash 'empty-fn-definition sexp))
           (let* ((hb (if (and (list? (cadr sexp)) (not (quoted-structure? (cadr sexp))))
                        (cdr sexp)
                        (cons (but-last (cdr sexp)) (list (last sexp)))))
                  (decl (fn-arguments (car hb)))
                  (body (cdr hb))
                  (key-def-constructors
                    (map
                      (lambda (kv)
                        (cons (car kv)
                              `((lambda () ,(val->scheme (cadr kv) new-ci)))))
                      (append (cdr (assoc "#default" decl))
                              (cdr (assoc "#key" decl))))))
             `(let ()
                (define (function kwargs locals)
                  ,(make-namespace
                     ci
                     `((define kd (quote ,key-def-constructors))
                       (for-each
                         (lambda (k) (ra::ns-set-var namespace k ((cdr (assoc k kd))) mut: #t))
                         kwargs)
                       (define rec
                         (ra::callable-meta-ns-set
                           (ra::callable-called-set
                             (ra::wrap-callable-in-meta function (quote ,decl))
                             #t)
                           (ra::ns-current namespace)))
                       (ra::ns-set-var namespace "#rec" rec mut: #t)
                       ,@(if (codegen-info-assignment-to ci)
                           (list `(ra::ns-set-var namespace ,(codegen-info-assignment-to ci) rec mut: #t))
                           '())
                       ,@(ns->scheme body (codegen-info-is-top-level-set new-ci #f)))
                     with-vars: 'locals))
                (ra::wrap-callable-in-meta function (quote ,decl)))))

          (else
            (if (and (string? (car sexp))
                     (or (string-prefix? "##" (car sexp)) (string-prefix? "ra::" (car sexp))))
              `(,(string->symbol (car sexp))
                 ,@(map (lambda (arg) (val->scheme arg new-ci)) (cdr sexp)))
              (let* ((callable (val->scheme (car sexp) new-ci))
                     (args
                       (list 'quasiquote
                             ((map-over (parse-fn-call (cdr sexp)))
                              (lambda (v)
                                (append
                                  (but-last v)
                                  (list (list 'unquote (val->scheme (last v) new-ci)))))))))
                `(ra::call #!void ,callable ,args)))))))))


(define (ra-compile file-path)
  (ra::handle-crash
    (let* ((absolute-path
             (path-normalize
               (if (equal? (string-ref file-path 0) #\/)
                 file-path
                 (string-append (current-directory) file-path))))
           (path-to-scm-file (string-append absolute-path ".tmp.scm")))
      (read-and-parse-module-by-path! path-to-scm-file)
      (call-with-output-file
        absolute-path
        (lambda (port) (write (ra::scheme-code-cont '()) port)))
      (let ((gambit-output
        (with-input-from-process
          `(path: "gsc" arguments: ("-exe" ,path-to-scm-file))
          read-line)))
        (if (not (equal? gambit-output #!eof))
          (error gambit-output))))))


(define (main-fn)
  (let ((args (cdr (command-line))))
    (cond ((null? args) (error "no file to compile"))
          ((> (length args) 1) (error "one file expected for compilation"))
          ((< (string-length (car args)) 1) (error "empty file string"))
          (else (ra-compile (car args))))))


(main-fn)

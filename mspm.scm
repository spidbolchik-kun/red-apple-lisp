#!/usr/local/Gambit/bin/gsi-script

; module system and primitive macros

(include "utils.scm")
(include "dt.scm")
(include "nsmod.scm")
(include "parser.scm")
(include "runtime.scm")


(define statements '("define" "define-macro" "do" "assign" "import-from" "assert"))


(define primitive-forms
  (append statements '("fn" "if" "let" "quote" "kv-quote" "and" "or")))


(define forbidden-refs (append primitive-forms '("_" "&" "=")))


(define (sexp-refs sexp)
  (define (sexp-refs* sexp)
    (if (not (list? sexp))
      (if (and (string? sexp) (not (one-of? sexp forbidden-refs)))
        (list sexp)
        '())
      (if (or (null? sexp)
              (and (one-of? (car sexp) '("quote" "kv-quote"))
                   (not (null? (cdr sexp)))
                   (not (list? (cadr sexp)))))
        '()
        (apply append (map sexp-refs* sexp)))))
  (unique (sexp-refs* sexp)))


(define (get-macro-expansion sexp)
  ; MOCK
  (list 'list '(list 10 5 10 20) "macro-expansion-test"))


(define (sexp->code sexp me-info)
  (define res (get-code-slice sexp))
  (if (not res)
    `(list ,(get-macro-expansion sexp) "unknown" (quote (-1 -1 -1 -1)) ,(object->string sexp) (quote ()))
    `(list ,(get-macro-expansion sexp)
           ,(ast-obj-path res)
           (quote ,(code-ast-obj-lines-cols res))
           ,(code-ast-obj->string res)
           (quote ,(sexp-refs sexp)))))



;(define library-path (car (read-file-string-list "library-path")))


(define-structure module full-path ast-tree)


(define (crash err sexp)
  (error (make-mspm-error #f (list err sexp))))


(define (read-and-parse-module full-path #!key import-code)
  (let ((result
    (with-exception-catcher
      identity
      (lambda () (parse (read-file-string full-path) full-path)))))
    (if (no-such-file-or-directory-exception? result)
      (if (not import-code)
        (crash 'no-such-file full-path)
        (crash 'nonexistent-file-in-import-statement import-code))
      (if (parsing-error? result)
        (error
          (make-mspm-error
            full-path
            (list (parsing-error-type result) (parsing-error-marked result))))
        (if (and (pair? result) (null? (car result)))
          ;; Empty file - create a module with empty AST
          (make-module full-path (make-ast-obj 'top-level '() #f full-path))
          (make-module full-path result))))))


(define (assert-not-forbidden-ref sexp me-info)
  (if (member sexp forbidden-refs)
    (crash (if (equal? sexp "=") 'bad-assignment 'forbidden-reference)
           (sexp->code sexp me-info))
    sexp))


(define (assert-no-forbidden-refs-in-dest dest me-info)
  (for-each (lambda (v) (assert-not-forbidden-ref (car v) me-info)) dest)
  dest)


(define (wrap-into-ast-obj type data)
  (if (ast-obj? data) data (make-ast-obj type data #f)))


(define (ast-obj->sexp obj #!key parent backquoted)
  (define wrap
    (fn-if
      (not (one-of? (ast-obj-type obj) '(unquoted-symbol #\()))
      (wrap-into-list
        (if (equal? (ast-obj-type obj) #\{) "kv-quote" "quote"))))

  (define bq (or backquoted (equal? (ast-obj-type obj) "`(")))

  (let ((wrapped
    (wrap
      (let ((sexp
        (if (list? (ast-obj-data obj))
          (map (lambda (child)
                 (ast-obj->sexp child
                    parent: obj
                    backquoted: (equal? (ast-obj-type obj) "`(")))
               (filter (lambda (child) (not (equal? (ast-obj-type child) #\;)))
                       (ast-obj-data obj)))
          (ast-obj-data obj))
        ))
        (if parent (ast-obj-lvl-up-set! sexp parent))
        (add-sexp! sexp obj)
        sexp))))
    (add-sexp! wrapped obj)
    wrapped))


(define (prefixize-head-=-chain sexp-list me-info #!key wrap-standalone)
  (define (assignment? ident) (equal? ident "="))

  (if (assignment? (car sexp-list))
    (crash 'bad-assignment (sexp->code (car sexp-list) me-info))
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
            (cdddr sexp-list))
          me-info)))))


(define (prefixize-= sexp-list me-info #!key wrap-standalone)
  (if (null? sexp-list)
    '()
    (let ((res (prefixize-head-=-chain sexp-list me-info
                 wrap-standalone: wrap-standalone)))
      (cons (car res) (prefixize-= (cdr res) me-info wrap-standalone: wrap-standalone)))))


(define (bad-quotes-and-refs sexp me-info)
  (if (list? sexp)
    (if (null? sexp)
      '()
      (if (one-of? (car sexp) '("quote" "kv-quote"))
        (if (or (not (= (length sexp) 2))
                (and (equal? (car sexp) "kv-quote")
                     (not (list? (cadr sexp)))))
          (list sexp)
          (apply append (map (lambda (x) (bad-quotes-and-refs x me-info)) (cadr sexp))))
        (apply append (map (lambda (x) (bad-quotes-and-refs x me-info)) sexp))))
    (if (one-of? sexp primitive-forms)
      (list sexp)
      '())))


(define (empty-quotes sexp me-info)
  (if (or (not (list? sexp)) (null? sexp))
    '()
    (if (and (one-of? (car sexp) '("quote" "kv-quote"))
             (null? (cadr sexp)))
      (list sexp)
      (apply append (map (lambda (x) (empty-quotes x me-info)) sexp)))))


(define (deduplicate-quotes sexp me-info)
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
            (crash 'incompatible-double-quotation (sexp->code sexp me-info))
            (deduplicate-quotes (cadr sexp) me-info))
          (cons (car sexp) (map (lambda (x) (deduplicate-quotes x me-info)) (cadr sexp)))))
      (map (lambda (x) (deduplicate-quotes x me-info)) sexp))))


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
       (not (or (quoted-list? sexp)
                (quoted-kv? sexp)
                (quoted-string? sexp)))))


(define (get-duplicates-by fn ls)
  (filter (lambda (x) (> (length x) 1))
          (partition (equal-by fn) ls)))


(define (check-duplicate-vars var-ls sexp me-info)
  (let ((dups (get-duplicates-by car var-ls)))
    (if (null? dups)
      var-ls
      (crash 'duplicate-definitions (sexp->code sexp me-info)))))


(define (destructuring-identifiers sexp me-info)
  (define (inner sexp #!key acc-or-false rest)
    (define acc
      (or acc-or-false
        (cond ((quoted-list? sexp) 0)
              ((quoted-kv? sexp) '())
              (else (crash 'not-a-quoted-structure (sexp->code sexp me-info))))))
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
      (crash 'expected-rest-marker (sexp->code sexp me-info)))

    (if (null? body)
      '()
      (if (equal? (car body) "&")
        (if (null? (cdr body))
          (crash 'null-rest (sexp->code (car body) me-info))
          (let* ((=-chain-and-rest
                  (prefixize-head-=-chain (cdr body) me-info wrap-standalone: #t))
                 (=-chain (cdar =-chain-and-rest))
                 (rest (cdr =-chain-and-rest)))
            (cond ((and (> (length =-chain) 1) (not (string? (car =-chain))))
                   (crash 'not-an-unquoted-symbol (sexp->code (car =-chain) me-info)))
                  ((and (= (length =-chain) 1)
                        (not (null? rest)))
                   (crash 'wrong-rest-argcount (sexp->code (car body) me-info)))
                  ((> (length =-chain) 2)
                   (crash '=-chain-too-long (sexp->code (car body) me-info)))
                  ((and (= (length =-chain) 2)
                        (or (and (quoted-list? sexp)
                                 (not (quoted-list? (cadr =-chain))))
                            (and (quoted-kv? sexp)
                                 (not (quoted-kv? (cadr =-chain))))))
                   (crash 'wrong-destructuring-type (sexp->code (cadr =-chain) me-info)))
                  ((and (quoted-structure? (last =-chain))
                     (let ((marker
                             (find-one (equal-to "&") (cadr (last =-chain))
                               on-failure: #f)))
                       (if marker
                         (if (and (not (null? rest)) (not (null? (cdr rest))))
                           (crash 'multiple-rest-patterns (sexp->code marker me-info))
                           #f)
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
                          (make-level 'kv-diff (car =-chain) acc)
                          (make-level 'kv-pick (car =-chain) getter-ls)))
                      next-level)
                    (inner (list (car sexp) rest)
                      acc-or-false: next-acc
                      rest: #t)))))))
        (let* ((=-chain-and-rest
                 (prefixize-head-=-chain body me-info wrap-standalone: #t))
               (=-chain (cdar =-chain-and-rest))
               (rest (cdr =-chain-and-rest)))
          (cond
            ((null? (length =-chain))
             (crash '=-chain-null-length (sexp->code (car body) me-info)))
            ((and (quoted-list? sexp) (> (length =-chain) 2))
             (crash '=-chain-too-long (sexp->code (list-ref =-chain 2) me-info)))
            ((> (length =-chain) 3)
             (crash '=-chain-too-long (sexp->code (list-ref =-chain 3) me-info)))
            (else
              (let*
                ((result
                   (if (quoted-structure? (last =-chain))
                     (cons (but-last =-chain) (inner (last =-chain)))
                     (if (unquoted-list? (last =-chain))
                       (crash 'unexpected-unquoted-list (sexp->code (last =-chain) me-info))
                       (cons =-chain '()))))
                 (=-chain* (car result))
                 (next-level (cdr result)))
                (let
                  ((lvl-args
                     (case (length =-chain*)
                       ((0) (if (quoted-kv? sexp)
                              (crash 'no-key (sexp->code =-chain* me-info))
                              (list #f acc)))
                       ((1) (if (and (not (string? (car =-chain*)))
                                     (quoted-list? sexp))
                              (crash 'not-an-unquoted-symbol (sexp->code (car =-chain*) me-info))
                              (list (if (quoted-string? (car =-chain*)) #f (car =-chain*))
                                    (if (quoted-list? sexp)
                                      acc
                                      (if (quoted-string? (car =-chain*))
                                        (cadr (car =-chain*))
                                        (car =-chain*))))))
                       ((2) (if (not (string? (car =-chain*)))
                              (crash 'not-an-unquoted-symbol (sexp->code (car =-chain*) me-info))
                              (list (car =-chain*) (cadr (cadr =-chain*))))))))
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
        (inner sexp))
      me-info)
    sexp
    me-info))


(define (fn-arguments args me-info)
  (define (group->code-segment group)
    (sexp->code (car (member group args)) me-info))

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
        (crash 'wrong-arg-kind-order (group->code-segment (caar parsed-groups)))
        (cons (car parsed-groups)
              (check-order (cdr parsed-groups)
                possible-kinds:
                  (new-possible-kinds (caar parsed-groups) possible-kinds))))))

  (define (assignment-length-checker wrong-len-pred)
    (lambda (assignment)
      (if (wrong-len-pred assignment)
        (crash 'wrong-assignment-chain-length (sexp->code (car assignment) me-info))
        (if (not (string? (car assignment)))
          (crash 'not-an-unquoted-symbol (sexp->code (car assignment) me-info))
          (if (= (length assignment) 1)
            (append assignment (list #!void))
            assignment)))))

  (define check-default
    (assignment-length-checker (lambda (a) (not (= (length a) 2)))))

  (define check-key
    (assignment-length-checker (lambda (a) (> (length a) 2))))

  (define (prefixize-=-unwrap group)
    (map cdr (prefixize-= group me-info wrap-standalone: #t)))

  (define (check-either arg)
    (if (not (unquoted-list? arg))
      (crash 'not-an-unquoted-list (sexp->code arg me-info))
      ((map-over arg)
       (map-with
         (lambda (var)
           (if (not (string? var))
             (crash 'not-an-unquoted-symbol (sexp->code var me-info))
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
      ((positional) (destructuring-identifiers (list "quote" (cdr group)) me-info))
      ((either) (check-either (cdr group)))
      ((key) (map (lambda (k) (check-key k)) (prefixize-=-unwrap (cdr group))))
      ((key-rest) (destructuring-identifiers (list "kv-quote" (cdr group)) me-info))
      ((default) (map check-default (prefixize-=-unwrap (cdr group))))))


  (define (one-of-kinds? obj) (one-of? obj kinds))

  (let ((dups (get-duplicates-by
                identity
                (filter one-of-kinds? args))))
    (if (not (null? dups))
      (crash 'duplicate-argument-markers (sexp->code (caar dups) me-info))
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
            (crash 'duplicate-argument-variables (sexp->code (caar dups) me-info))
            groups))))))


(define (parse-fn-call sexp me-info)
  (define (keyword? obj)
    (and (string? obj) (string-suffix? ":" obj)))

  (define (parse-pairs sexp)
    (if (null? sexp)
      '()
      (cond
        ((equal? (car sexp) "&")
         (cond ((null? (cdr sexp))
                (crash 'unexpected-rest-marker (sexp->code (car sexp) me-info)))
               ((equal? "&" (cadr sexp))
                (crash 'unexpected-rest-marker (sexp->code (cadr sexp) me-info)))
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
        (crash 'positional-after-key (sexp->code (cadar args) me-info))
        (cons (car args)
              (check-order (cdr args)
                can-be-positional:
                  (and can-be-positional
                       (or (equal? (caar args) 'positional)
                           (and (equal? (caar args) "&")
                                (not (quoted-kv? (cadar args)))))))))))

  (check-order (parse-pairs sexp)))


(define modules-code '())


(define (assert-is-value sexp me-info)
  (assert-not-forbidden-ref sexp me-info)
  (if (null? sexp)
    (crash 'empty-call (sexp->code sexp me-info))
    (if (and (list? sexp)
             (member (car sexp) statements)
             (not (equal? (car sexp) "do")))
      (crash 'statement-instead-of-value (sexp->code sexp me-info))
      sexp)))


(define (check-ns-assignment sexp me-info)
  (define args (cdr sexp))

  (if (< (length args) 2)
    (crash 'empty-assignment (sexp->code sexp me-info))
    (if (> (length args) 3)
      (crash 'assignment-too-many-args (sexp->code (list-ref args 3) me-info))
      (let ((args2 (but-last args)))
        (assert-is-value (last args) me-info)
        (if (string? (car args2))
          (assert-not-forbidden-ref (car args2) me-info))
        (case (length args2)
          ((1) (if (and (not (quoted-structure? (car args2)))
                        (not (string? (car args2))))
                 (crash 'not-a-variable-name-or-destructuring
                        (sexp->code (car args2) me-info))
                 sexp))
          ((2) (if (not (string? (car args2)))
                 (crash 'not-a-variable-name (sexp->code (car args2) me-info))
                 (if (not (quoted-structure? (last args2)))
                   (crash 'not-a-destructuring (sexp->code (last args2) me-info))
                   sexp))))))))


(define (prepare-definition sexp me-info)
  (define args (cdr sexp))

  (if (< (length args) 2)
    (crash 'empty-definition (sexp->code (car sexp) me-info))
    (if (list? (car args))
      (if (quoted-structure? (car args))
        (crash 'quoted-structure-in-definition (sexp->code (car args) me-info))
        (if (null? (car args))
          (crash 'empty-fn-definition (sexp->code (car args) me-info))
          (list "assign" (caar args)
            `("fn" ,(cdar args) ,@(cdr args)))))
      (if (> (length args) 3)
        (crash 'definition-too-many-args (sexp->code (list-ref args 3) me-info))
        (cons "assign" args)))))


(define (prepare-first-statement sexp me-info)
  (if (null? sexp)
    '()
    (let ((sexp (prefixize-head-=-chain sexp me-info)))
      (if (nonempty-list? (car sexp))
        (if (equal? (caar sexp) "define")
          (prepare-first-statement
            (cons (prepare-definition (car sexp) me-info) (cdr sexp))
            me-info)
          (if (equal? (caar sexp) "assign")
            (let ()
              (check-ns-assignment (car sexp) me-info)
              (if (= (length (car sexp)) 4)
                `(,(list "assign" (cadr (car sexp)) (last (car sexp)))
                  ,(list "assign" (caddr (car sexp)) (cadr (car sexp)))
                  ,@(cdr sexp))
                sexp))
            sexp))
        (cons (assert-is-value (car sexp) me-info) (cdr sexp))))))


(define-structure codegen-info ns assignment-to macro-expansion-code)

(define (set-macro-expansion-code ci code)
  (if (equal? #!void (codegen-info-macro-expansion-code ci))
    (codegen-info-macro-expansion-code-set ci code)))

(define (codegen-info-is-top-level ci)
  (string? (me-ns-parent-or-modpath (codegen-info-ns ci))))

(define (codegen-info-path ci)
  (me-ns-module-path (codegen-info-ns ci)))

(define (codegen-info-branch-out! ci)
  (make-codegen-info (me-ns-branch-out! (codegen-info-ns ci))
                     (codegen-info-assignment-to ci)
                     (codegen-info-macro-expansion-code ci)))

(define (codegen-info-set-var! ci var value)
  (let ((ns (codegen-info-ns ci)))
    (with-exception-catcher
      (lambda (e)
        (if (and (error-exception? e)
                 (equal? (error-exception-message e) 'duplicate-definitions))
          (crash 'duplicate-definitions
                 (sexp->code var (codegen-info-macro-expansion-code ci)))
          (raise e)))
      (lambda () (me-ns-set-var! ns var value)))))


(define (var-setter ci lval rval)
  (define (get-variable-sym-with-pref! varname)
    (get-variable-symbol! varname (codegen-info-path ci)))

  (if (not (quoted-structure? lval))
    (let ((ci (codegen-info-assignment-to-set ci lval)))
      (define scm-val (val->scheme rval ci))
      (define nspref-lval (ra::set-ns (ra::erase-ns lval) (codegen-info-ns ci)))
      (define ci-rval
        (if (procedure-sexp? rval)
          rval
          (eval-sc-sexp-to-tmpvar (with-dynamic-ns (codegen-info-ns ci) scm-val))))
      (codegen-info-set-var! ci lval ci-rval)
      (if (not (codegen-info-is-top-level ci))
        (codegen-info-set-var! ci nspref-lval ci-rval))
      `(begin
         (define ,(get-variable-sym-with-pref! lval) ,scm-val)
         (define ,(get-variable-sym-with-pref! nspref-lval)
                 ,(get-variable-sym-with-pref! lval))))
    (let ((getters (destructuring-identifiers lval (codegen-info-macro-expansion-code ci)))
          (tmp-value-sym (gensym!))
          (getters-sym (gensym!)))
      (define scm-val (val->scheme rval ci))
      `(begin
         (define ,tmp-value-sym ,scm-val)
         (define ,getters-sym (ra::assignment->ns (quote ,getters) ,tmp-value-sym))
         ,@((map-over getters)
            (lambda (kv)
              (define getter-exp
                `(cdr (assoc ,(car kv) (ra::ns-ref (lambda () ,getters-sym) #!void))))
              (define nspref-lval (ra::set-ns (ra::erase-ns (car kv)) (codegen-info-ns ci)))
              (define ci-rval (eval-sc-sexp-to-tmpvar getter-exp))
              (codegen-info-set-var! ci (car kv) ci-rval)
              (if (not (codegen-info-is-top-level ci))
                (codegen-info-set-var! ci nspref-lval ci-rval))
              `(begin
                 (define ,(get-variable-sym-with-pref! (car kv)) ,getter-exp)
                 (define ,(get-variable-sym-with-pref! nspref-lval)
                         ,(get-variable-sym-with-pref! (car kv))))))))))


(define (var-getter ci val)
  (define syms (get-variable-symbols-chain! val (codegen-info-path ci)))
  `(ra::ns-ref
     (list ,@(map (lambda (s) `(lambda () ,s)) syms))
     ,(sexp->code val (codegen-info-macro-expansion-code ci))))


(define last-evaled #!void)


(define (eval-sc-sexp-to-tmpvar sexp)
  (set! last-evaled sexp)
  (let ((var (gensym!)))
    (with-exception-catcher
      (lambda (e)
        (if (and (error-exception? e)
                 (equal? (error-exception-message e)
                         'attempted-to-use-unbound-values-in-macro))
          (eval `(define ,var ra::me-hole))
          (if (and (error-exception? e)
                   (equal? (error-exception-message e)
                           'unbound-variable))
            (eval `(define ,var ra::unbound))
            (raise e))))
      (lambda () (eval `(define ,var ,sexp))))
    var))


(define (get-module-full-path path #!key ci)
  (path-normalize
    (if (string-prefix? "/" path)
      path
      (if (string-prefix? "./" path)
        (string-append
          (dir-from-path (if (not ci) (current-directory) (codegen-info-path ci)))
          (substring path 2 (string-length path)))
        (string-append library-path path)))))


(define (read-and-parse-module-by-path! full-path #!key import-code)
  (if (equal? #!void (find-one (equal-to full-path) modules-code))
    (let* ((parsed (read-and-parse-module full-path import-code: import-code))
           (sexp-code
             (fn-if (not (equal? full-path (get-module-full-path "builtins.ra")))
                    (lambda (code) (cons '("import-from" ("quote" "builtins.ra")) code))
                    (cadr (ast-obj->sexp (module-ast-tree parsed))))))
      (set! modules-code (cons full-path modules-code))
      (let ((maybe-error
        (with-exception-catcher
          (lambda (e)
            (if (error-exception? e)
              (error-exception-message e)
              e))
          (lambda ()
            (ns->scheme sexp-code (make-codegen-info (get-module-ns! full-path) #f #!void))
            #!void))))
        (if (mspm-error? maybe-error)
          (error (mspm-error-path-set maybe-error full-path))
          (if (equal? maybe-error #!void)
            #!void
            (error maybe-error)))))))


(define (import-module! sexp ci)
  (define (get-variable-sym-with-pref! varname)
    (get-variable-symbol! varname (codegen-info-path ci)))

  (define (get-pref sexp* #!key (cont identity))
    (if (null? sexp*)
      (cons "" (cont '()))
      (if (equal? (car sexp*) "with-prefix:")
        (if (null? (cdr sexp*))
          (crash 'unspecified-import-prefix
                 (sexp->code sexp (codegen-info-macro-expansion-code ci)))
          (if (not (string? (cadr sexp*)))
            (crash 'prefix-is-not-a-symbol
                   (sexp->code sexp (codegen-info-macro-expansion-code ci)))
            (cons (cadr sexp*) (cont (cddr sexp*)))))
        (get-pref (cdr sexp*)
          cont: (lambda (acc) (cont (cons (car sexp*) acc)))))))

  (define pref/imp-st (get-pref sexp))
  (define pref (car pref/imp-st))
  (define imp-st (cdr pref/imp-st))
  (define sym-dest #!void)
  (if (> (length imp-st) 2)
    (begin
      (set! sym-dest (last imp-st))
      (set! imp-st (but-last imp-st))))

  (if (or (not (= (length imp-st) 2))
          (not (list? (cadr imp-st)))
          (not (equal? (caadr imp-st) "quote"))
          (not (= (length (cadr imp-st)) 2))
          (not (string? (cadadr imp-st))))
    (crash 'invalid-import-format (sexp->code sexp (codegen-info-macro-expansion-code ci)))
    (let ()
      (define full-path (get-module-full-path (cadadr imp-st) ci: ci))
      (read-and-parse-module-by-path! full-path
        import-code: (sexp->code (cadadr sexp) (codegen-info-macro-expansion-code ci)))
      (if (equal? sym-dest #!void)
        (let ()
          (define import-statement (cons 'begin #!void))
          (define (run-import-statement)
            (set-cdr! import-statement
               ((map-over (get-module-varnames full-path))
                (lambda (varname)
                  `(define
                     ,(get-variable-symbol!
                        (string-append pref varname)
                        (codegen-info-path ci))
                     ,(get-variable-symbol! varname full-path))))))
          (for-each
            (lambda (varname)
              (codegen-info-set-var!
                ci
                (string-append pref varname)
                (me-ns-ref (get-module-ns! full-path) varname)))
            (get-module-varnames full-path))
          (add-deferred-import-statement! run-import-statement)
          import-statement)
        (if (not (quoted-kv? sym-dest))
          (crash 'expected-a-dictionary
                 (sexp->code sym-dest (codegen-info-macro-expansion-code ci)))
          (let* ((dest
                   ((map-over (destructuring-identifiers sym-dest (codegen-info-macro-expansion-code ci)))
                    (lambda (v) (cons (string-append pref (car v)) (cdr v)))))
                 (imported-tl (unique (map cdadr dest)))
                 (dct-sym (gensym!))
                 (getters-sym (gensym!)))
            (add-deferred-import-statement!
              (lambda ()
                (for-each
                  (lambda (v)
                    (if (not (member v (get-module-varnames full-path)))
                      (crash 'unbound-variable
                             (sexp->code v (codegen-info-macro-expansion-code ci)))))
                  imported-tl)))
            `(begin
               (define ,dct-sym
                 (make-ra::dictionary
                   (list
                     ,@(map (lambda (k)
                              `(cons ,k ,(get-variable-symbol! k full-path)))
                            imported-tl))))
               (define ,getters-sym (ra::assignment->ns (quote ,dest) ,dct-sym))
               ,@((map-over dest)
                  (lambda (kv)
                    (define getter-exp
                      `(cdr (assoc ,(car kv) (ra::ns-ref (lambda () ,getters-sym) #!void))))
                    (pp getter-exp)
                    (codegen-info-set-var! ci (car kv) (eval-sc-sexp-to-tmpvar getter-exp))
                    `(define ,(get-variable-sym-with-pref! (car kv))
                             ,getter-exp))))))))))


(define (ns->scheme sexp ci)
  (define (val-from-car sexp)
    (let ((val (val->scheme (car sexp) ci)))
      (if (equal? (length sexp) 1)
        val
        `(define ,(gensym!) ,val))))

  (if (null? sexp)
    '()
    (call/cc
      (lambda (return)
        (let ((sexp (prepare-first-statement sexp (codegen-info-macro-expansion-code ci))))
          (define fst-exp
            (if (and (list? (car sexp))
                     (not (null? (car sexp))))
              (cond ((equal? "assign" (caar sexp))
                     (var-setter ci (cadar sexp) (caddar sexp)))

                    ((macro? (me-ns-ref (codegen-info-ns ci) (caar sexp) #f))
                     (return
                       (ns->scheme
                         (cons
                           (expand-macro
                             (sexp->code (car sexp) #!void)
                             (eval (eval-sc-sexp-to-tmpvar
                               (procedure-with-dynamic-ns (me-ns-ref (codegen-info-ns ci) (caar sexp)))))
                             (cdar sexp))
                           (cdr sexp))
                         (set-macro-expansion-code ci (sexp->code (car sexp) #!void))
                         )))

                    ((equal? "define-macro" (caar sexp))
                     (let ((def (cdar sexp)))
                       (define sink0
                         (if (< (length def) 3)
                           (crash 'invalid-macro-definition-format
                                  (sexp->code def (codegen-info-macro-expansion-code ci)))))
                       (define name (car def))
                       (define arg (cadr def))
                       (define body (cddr def))
                       (define sink1
                         (if (not (string? name))
                           (crash 'macro-name-is-not-an-unquoted-symbol
                                  (sexp->code name (codegen-info-macro-expansion-code ci)))))

                       (define fn-sexp
                         (val->scheme `("fn" (,arg) ,@body)
                                      (codegen-info-assignment-to-set ci name)))
                       (declare-procedure-a-macro! fn-sexp)
                       (codegen-info-set-var! ci name fn-sexp)))

                    ((equal? "import-from" (caar sexp))
                     (import-module! (car sexp) ci))
                    ((equal? "assert" (caar sexp))
                     (let ((expr (val->scheme `("and" ,@(map (lambda (v) v) (cdar sexp))) ci)))
                       `(define ,(gensym!)
                          (if (not ,expr)
                            (error 'contract-violation
                                   ,(sexp->code (car sexp) (codegen-info-macro-expansion-code ci)))))))
                    ((equal? "do" (caar sexp))
                     (return (ns->scheme (append (cdar sexp) (cdr sexp)) ci)))
                    (else (val-from-car sexp)))
              (val-from-car sexp)))
          (if (codegen-info-is-top-level ci)
            (ra::push-scheme-statement! fst-exp))
          (cons fst-exp (ns->scheme (cdr sexp) ci)))))))


(define (list->scheme sexp ci #!key unpack (cont identity))
  (define (next new-body unpack cont)
    (list->scheme (list (car sexp) new-body) ci unpack: unpack cont: cont))

  (define (push value)
    (cont-cons
      (list 'unquote (fn-if (not unpack) (wrap-into-list 'list) value))
      cont))

  (if (not (quoted-list? sexp))
    (crash 'not-a-quoted-list
           (sexp->code sexp (codegen-info-macro-expansion-code ci)))
    (let ((body (cadr sexp)))
      (if (null? body)
        (if unpack
          (crash 'unexpected-rest-marker
                 (sexp->code unpack (codegen-info-macro-expansion-code ci)))
          (list 'apply 'append (list 'quasiquote (cont '()))))
        (if (equal? "&" (car body))
          (if unpack
            (crash 'unexpected-rest-marker
                   (sexp->code (car body) (codegen-info-macro-expansion-code ci)))
            (next (cdr body) (car body) cont))
          (next (cdr body) #f (push (val->scheme (car body) ci))))))))


(define (kv->scheme sexp ci #!key unpack (acc '()))
  (define (next new-body unpack acc)
    (kv->scheme (list (car sexp) new-body) ci unpack: unpack acc: acc))

  (define (push value)
    (cons
      (if unpack
        (list 'ra::dictionary-alist (cdr value))
        (list 'list (list 'cons (car value) (cdr value))))
      acc))

  (if (not (quoted-kv? sexp))
    (crash 'not-a-quoted-kv
           (sexp->code sexp (codegen-info-macro-expansion-code ci)))
    (let ((body (cadr sexp)))
      (if (null? body)
        (if unpack
          (crash 'unexpected-rest-marker
                 (sexp->code unpack (codegen-info-macro-expansion-code ci)))
          (list 'make-ra::dictionary
            (cons 'append acc)))
        (if (equal? "&" (car body))
          (if unpack
            (crash 'unexpected-rest-marker
                   (sexp->code (car body) (codegen-info-macro-expansion-code ci)))
            (next (cdr body) (car body) acc))
          (let* ((=-chain-and-rest
                   (prefixize-head-=-chain body (codegen-info-macro-expansion-code ci) wrap-standalone: #t))
                 (=-chain (cdar =-chain-and-rest))
                 (rest (cdr =-chain-and-rest)))
            (if (> (length =-chain) 2)
              (crash '=-chain-too-long
                     (sexp->code (list-ref =-chain 2) (codegen-info-macro-expansion-code ci)))
              (if (= (length =-chain) 1)
                (if (not (string? (car =-chain)))
                  (crash 'not-an-unquoted-symbol
                         (sexp->code (car =-chain) (codegen-info-macro-expansion-code ci)))
                  (next rest #f (push (cons (car =-chain) (val->scheme (car =-chain) ci)))))
                (next rest #f
                  (push (cons (val->scheme (car =-chain) ci) (val->scheme (cadr =-chain) ci))))))))))))


(define (qs->scheme sexp ci)
  (cond ((quoted-list? sexp) (list->scheme sexp ci))
        ((quoted-kv? sexp) (kv->scheme sexp ci))
        (else (crash 'not-a-quoted-structure (sexp->code sexp (codegen-info-macro-expansion-code ci))))))


(define (expand-macro code proc sexp)
  (ra::call code proc (list (list 'positional sexp))))


(define (val->scheme sexp ci)
  (define new-ci (codegen-info-assignment-to-set ci #f))
  (define (continue ls)
    (map (lambda (a) (val->scheme a new-ci)) ls))

  (assert-is-value sexp (codegen-info-macro-expansion-code ci))

  (if (quoted-structure? sexp)
    (qs->scheme sexp new-ci)
    (if (string? sexp)
      (if (or (string-prefix? "##" sexp)
              (string-prefix? "ra::" sexp)
              (string-prefix? "make-ra::" sexp))
        (string->symbol sexp)
        (var-getter ci sexp))
      (if (not (list? sexp))
        sexp
        (cond
          ((equal? (car sexp) "quote") (cadr sexp))

          ((macro? (me-ns-ref (codegen-info-ns ci) (car sexp) #f))
           (val->scheme
             (expand-macro
               (sexp->code sexp #!void)
               (eval (eval-sc-sexp-to-tmpvar
                 (procedure-with-dynamic-ns (me-ns-ref (codegen-info-ns ci) (car sexp)))))
               (cdr sexp))
             (set-macro-expansion-code ci (sexp->code sexp #!void))))

          ((equal? (car sexp) "do")
           (val->scheme `("let" ,@(cdr sexp)) ci))

          ((equal? (car sexp) "if")
           (if (< (length (cdr sexp)) 2)
             (crash 'empty-if-branch
                    (sexp->code (car sexp) (codegen-info-macro-expansion-code ci))))
           (if (> (length (cdr sexp)) 3)
             (crash 'if-branch-too-many-args
                    (sexp->code sexp (codegen-info-macro-expansion-code ci))))
           `(if ,@(continue (map (lambda (b) `("let" ,b)) (cdr sexp)))))

          ((equal? (car sexp) "let")
           (if (null? (cdr sexp))
             (crash 'empty-let-block
                    (sexp->code sexp (codegen-info-macro-expansion-code ci)))
           `(let ()
              ,@(ns->scheme (cdr sexp) (codegen-info-branch-out! new-ci)))))

          ((member (car sexp) '("and" "or"))
           `(,(string->symbol (car sexp))
              ,@(continue (map (lambda (b) `("let" ,b)) (cdr sexp)))))

          ((equal? (car sexp) "fn")
           (if (< (length (cdr sexp)) 2)
             (crash 'empty-fn-definition
                    (sexp->code (car sexp) (codegen-info-macro-expansion-code ci))))

           (let ((new-ci (codegen-info-branch-out! new-ci)))
             (define head-and-body
               (if (and (list? (cadr sexp)) (not (quoted-structure? (cadr sexp))))
                 (cdr sexp)
                 (cons (but-last (cdr sexp)) (list (last sexp)))))

             (define decl (fn-arguments (car head-and-body) (codegen-info-macro-expansion-code ci)))

             (define ordered-args
               (list-sort string<=? 
                 (append
                   (apply append (cdr (assoc "#either" decl)))
                   (map car
                     (apply append
                       (map cdr
                         (filter (lambda (g) (not (equal? (car g) "#either")))
                                 decl)))))))

             (define ordered-args-symbols
               (map (lambda (v) (get-variable-symbol! v (codegen-info-path ci)))
                    ordered-args))

             (define body (cdr head-and-body))

             (define ns-with-default
               (list 'make-ra::dictionary
                 (cons 'list
                   (map (lambda (kv) `(cons ,(car kv) ,(val->scheme (cadr kv) new-ci)))
                        (append (cdr (assoc "#default" decl)) (cdr (assoc "#key" decl)))))))

             (define ns-inside-called
               (list 'make-ra::dictionary
                 (cons 'list
                   (map (lambda (str sym) `(cons ,str ,sym))
                        ordered-args
                        ordered-args-symbols))))

             (define name
               (if (codegen-info-assignment-to ci) (codegen-info-assignment-to ci) #!void))

             (define (get-variable-sym-with-pref! varname)
               (get-variable-symbol! varname (codegen-info-path ci)))
             
             (define sink
               (let ()
                 (codegen-info-set-var! new-ci "#rec" 'ra::me-hole)
                 (codegen-info-set-var! new-ci "#clear-args:rec" 'ra::me-hole)
                 (for-each
                   (lambda (v) (codegen-info-set-var! new-ci v 'ra::me-hole))
                   ordered-args)))

             (define rec-sym (get-variable-symbol! "#rec" (codegen-info-path ci)))
             (define cn-rec-sym (get-variable-symbol! "#clear-args:rec" (codegen-info-path ci)))

             (define generated-code 
               `(let ()
                  (define wrapped-function #f)
                  (define (function ,@ordered-args-symbols)
                    (define ,rec-sym
                      (ra::wrap-callable-in-meta
                        function
                        (ra::callable-meta-called-set
                          (ra::init-callable-meta ,ns-inside-called (quote ,decl) ,name)
                          #t)))
                    (define ,cn-rec-sym wrapped-function)
                    ,@(if (equal? name #!void)
                        '()
                        (let ((name-sym (get-variable-sym-with-pref! name))
                              (cn-name-sym (get-variable-sym-with-pref!
                                             (string-append "clear-args:" name))))
                          (codegen-info-set-var! new-ci name 'ra::me-hole)
                          (codegen-info-set-var! new-ci (string-append "clear-args:" name) 'ra::me-hole)
                          `((define ,name-sym ,rec-sym)
                            (define ,cn-name-sym ,cn-rec-sym))))
                    (let () ,@(ns->scheme body (codegen-info-branch-out! new-ci))))
                  (set! wrapped-function
                    (ra::wrap-callable-in-meta
                       function
                       (ra::init-callable-meta ,ns-with-default (quote ,decl) ,name)))
                  wrapped-function))

             (add-procedure-ns! generated-code (codegen-info-ns ci))
             generated-code
             )
           )

          (else
            (if (and (string? (car sexp))
                     (or (string-prefix? "##" (car sexp))
                         (string-prefix? "ra::" (car sexp))
                         (string-prefix? "make-ra::" (car sexp))))
              `(,(string->symbol (car sexp))
                 ,@(map (lambda (arg) (val->scheme arg new-ci)) (cdr sexp)))
              (let* ((callable (val->scheme (car sexp) new-ci))
                     (args
                       (list 'quasiquote
                             ((map-over (parse-fn-call (cdr sexp) (codegen-info-macro-expansion-code ci)))
                              (lambda (v)
                                (append
                                  (but-last v)
                                  (list (list 'unquote (val->scheme (last v) new-ci)))))))))
                `(ra::call ,(sexp->code sexp (codegen-info-macro-expansion-code ci)) ,callable ,args)))))))))



(define (get-absolute-path file-path)
  (path-normalize
    (if (equal? (string-ref file-path 0) #\/)
      file-path
      (string-append (current-directory) file-path))))


(define (get-scheme-code)
  (ra::handle-crash-fn
    (lambda () (ra::scheme-code-cont '()))))


(define (ra-compile file-path)
  (ra::handle-crash
    (let* ((absolute-path (get-absolute-path file-path))
           (path-to-scm-file (string-append absolute-path ".tmp.scm")))
      (read-and-parse-module-by-path! absolute-path)
      (run-deferred-import-statements!)
      (call-with-output-file
        path-to-scm-file
        (let ((scheme-code
                (list `(include ,(get-module-full-path "runtime.scm"))
                      (append (enumerate-variables-set-unbound)
                              (get-scheme-code)))))
          (lambda (port)
            (for-each (lambda (sexp) (pp sexp port)) scheme-code))))
      (let ((gambit-output
        (with-input-from-process
          `(path: "gsc" arguments: ("-exe" ,path-to-scm-file))
          read-line)))
        (if (not (equal? gambit-output #!eof))
          (error gambit-output))))))


(define code-to-eval #!void)


(define (ra-transpile-and-run file-path)
  (ra::handle-crash
    (let* ((absolute-path (get-absolute-path file-path))
           (path-to-scm-file (string-append absolute-path ".tmp.scm")))
      (read-and-parse-module-by-path! absolute-path)
      (run-deferred-import-statements!)
      (let ((scheme-code
              (cons 'begin
                 (cons (enumerate-variables-set-unbound)
                       (get-scheme-code)))))
        (set! code-to-eval scheme-code)
        (eval code-to-eval)))))

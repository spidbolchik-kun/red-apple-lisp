(include "dt.scm")


; Used for conversion between unquoted strings in s-expressions and Scheme symbols.
(define *variable-counter* 0)
(define *sym->num* (make-table))


(define sym-ns-map (make-table test: eq?))
(define injected-map (make-table test: eq?))


(define (add-v-prefix num)
  (string->symbol
    (string-append
      "v"
      (number->string num))))


(define (enumerate-variables-set-unbound)
  (cons 'begin
    (map (lambda (v) `(define ,v ra::unbound))
         (map add-v-prefix (iota *variable-counter*)))))


(define (gensym!)
  (let ((old-counter *variable-counter*))
    (define vsym (add-v-prefix old-counter))
    (set! *variable-counter* (+ 1 *variable-counter*))
    (eval `(define ,vsym ra::unbound))
    vsym))


(define (get-variable-symbol! name mod-path)
  (define ns
    (let ((maybe-ns (table-ref sym-ns-map name #f)))
      (if (not maybe-ns)
        (get-module-ns! mod-path)
        maybe-ns)))
  (let* ((pair (list name (me-ns-num ns)))
         (num (table-ref *sym->num* pair #f))
         (old-counter *variable-counter*))
    (define vsym
      (add-v-prefix
        (if num num
          (begin
            (table-set! *sym->num* pair *variable-counter*)
            (set! *variable-counter* (+ 1 *variable-counter*))
            old-counter))))
    (if (not num)
      (eval `(define ,vsym ra::unbound)))
    vsym))


(define (get-variable-symbols-chain! name mod-path)
  (define ns
    (let ((maybe-ns (table-ref sym-ns-map name #f)))
      (if (not maybe-ns)
        (get-module-ns! mod-path)
        maybe-ns)))
  (let loop ((ns ns))
    (if (not ns)
      '()
      (cons (get-variable-symbol!
              (ra::set-ns (ra::erase-ns name) ns)
              mod-path)
            (loop (if (string? (me-ns-parent-or-modpath ns))
                    #f
                    (me-ns-parent-or-modpath ns)))))))


(define ast-objects-lvl-up '())


(define (ast-obj-lvl-up-set! sexp parent)
  (set! ast-objects-lvl-up (cons (cons sexp parent) ast-objects-lvl-up)))


(define (get-parent-ast-obj sexp)
  (cdr (assq sexp ast-objects-lvl-up)))


(define sexp-ast-obj-pairs (make-table test: eq?))


(define (add-sexp! sexp code-slice)
  (table-set! sexp-ast-obj-pairs sexp code-slice))


(define (get-code-slice sexp)
  (table-ref sexp-ast-obj-pairs sexp #f))


(define (ra::inject sym)
  (if (table-ref sym-ns-map sym #f)
    sym
    (let ((cpy (string-copy sym)))
      (add-sexp! cpy (get-code-slice sym))
      (table-set! injected-map cpy #t)
      cpy)))


(define (ra::set-ns sym ns)
  (if (table-ref sym-ns-map sym #f)
    sym
    (let ((cpy (string-copy sym)))
      (add-sexp! cpy (get-code-slice sym))
      (table-set! sym-ns-map cpy ns)
      cpy)))


(define (ra::erase-ns sym)
  (let ((cpy (string-copy sym)))
    (add-sexp! cpy (get-code-slice sym))
    cpy))


(define *ns-counter* 0)


(define-structure me-ns num dict parent-or-modpath children)


(define (me-ns-module-path ns)
  (let ((res (me-ns-parent-or-modpath ns)))
    (if (string? res)
      res
      (me-ns-module-path res))))


(define modules (make-table))


(define (syms-equal? x y)
  (define (ns-num-or-false sym)
    (let ((res (table-ref sym-ns-map sym #f)))
      (if (not res) res (me-ns-num res))))
  (equal? (cons x (ns-num-or-false x))
          (cons y (ns-num-or-false y))))


(define (get-module-ns! normalized-path)
  (let ((res (table-ref modules normalized-path #f)))
    (if res res
      (begin
        (table-set! modules normalized-path
          (make-me-ns *ns-counter* (make-table test: syms-equal?) normalized-path '()))
        (set! *ns-counter* (+ *ns-counter* 1))
        (table-ref modules normalized-path)))))


(define (me-ns-branch-out! ns)
  (define new-ns (make-me-ns *ns-counter* (make-table test: syms-equal?) ns '()))
  (set! *ns-counter* (+ *ns-counter* 1))
  (me-ns-children-set! ns (cons new-ns (me-ns-children ns)))
  new-ns)


(define (me-ns-set-var! ns sym value)
  (let ((dict (me-ns-dict ns)))
    (with-exception-catcher
      (lambda (e)
        (if (unbound-key-exception? e)
          (table-set! dict sym value)
          (raise e)))
      (lambda ()
        (table-ref dict sym)
        (error 'duplicate-definitions sym)))))


(define (me-ns-ref ns sym . def)
  (define (inner ns)
    (if (not ns)
      (if (null? def)
        (error 'unbound-variable sym)
        (car def))
      (with-exception-catcher
        (lambda (e)
          (if (unbound-key-exception? e)
            (inner (if (string? (me-ns-parent-or-modpath ns))
                     #f
                     (me-ns-parent-or-modpath ns)))
            (raise e)))
        (lambda () (table-ref (me-ns-dict ns) sym)))))
  (inner ns))


(define (get-module-varnames full-path)
  (map car (table->list (me-ns-dict (get-module-ns! full-path)))))


(define deferred-import-statements '())


(define (add-deferred-import-statement! proc)
  (set! deferred-import-statements (cons proc deferred-import-statements)))


(define (run-deferred-import-statements!)
  (for-each (lambda (p) (p)) (reverse deferred-import-statements)))


(define (with-dynamic-ns ns final-scheme-expr)
  (define path (me-ns-module-path ns))
  (define flat (make-table test: syms-equal?))

  (define (flatten ns)
    (for-each
      (lambda (kv)
        (if (not (table-ref flat (car kv) #f))
          (table-set! flat (car kv) (cdr kv))))
      (table->list (me-ns-dict ns)))
    (if (not (string? (me-ns-parent-or-modpath ns)))
      (flatten (me-ns-parent-or-modpath ns))))

  (define (build-scheme-sexp alist)
    (if (null? alist)
      (list final-scheme-expr)
      (cons `(define ,(get-variable-symbol! (caar alist) path)
               ,(if (symbol? (cdar alist))
                  (cdar alist)
                  (procedure-with-dynamic-ns (cdar alist))))
            (build-scheme-sexp (cdr alist)))))

  (flatten ns)

  `(let () ,@(build-scheme-sexp (table->list flat))))


(define procedure-ns-table (make-table test: eq?))
(define syntactic-procedures (make-table test: eq?))

(define (add-procedure-ns! proc-sexp ns)
  (table-set! procedure-ns-table proc-sexp ns))

(define (get-procedure-ns proc-sexp)
  (table-ref procedure-ns-table proc-sexp))

(define (declare-procedure-a-macro! proc-sexp)
  (table-set! syntactic-procedures proc-sexp #t))

(define (macro? proc-sexp)
  (table-ref syntactic-procedures proc-sexp #f))

(define (procedure-sexp? sexp-code)
  (if (table-ref procedure-ns-table sexp-code #f) #t #f))

(define (procedure-with-dynamic-ns proc-sexp)
  (define proc-ns (get-procedure-ns proc-sexp))
  (define new-dict (make-table test: syms-equal?))

  (for-each (lambda (kv) (table-set! new-dict (car kv) (cdr kv)))
    (filter (lambda (kv) (not (equal? (cdr kv) proc-sexp)))
            (table->list (make-table test: syms-equal?))))

  (with-dynamic-ns (me-ns-dict-set proc-ns new-dict) proc-sexp))

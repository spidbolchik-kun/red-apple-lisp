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


(define (gensym!)
  (let ((old-counter *variable-counter*))
    (set! *variable-counter* (+ 1 *variable-counter*))
    (add-v-prefix old-counter)))


(define (get-variable-symbol! name mod-path)
  (let* ((pair (list name mod-path (table-ref sym-ns-map name #f)))
         (num (table-ref *sym->num* pair #f))
         (old-counter *variable-counter*))
    (add-v-prefix
      (if num num
        (begin
          (table-set! *sym->num* pair *variable-counter*)
          (set! *variable-counter* (+ 1 *variable-counter*))
          old-counter)))))


(define rec-sym (get-variable-symbol! "#rec" #!void))
(define cn-rec-sym (get-variable-symbol! "#cn:rec" #!void))


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
  (equal? (cons x (table-ref sym-ns-map x #f))
          (cons y (table-ref sym-ns-map y #f))))


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


; Used to distinguish between regular procedures and procedures
; defined using the <define-macro> special form.
; Keys are <ra::procedure>s, values are s-expression tree paths.
(define *syntactic-procedures* (make-table test: eq?))


(define (add-syntactic-procedure! proc ns)
  (table-set! *syntactic-procedures* proc ns))


(define (get-syntactic-procedure-ns obj)
  (table-ref *syntactic-procedures* obj #f))


(define (get-module-varnames full-path)
  (map car (table->list (me-ns-dict (get-module-ns! full-path)))))

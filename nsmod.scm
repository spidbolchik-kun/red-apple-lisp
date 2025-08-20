(include "dt.scm")


; Used for conversion between unquoted strings in s-expressions and Scheme symbols.
(define *variable-counter* 0)
(define *sym->num* (make-table))


(define (add-v-prefix num)
  (string->symbol
    (string-append
      "v"
      (number->string num))))


(define (gensym!)
  (let ((old-counter *variable-counter*))
    (set! *variable-counter* (+ 1 *variable-counter*))
    (add-v-prefix old-counter)))


(define (get-variable-symbol! name prefix)
  (let* ((pair (cons name prefix))
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


; Used to distinguish between regular procedures and procedures
; defined using the <define-macro> special form.
; Keys are <ra::procedure>s, values are s-expression tree paths.
(define *syntactic-procedures* (make-table))


(define (add-syntactic-procedure! proc ns-or-path)
  (table-set! *syntactic-procedures*
              proc
              (fn-if ct-ns? ct-ns-path ns-or-path)))


(define (get-syntactic-procedure-path obj)
  (table-ref *syntactic-procedures* obj #f))


; Namespaces with values known in compile-time.
; Contain lists of not statically known values.
; 
; Namespace paths are of format:
;  `(,@<node-paths-in-s-expression-tree-in-normal-form>
;  <normalized-module-path>)
(define-structure ct-ns path dyn dictionary)


(define *ct-ns-table* (make-table))


(define (ns-by-path ns-or-path)
  (if (ct-ns? ns-or-path)
    ns-or-path
    (table-ref *ct-ns-table* ns-or-path)))


(define (ct-ns-ref ns-or-path sym)
  (let loop ((path (fn-if ct-ns? ct-ns-path ns-or-path)))
    (let ((ns (table-ref *ct-ns-table* path)))
      (if (member sym (ct-ns-dyn ns))
        (error 'variable-not-statically-known sym))
      (with-exception-catcher
        (lambda (e)
          (if (and (error-exception? e)
                   (equal? (error-exception-message 'no-such-key)))
            (if (= (length path) 1)
              (error 'undefined-variable sym)
              (loop (table-ref *ct-ns-table* (cdr path))))
            (raise e)))
        (lambda () (ra::dictionary-ref (ct-ns-dictionary ns) sym))))))


(define (ct-ns-set! ns-or-path sym value)
  (let ((ns (ns-by-path ns-or-path)))
    (ct-ns-dictionary-set! ns
      (ra::dictionary-set (ct-ns-dictionary ns) sym value))))


(define (ct-ns-not-statically-known-set! ns-or-path sym)
  (let ((ns (ns-by-path ns-or-path)))
    (ct-ns-dyn-set! ns (cons sym (ct-ns-dyn ns)))))


(define (ct-ns-branch-out! ns-or-path child-node)
  (let* ((path (cons child-node (fn-if ct-ns? ct-ns-path ns-or-path)))
         (ns (make-ct-ns path '() ra::empty-dictionary)))
    (table-set! *ct-ns-table* path ns)
    ns))


; Normalized modules code
(define *mod-sexps* '())


(define (add-module-code! normalized-path sexp)
  (if (assoc normalized-path sexp)
    (error 'module-already-exists normalized-path))
  (set! *mod-sexps* (cons `(,normalized-path . ,sexp) *mod-sexps*)))


(define (get-ns-sexp ns-or-path)
  (let ((path (fn-if ct-ns? ct-ns-path ns-or-path)))
    (let loop ((nodes (apply append (reverse (but-last path))))
               (code (cdr (assoc (last path) *mod-sexps*))))
      (if (null? nodes)
        code
        (loop (cdr nodes) ((caar nodes) code (cadar nodes)))))))


; TODO переделать

(define *mod-top-level-varnames* '())

(define (add-module-top-level-varname! full-path varname)
  (define new
    (let loop ((modules *mod-top-level-varnames*))
      (if (null? modules)
        `((,full-path . (,varname)))
        (if (equal? (caar modules) full-path)
          (cons (cons (caar modules) (cons varname (cdar modules)))
                (cdr modules))
          (cons (car modules) (loop (cdr modules)))))))
  (set! *mod-top-level-varnames* new))


(define (get-module-varnames full-path)
  (cdr (assoc full-path *mod-top-level-varnames*)))

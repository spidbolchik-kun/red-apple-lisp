(import (srfi 69))


(define (one-of? obj ls)
  (if (null? ls)
    #f
    (if (equal? (car ls) obj)
      #t
      (one-of? obj (cdr ls)))))


(define (o . functions)
  (fold-right
    (lambda (f0 f1) (lambda (arg) (f0 (f1 arg))))
    identity
    functions))


(define (decimal-digit-number->char digit)
  (let ((str (number->string digit)))
    (and (= (string-length str) 1)
         (string-ref str 0))))


(define (reversed-cons x y) (cons y x))


(define (cons-enumerate ls #!key (reversed #f))
  (define conser (if reversed reversed-cons cons))
  (define (inner-cons-enumerate i ls)
    (if (null? ls)
      '()
      (cons (conser i (car ls))
            (inner-cons-enumerate (+ 1 i) (cdr ls)))))
  (inner-cons-enumerate 0 ls))


(define (cdr-assoc-or-false obj ls)
  (let ((pair (assoc obj ls)))
    (and pair (cdr pair))))


(define (subset? set of)
  (fold-right
    (lambda (x y) (and x y))
    #t
    (map (lambda (e) (one-of? e of)) of)))


(define (conjoin . preds)
  (lambda (val)
    (fold-right
      (lambda (x y) (and x y))
      #t
      (map (lambda (p) (p val)) preds))))


(define (const x) (lambda (y) x))


(define (set-diff ls0 ls1)
  (filter (lambda (o) (not (one-of? o ls1))) ls0))


(define (exception-string exc)
  (with-output-to-string
    (lambda () (display-exception exc))))


(define (dir-from-path path #!key (index (- (string-length path) 1)))
  (if (<= index 0)
    #f
    (if (equal? (string-ref path index) #\/)
      (substring path 0 (+ index 1))
      (dir-from-path path index: (- index 1)))))


(define (cont-cons head cont)
  (lambda (acc) (cont (cons head acc))))


(define (void? obj) (equal? obj (void)))


(define (fn-if pred-or-bool fn #!optional (val (void)))
  (define pred
    (if (boolean? pred-or-bool)
      (const pred-or-bool)
      pred-or-bool))
  (define (inner val)
    (if (pred val) (fn val) val))
  (if (equal? val (void)) inner (inner val)))


(define (map-with fn)
  (lambda (obj) (map fn obj)))


(define (map-over ls)
  (lambda (f) (map f ls)))


(define (wrap-into-list head)
  (lambda (obj) (list head obj)))


(define (cons-with head)
  (lambda (obj) (cons head obj)))


(define (unzip ls)
  (cons (map car ls) (map cdr ls)))


(define (nonempty-list? val)
  (and (list? val) (not (null? val))))


(define (take-while pred ls)
  (if (or (null? ls) (not (pred (car ls))))
    '()
    (cons (car ls) (take-while pred (cdr ls)))))


(define (but-last ls)
  (if (or (null? ls) (= (length ls) 1))
    '()
    (cons (car ls) (but-last (cdr ls)))))


(define (string-but-last str)
  (list->string (but-last (string->list str))))


(define (replace obj with in)
  (map (lambda (x) (if (equal? x obj) with x)) in))


(define (partition equiv-pred ls #!key (res '()))
  (define (insert-if-not-in val res)
    (if (null? res)
      (list (list val))
      (if (equiv-pred (caar res) val)
        (cons (cons val (car res)) (cdr res))
        (cons (car res) (insert-if-not-in val (cdr res))))))

  (if (null? ls)
    res
    (partition equiv-pred (cdr ls)
      res: (insert-if-not-in (car ls) res))))


(define (equal-by fn)
  (lambda (x y) (equal? (fn x) (fn y))))


(define (equal-to x)
  (lambda (y) (equal? x y)))


(define (find-one pred ls #!key (on-failure (void)))
  (if (null? ls)
    on-failure
    (if (pred (car ls))
      (car ls)
      (find-one pred (cdr ls) on-failure: on-failure))))


(define (unique ls #!key (acc '()))
  (if (null? ls)
    '()
    (if (one-of? (car ls) acc)
      (unique (cdr ls) acc: acc)
      (cons (car ls)
            (unique (cdr ls) acc: (cons (car ls) acc))))))


(define (alist-delete key alist)
  (filter (lambda (kv) (not (equal? key (car kv))))
          alist))


(define (tree-map f obj)
  (cond ((list? obj)
         (map (lambda (obj) (tree-map f obj)) obj))
        ((pair? obj)
         (cons (f (car obj)) (f (cdr obj))))
        (else (f obj))))


(define (display-in-red-with-newline string #!key stderr)
  (print port: (if stderr (current-error-port) (current-output-port))
         "\033[31m"
         string
         "\033[0m\n"))


(define-syntax quote-sexp-with-symbols->strings
  (syntax-rules ()
    ((_ e ...)
     (tree-map
       (lambda (obj)
         (cond ((symbol? obj) (symbol->string obj))
               ((string? obj) (cons "quote" obj))
               (else obj)))
       '(e ...)))))

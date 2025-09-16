(include "utils.scm")


; We are using the simplest data structure which preserves the order of inserted keys here.
; The choice of the appropriate data structure for faster access time is done by the optimizer.
(define-structure ra::dictionary alist)


(define ra::empty-dictionary (make-ra::dictionary '()))


; This structure is for labeling any Scheme values.
; Labels get erased before arguments being passed to ##-prefixed Gambit procedures.
(define-structure ra::labeled data meta)


(define (ra::erase-all-labels val)
  (if (ra::labeled? val)
    (ra::labeled-data val)
    val))


(define (ra::over-possibly-labeled f val)
  (let ((new-data (f (ra::erase-all-labels val))))
    (if (ra::labeled? val)
      (ra::labeled-data-set val new-data)
      new-data)))


(define (ra::dictionary-delete dictionary key)
  (make-ra::dictionary
    (filter (lambda (kv) (not (equal? (car kv) key)))
            (ra::dictionary-alist dictionary))))


(define (ra::dictionary-delete-many dictionary keys)
  (make-ra::dictionary
    (filter (lambda (kv) (not (member (car kv) keys)))
            (ra::dictionary-alist dictionary))))


(define (ra::dictionary-set dictionary key value)
  (make-ra::dictionary
    (cons (cons key value) (ra::dictionary-alist dictionary))))


(define (ra::dictionary->list dictionary)
  (define ht (make-table))
  (let loop ((alist (ra::dictionary-alist dictionary))
             (acc '()))
    (if (null? alist)
      acc
      (if (table-ref ht (caar alist) #f)
        (loop (cdr alist) acc)
        (begin
          (table-set! ht (caar alist) #t)
          (loop (cdr alist) (cons (car alist) acc)))))))


(define (ra::dictionary-empty? dict)
  (null? (ra::dictionary-alist dict)))


(define (ra::dictionary-merge/left-overrides d1 d2)
  (make-ra::dictionary
    (append (ra::dictionary-alist d1)
            (ra::dictionary-alist d2))))


(define (ra::dictionary-ref dict key . default)
  (let ((res (assoc key (ra::dictionary-alist dict))))
    (if (not res)
      (if (null? default)
        (error 'no-such-key key)
        (car default))
      (cdr res))))


(define (ra::dictionary-has? dictionary key)
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (ra::dictionary-ref dictionary key)
      #t)))


(define (ra::dictionary-pick dictionary keys)
  (make-ra::dictionary
    (apply append
      ((map-over (unique keys))
       (lambda (key)
         (with-exception-catcher
           (lambda (e)
             (if (equal? (error-exception-message e) 'no-such-key)
               '()
               (raise e)))
           (lambda ()
             (list (ra::dictionary-ref dictionary key)))))))))


(define (ra::set-label obj key val)
  (if (ra::dictionary? obj)
    (error 'labeling-dictionaries-not-allowed obj)
    (if (not (ra::labeled? obj))
      (make-ra::labeled
        obj
        (make-ra::dictionary (list (cons key val))))
      (ra::labeled-meta-set
        obj
        (ra::dictionary-set (ra::labeled-meta obj) key val)))))


(define (ra::del-label obj key)
  (if (not (ra::labeled? obj))
    obj
    (let ((new-meta (ra::dictionary-delete (ra::labeled-meta obj) key)))
      (if (ra::dictionary-empty? new-meta)
        (ra::labeled-data obj)
        (ra::labeled-meta-set obj new-meta)))))


(define (ra::get-label obj key . default)
  (let ((dict (if (ra::labeled? obj) (ra::labeled-meta obj) ra::empty-dictionary)))
    (if (not (ra::dictionary-has? dict key))
      (if (null? default)
        (error 'no-such-label)
        (car default))
      (ra::dictionary-ref dict key))))


(define (ra::labels-pick obj labels)
  (make-ra::labeled (ra::labeled-data obj)
    (ra::dictionary-pick (ra::labeled-meta obj) labels)))


; To be called from Red Apple Lisp code.
(define (ra::get structure args #!key stop-on-void)
  (define (list-ref-or-void ls i)
    (let ((i (if (< i 0) (+ i (length ls)) i)))
      (if (<= 0 i (- (length ls) 1))
        (list-ref ls i)
        #!void)))

  (define (string-ref-or-void str i)
    (let ((i (if (< i 0) (+ i (string-length str)) i)))
      (if (<= 0 i (- (string-length str) 1))
        (string (string-ref str i))
        #!void)))

  (let loop ((structure structure) (args args))
    (if (or (null? args)
            (and stop-on-void
                 (equal? (ra::erase-all-labels structure) #!void)))
      structure
      (let* ((structure (ra::erase-all-labels structure))
             (new-structure
        (cond ((ra::dictionary? structure)
               (ra::dictionary-ref structure (car args) #!void))
              ((list? structure)
               (list-ref-or-void structure (car args)))
              ((string? structure)
               (string-ref-or-void structure (car args)))
              (else (error 'unsupported-data structure args)))))
        (loop new-structure (cdr args))))))


(define (ra::get* structure . args) (ra::get structure args))


; Procedure wrapper for custom call and argument passing semantics.
(define-structure ra::procedure code meta enclosed)


; Values not known at compile tile
(define-structure ra::me-hole-type)
(define ra::me-hole (make-ra::me-hole-type))


(define-structure ra::unbound-type)
(define ra::unbound (make-ra::unbound-type))

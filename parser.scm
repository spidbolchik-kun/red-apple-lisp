;read-file-string


(include "utils.scm")


(define-structure position i line col)
(define-structure cursor module-str position prev)
(define-structure range module-str start end)
(define-structure ast-obj type data range path)
(define-structure parsing-error type marked)


(define (range->string range)
  (define (newline-or-end module-str i)
    (if (or (= i (string-length module-str))
            (equal? (string-ref module-str i) #\newline))
      i
      (newline-or-end module-str (+ i 1))))

  (string-append
    "\033[90m"
    (substring
      (range-module-str range)
      (- (position-i (range-start range))
         (- (position-col (range-start range)) 1))
      (position-i (range-start range)))
    "\033[0m"
    (substring
      (range-module-str range)
      (position-i (range-start range))
      (position-i (range-end range)))
    (let ((new-i (newline-or-end
                   (range-module-str range)
                   (position-i (range-end range)))))
      (if (equal? (position-i (range-start range)) new-i)
        ""
        (string-append
          "\033[90m"
          (substring (range-module-str range)
                     (position-i (range-end range))
                     new-i)
          "\033[0m")))))


(define (code-ast-obj->string obj)
  (if (equal? obj #!void)
    #!void
    (range->string (ast-obj-range obj))))


(define (range-lines-cols range)
  (list
    (position-line (range-start range))
    (position-col (range-start range))
    (position-line (range-end range))
    (position-col (range-end range))))


(define (code-ast-obj-lines-cols obj)
  (if (equal? obj #!void)
    #!void
    (range-lines-cols (ast-obj-range obj))))


(define (init-cursor module-str)
  (make-cursor module-str (make-position 0 1 1) #!void))


(define (init-range c0 c1)
  (if (not (eq? (cursor-module-str c0)
                (cursor-module-str c1)))
    (error "unable to make a range: different module strings")
    (make-range (cursor-module-str c0)
                (cursor-position c0)
                (cursor-position c1))))

(define (cursor-get-char cursor)
  (let ((index (position-i (cursor-position cursor)))
        (module-str (cursor-module-str cursor)))
    (and (< index (string-length module-str))
         (string-ref module-str index))))

(define (cursor-step cursor)
  (cursor-prev-set
    (cursor-position-set cursor
      (let ((position (cursor-position cursor)))
        (position-i-set
          (case (cursor-get-char cursor)
            ((#f) (error "reached the end of string"))
            ((#\newline)
             (position-line-set
               (position-col-set position 1)
               (+ 1 (position-line position))))
            (else (position-col-set position
                    (+ 1 (position-col position)))))
          (+ 1 (position-i position)))))
    cursor))

(define interpreted-as-space
  '(#\alarm
    #\backspace
    #\delete
    #\escape
    #\newline
    #\null
    #\return
    #\space
    #\tab
    #\,))

(define (get-closing-char char)
  (define closing-chars
    '((#\( . #\))
      (#\{ . #\})
      (#\[ . #\])
      (#\" . #\")
      ("`(" . #\))))
  (let ((result (assoc char closing-chars)))
    (and result (cdr result))))

(define separated-chars
  '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\`))

(define types
  '(top-level
    unquoted-symbol
    #\space #\( #\{ #\[ #\" #\' #\\ #\x "`("))

(define (hex-char-list-to-unicode-char char-list)
  (define lookup-table
    (cons-enumerate
      (append (map decimal-digit-number->char (iota 10))
              '(#\a #\b #\c #\d #\e #\f))
      reversed: #t))
  (define hex-digits
    (map (o (lambda (obj) (cdr-assoc-or-false obj lookup-table))
            char-downcase)
         char-list))
  (if (> (length (filter not hex-digits)) 0)
    'invalid-letter
    (let ((char-num
            (apply +
              (map (lambda (digit weight)
                     (* digit (expt 16 weight)))
                   hex-digits
                   (reverse (iota (length hex-digits)))))))
      (if (not char-num)
        #f
        (let ((char (with-exception-catcher
                      (lambda (e) #f)
                      (lambda () (integer->char char-num)))))
          (and char
               (or (char<=? #\null char #\xd7ff)
                   (char<=? #\xe000 char #\x10ffff))
               char))))))


(define (parse-unquoted-symbol str)
  (cond ((string=? str "#f") #f)
        ((string=? str "#t") #t)
        (else (or (string->number str) str))))


(define (finalize-parsing-error path parsing-error)
  (let ((name (car (parsing-error-type parsing-error)))
        (type (cadr (parsing-error-type parsing-error)))
        (marked (parsing-error-marked parsing-error)))
    (make-parsing-error
      (string->symbol
        (string-append
          (symbol->string name)
          " "
          (if (char? type) (string type) (symbol->string type))))
      (let ((range (if (range? marked)
                     marked
                     (init-range marked (cursor-step marked)))))
        `(list ,path
               (quote ,(range-lines-cols range))
               ,(range->string range)
               (quote '()))))))


(define (parse str path)
  (define (run-parse type cursor-start #!key (kont identity) (wrap-in-ast-obj #t))
    (define cursor
      (if (one-of? type '(top-level #\( #\[ #\{ "`("))
        (cdr (run-parse #\space cursor-start))
        cursor-start))
    (define char (cursor-get-char cursor))
    (define (wrap scan-result)
      (if (or (parsing-error? scan-result)
              (not wrap-in-ast-obj)
              (one-of? type '(#\space #\x)))
        scan-result
        (let ((type (if (equal? type #\') #\" type)))
          (make-ast-obj
            type
            (car scan-result)
            (init-range
              (if (one-of? type '(#\( #\[ #\{ #\"))
                (cursor-prev cursor)
                (if (equal? type "`(")
                  (cursor-prev (cursor-prev cursor))
                  cursor))
              (cdr scan-result))
            path))))

    (define (terminate)
      (if (equal? type #\space)
        #f
        (if (one-of? type '(unquoted-symbol #\' #\" #\;))
          (let ((term (list->string (filter identity (kont '())))))
            (if (equal? type 'unquoted-symbol)
              (parse-unquoted-symbol term)
              term))
          (kont '()))))

    (if (equal? char #f)
      (if (one-of? type '(#\( #\{ #\[ #\" #\x "`("))
        (make-parsing-error (list 'unterminated type) #f)
        (cons (terminate) cursor))
      (if (and
            (one-of? type '(top-level #\( #\[ #\{ "`("))
            (one-of? char
              (set-diff '(#\) #\} #\]) (list (get-closing-char type)))))
        (make-parsing-error (list 'bad-terminator type) cursor)
        (cond
          ((or (and (equal? type #\space)
                    (not (one-of? char interpreted-as-space)))
               (and (one-of? type '(#\' unquoted-symbol))
                    (one-of? char
                      (append interpreted-as-space separated-chars))))
           (wrap (cons (terminate) cursor)))

          ((or (equal? char (get-closing-char type))
               (and (equal? type #\x) (equal? char #\;))
               (and (equal? type #\;) (equal? char #\newline)))
           (wrap (cons (terminate) (cursor-step cursor))))

          (else
            (cond
              ((equal? type #\\)
               (if (one-of? char '(#\newline #\space))
                 (run-parse #\space cursor)
                 (let ((to-return
                   (case char
                     ((#\a) #\alarm)
                     ((#\b) #\backspace)
                     ((#\t) #\tab)
                     ((#\n) #\newline)
                     ((#\r) #\return)
                     ((#\") #\")
                     ((#\\) #\\)
                     (else #f))))
                   (if to-return
                     (cons to-return (cursor-step cursor))
                     (if (equal? char #\x)
                       (let ((hex-scan (run-parse #\x (cursor-step cursor))))
                         (if (parsing-error? hex-scan)
                           hex-scan
                           (let ((range (init-range cursor (cdr hex-scan)))
                                 (char-result
                                   (hex-char-list-to-unicode-char (car hex-scan))))
                             (if (char? char-result)
                               (cons char-result (cdr hex-scan))
                               (make-parsing-error
                                 (list (or char-result 'out-of-range) #\x)
                                 range)))))
                       (make-parsing-error (list 'bad-escaped-char type) cursor))))))

              (else
                (let ((scan
                  (cond ((one-of? type '(top-level #\( #\[ #\{ "`("))
                         (let ((new-type
                                 (if (one-of? char '(#\( #\[ #\{ #\` #\" #\' #\;))
                                   char
                                   'unquoted-symbol)))
                           (if (equal? new-type #\`)
                             (if (equal? (cursor-get-char (cursor-step cursor)) #\()
                               (run-parse "`(" (cursor-step (cursor-step cursor)))
                               (make-parsing-error
                                 (list 'expected-parenthesis-after-backquote type)
                                 (cursor-step cursor)))
                             (run-parse
                               new-type
                               (if (equal? new-type 'unquoted-symbol)
                                 cursor
                                 (cursor-step cursor))))))
                        ((and (equal? type #\") (equal? char #\\))
                         (run-parse #\\ (cursor-step cursor)))
                        (else (cons char (cursor-step cursor))))))
                  (if (parsing-error? scan)
                    (if (equal? (car (parsing-error-type scan)) 'unterminated)
                      (parsing-error-marked-set scan cursor)
                      scan)
                    (let ((new-cursor (if (pair? scan)
                                        (cdr scan)
                                        (cursor-position-set cursor
                                          (range-end (ast-obj-range scan)))))
                          (obj (if (pair? scan) (car scan) scan)))
                      (wrap
                        (run-parse type new-cursor
                          kont: (lambda (acc) (kont (cons obj acc)))
                          wrap-in-ast-obj: #f))))))))))))
  (let ((result (run-parse 'top-level (init-cursor str))))
    (if (parsing-error? result)
      (finalize-parsing-error path result)
      result)))




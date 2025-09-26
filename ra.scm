;(define library-path (car (read-file-string-list "library-path")))
(load (string-append library-path "mspm"))


(define (main args #!key to-compile)
  (if (null? args)
    (ra::display-in-red "specify file to run\n")
    (if (equal? (car args) "--compile")
      (main (cdr args) to-compile: #t)
      (cond
        ((> (length args) 1) (ra::display-in-red "one file expected for compilation\n"))
        ((< (string-length (car args)) 1) (ra::display-in-red "empty file string\n"))
        (else ((if to-compile ra-compile ra-transpile-and-run) (car args)))))))


(main (cdr (command-line)))

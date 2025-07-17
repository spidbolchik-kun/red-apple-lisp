(define library-path (car (read-file-string-list "library-path")))

(define (main . args)
  (load (string-append library-path "mspm"))
  (cond ((null? args) (ra::display-in-red "specify file to run\n"))
        ((> (length args) 1) (ra::display-in-red "one file expected for compilation\n"))
        ((< (string-length (car args)) 1) (ra::display-in-red "empty file string\n"))
        (else (ra-transpile-and-run (car args)))))


(apply main (cdr (command-line)))

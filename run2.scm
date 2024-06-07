(load "macros2.scm")

(define (main)
  (let ((r (with-input-from-file
             "test1.txt" read)))
    (write r)
    (newline)))

(main)
(exit)
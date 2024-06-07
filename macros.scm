(define-macro when
  (lambda (test . body) `(if ,test (begin ,@body) #f)))
(define-macro unless 
  (lambda (test . body) `(if ,test #f (begin ,@body))))


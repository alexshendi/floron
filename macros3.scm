(macro unless 
  (lambda (form)
   `(when (not ,(cadr form)) ,@(cddr form))))

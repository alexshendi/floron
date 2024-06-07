(define *stdin* (current-input-port))
(define *stdout* (current-output-port))

(macro when
  (lambda (form)
    `(if ,(cadr form)
         (begin ,@(cddr form))
         #f))) 

(macro unless
  (lambda (form)
           `(if ,(cadr form)
                #f
                (begin 
                  ,@(cddr form)))))

(macro with-input-from-file
 (lambda (e) 
   `(call-with-input-file 
     ,(cadr e)
     (lambda (ip)
       (set-fluid!
         input-port ip)
       (let
         ((rr (,(caddr e))))
         (set-fluid! 
           input-port
           *stdin*)
         rr)))))

             
(macro with-output-to-file
 (lambda (e) 
   `(call-with-output-file 
     ,(cadr e)
     (lambda (op)
       (set-fluid!
         output-port op)
       (let
         ((rr (,(caddr e))))
         (set-fluid! 
           output-port
           *stdin*)
         rr)))))


(define ee 
  '(let ((r (with-input-from-file f read)))
     r))

(display (expand-macro ee))
(newline)

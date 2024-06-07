(define (scone ls new)
  (append ls (list new)))

(define (%parse-until end-chars specials res)
   (let ((char (read-char)))
     (cond
       ((char=? char #\\)
        (let ((escaped (read-char)))
          (%parse-until end-chars specials
                        (scone res escaped))))
       ((or (eof-object? char)
            (member char end-chars))
        (append res '()))
       ((member char specials)
        (let ((special-block (switch-special char)))
         (%parse-until end-chars specials
                       (scone res special-block))))
       (else
        (%parse-until end-chars specials
                      (scone res char))))))

(define (%parse-paragraph res)
   (let ((next (peek-char)))
      (if (or (eof-object? next) (char=? #\newline next))
          (append res '())
          (let ((line (parse-line)))
             (%parse-paragraph
             (append res (cons #\space line)))))))

(define (%parse res special)
   (let ((next (peek-char)))
      (cond
         ((eof-object? next) (append res '()))
         ((or (char=? #\newline next) (char=? #\space next))
          (let ((a_ (read-char)))
            (%parse res special)))
         ((member next special)
          (let ((special-line
                  (switch-special-line (read-char))))
            (%parse (scone res special-line) special)))
         (else
           (let ((paragraph (parse-paragraph)))
             (%parse (scone res paragraph) special))))))

(define (switch-special type)
   (let ((next (peek-char)))
      (cond
        ((or (char=? type #\_) (char=? type #\*))
         (if (or (char=? next #\_) (char=? next #\*))
              (cons 'strong (parse-strong type))
              (cons 'em (parse-line-until type))))
        ((char=? type #\[) (cons 'a (parse-link)))
        ((char=? type #\`) (cons 'code (parse-line-until #\`)))
        (else (error (string-append "Not a special character: "
                                    (string type)))))))

(define (switch-special-line type) 
   (cond
      ((char=? type #\!) (cons 'figure (parse-img)))
      ((char=? type #\-) (begin (parse-line) '(hr)))))

(define (parse-img)
  (read-char)
  (let ((title (list->string (%parse-until '(#\]) '() '()))))
    (read-char)
    (let ((link (list->string (%parse-until '(#\)) '() '()))))
     `((img (@ (src ,link) (title ,title)))))))

(define (parse-link)
    (let ((name (list->string (%parse-until '(#\]) '() '()))))
       (read-char)
       (let ((link (list->string (%parse-until '(#\)) '() '()))))
          (cons `(@ (href ,link)) `(,name)))))

(define (parse-strong end)
   (read-char)
   (let ((text (%parse-until (list end) '() '())))
      (read-char)
      text))

(define (parse-line)
  (%parse-until '(#\newline) special-inner '()))

(define (parse-line-until end)
  (%parse-until (list #\newline end)
                special-inner '()))

(define (parse-paragraph) (%parse-paragraph '(p)))
(define (markdown->sxml) (%parse '() special-line))

(define (parse-md-file fname)
  (with-input-from-file fname
     (lambda ()
       (let ((l1 (parse-line))
             (sx (begin (parse-line) (markdown->sxml))))
         (cons (cons 'h2 l1) sx)))))

(define special-line '(#\! #\- ))
(define special-inner '(#\* #\_ #\[ #\`))


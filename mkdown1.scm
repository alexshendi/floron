(define (read-char-1) (read-char (current-input-port)))
(define (peek-char-1) (peek-char (current-input-port)))

(define (%parse-until end-chars specials res)
   (let ((char (read-char-1)))
     (cond
       ((char=? char #\\)
        (let ((escaped (read-char)))
          (%parse-until end-chars specials
                        (cons escaped res))))
       ((or (eof-object? char)
            (member char end-chars))
        (reverse res))
       ((member char specials)
        (let ((special-block (switch-special char)))
         (%parse-until end-chars specials
                       (cons special-block res))))
       (else
        (%parse-until end-chars specials
                      (cons char res))))))

(define (%parse-paragraph res)
   (let ((next (peek-char-1)))
      (if (or (eof-object? next) (char=? #\newline next))
          (append res '())
          (let ((line (parse-line)))
             (%parse-paragraph
             (append res (cons #\space line)))))))

(define (%parse res special)
   (let ((next (peek-char-1)))
      (cond
         ((eof-object? next) (reverse res))
         ((or (char=? #\newline next) (char=? #\space next))
          (let ((a_ (read-char-1)))
            (%parse res special)))
         ((member next special)
          (let ((special-line
                  (switch-special-line (read-char-1))))
            (%parse (cons special-line res) special)))
         (else
           (let ((paragraph (parse-paragraph)))
             (%parse (cons paragraph res) special))))))

(define (switch-special type)
   (let ((next (peek-char-1)))
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
  (read-char-1)
  (let ((title (list->string (%parse-until '(#\]) '() '()))))
    (read-char-1)
    (let ((link (list->string (%parse-until '(#\)) '() '()))))
     `((img (@ (src ,link) (title ,title)))))))

(define (parse-link)
    (let ((name (list->string (%parse-until '(#\]) '() '()))))
       (read-char-1)
       (let ((link (list->string (%parse-until '(#\)) '() '()))))
          (cons `(@ (href ,link)) `(,name)))))

(define (parse-strong end)
   (read-char-1)
   (let ((text (%parse-until (list end) '() '())))
      (read-char-1)
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



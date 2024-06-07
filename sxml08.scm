;; snarfed from chibi-scheme
;; CHANGES:
;; * 2024-02-10
;; - rewrote call-with-output-string to use temporary file
;; - all output ist to the standard output port
;; - used char=? instead of eq? to compare characters
;; * 2024-02-15
;; - rewrote call-with-output-string using buffer object
;; - introduced buffer object with messages: 
;;   GET RESET and DISP.
;; - no output to stdout except for debugging. all output done
;;   via ``display-to-buffer''.
;;
(define *bufsize* 1024) 

(define *esc-table*
        '((#\< "&lt;") (#\< "&lt;") (#\" "&quot;") (#\' "&apos;")
	  (#\& "&amp;")))


(define (escape-char c)
  (let ((r (assv c *esc-table*)))
    (if r
        (cadr r)
	r)))

(define (string-ref-2 s idx)
  (write (list s (string-length s) idx))
  (string-ref s idx))

(define (make-buffer) 
  (let ((b (make-string *bufsize*)) (is 0) (r ""))
    (lambda (msg . args)
      (set! r "")
      (case msg
	((reset) (set! is 0))
	((disp) (let* ((s (display-to-string (car args)))
		       (l (string-length s)))
		  (buffer-copy! b is s) 
		  (set! is (+ is l))))
	((get) (set! r (substring b 0 is)) (set! is 0))
	(else (error "Illegal message: " msg)))
      r)))

(define (buffer-copy! b is s)
  ; (write (list 'buffer-copy! b ib is s sstart size))
  ; (newline)
  (let ((size (string-length s)))
    (let loop ((ii 0))
      (if (>= ii size)
          #f
          (begin
	     (string-set! b (+ is ii) (string-ref s ii))
	     (loop (+ ii 1)))))))

(define (open-output-string) (make-buffer))

(define (display-1 obj ostr)
  (ostr 'disp obj))

(define (newline-1 obj ostr)
  (ostr 'disp (string #\newline)))

(define (write-char-1 ch ostr)
  (ostr 'disp (string ch)))

(define (get-output-string ostr)
  (ostr 'get))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (display-to-string x)
  (cond ((string? x) x)
        ((char? x) (string x))
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else (error "don't know how to display as sxml" x))))

(define (html-display-escaped-attr str . o)
  (let ((start 0)
        (end (string-length str))
        (out (if (pair? o) (car o) (current-output-port))))
    (let lp ((from start) (to start))
      (if (>= to end)
          (display-1 (substring str from to) out)
          (let* ((c (string-ref str to))
		 (s (escape-char c)))
	    (if s
	        (begin
		  (display-1 (substring str from to) out)
                  (display-1 s out)
                  (lp (+ to 1) (+ to 1)))
                (begin 
                  (lp from (+ to 1)))))))))

(define (html-escape-attr str)
  (call-with-output-string
    (lambda (out) (html-display-escaped-attr (display-to-string str) out))))

(define (html-attr->string attr)
  (if (null? (cdr attr))
      (symbol->string (car attr))
      (let ((val (if (pair? (cdr attr)) (cadr attr) (cdr attr))))
        (string-append (symbol->string (car attr))
                       "=\"" (html-escape-attr val) "\""))))

(define (html-tag->string tag attrs)
  (let lp ((ls attrs) (res (list (symbol->string tag) "<")))
    (if (null? ls)
        (apply string-append (reverse (cons ">" res)))
        (lp (cdr ls) (cons (html-attr->string (car ls)) (cons " " res))))))

(define void-elements
  '(area base br col embed hr img input keygen link meta param source track wbr))

(define (html-display-escaped-string x . o)
  (let* ((str (display-to-string x))
         (start 0)
         (end (string-length str))
         (out (if (pair? o) (car o) (current-output-port))))
    (let lp ((from start) (to start))
      (if (>= to end)
          (display-1 (substring str from to) out)
          (let* ((c (string-ref str to))
		 (s (escape-char c)))
	    (if s
	        (begin
		  (display-1 (substring str from to) out)
                  (display-1 s out)
                  (lp (+ to 1) (+ to 1)))
                (begin 
                  (lp from (+ to 1)))))))))

(define (html-escape str)
  (call-with-output-string
    (lambda (out) (html-display-escaped-string str out))))

(define indentable-elements
  '(address article aside base blockquote body dd details dialog
    div dl dt fieldset figcaption figure footer form h1 h2 h3 h4
    h5 h6 head header hgroup hr li link main meta nav ol p pre
    script section style table title ul))

(define (indent i out)
  (do ((j (* 2 i) (- j 1))) ((= j 0)) (write-char-1 #\space out)))

;;> Render (valid, expanded) \var{sxml} as html.
;;> \var{@raw} tag is considered safe text and not processed or escaped.
(define (sxml-display-as-html sxml . args)
  (let* ((out (if (null? args) (current-output-port) (car args)))
         (args (if (null? args) args (cdr args)))
	 (indent? (if (null? args) #f (car args)))
         (args (if (null? args) args (cdr args))))
    (unless (null? args) (error "too many args"))
    (let lp ((sxml (if (and (pair? sxml) (eq? '*TOP* (car sxml)))
                       (cdr sxml)
                       sxml))
	     (depth 0))
      (cond
       ((pair? sxml)
        (let ((tag (car sxml))
              (rest (cdr sxml)))
          (cond
           ((symbol? tag)
            (cond
             ((char=? #\! (string-ref (symbol->string tag) 0))
              (display-1 "<" out) (display-1 tag out)
              (for-each (lambda (x) (display-1 " " out) (display-1 x out)) rest)
              (display-1 ">" out)
	      (newline-1 out
			 (newline-1 out)))
             ((and (eq? '@raw tag)
                   (string? (car rest)))
              (if (not (null? (cdr rest)))
                  (error "@raw takes only one value" sxml))
              (display-1 (car rest) out))
             ((and (pair? rest)
                   (pair? (car rest))
                   (eq? '@ (caar rest)))
	      (when (and indent? (memq tag indentable-elements))
		(newline-1 out)
		(indent depth out))
	      (display-1 (html-tag->string tag (cdar rest)) out)
	      (for-each (lambda (x) (lp x (+ 1 depth))) (cdr rest))
	      (unless (and (null? (cdr rest)) (memq tag void-elements))
                (display-1 "</" out) (display-1 tag out) (display-1 ">" out)))
             (else
	      (when (and indent? (memq tag indentable-elements))
		(newline-1 out)
		(indent depth out))
	      (display-1 (html-tag->string tag '()) out)
	      (for-each (lambda (x) (lp x (+ 1 depth))) rest)
	      (unless (and (null? rest) (memq tag void-elements))
                (display-1 "</" out) (display-1 tag out) (display-1 ">" out)))))
           (else
            (for-each (lambda (x) (lp x (+ 1 depth))) sxml)))))
       ((null? sxml))
       (else (html-display-escaped-string sxml out))))))

;;> Render \var{sxml} as \var{xml}.
;;> \var{@raw} tag is considered safe text and not processed or escaped.
(define (sxml->xml sxml)
  (call-with-output-string
    (lambda (out) (sxml-display-as-html sxml out))))

;;> Render \var{sxml} as simple text, stripping all tags.
(define (sxml-strip sxml)
  (call-with-output-string
    (lambda (out)
      (let strip ((x sxml))
        (cond
         ((pair? x)
          (for-each
           strip
           (if (and (pair? (cdr x)) (eq? '@ (cadr x))) (cddr x) (cdr x))))
         ((string? x)
          (display-1 x out)))))))

;;> Render \var{sxml} as text for viewing in a terminal.
(define (sxml-display-as-text sxml . o)
  (let ((out (if (pair? o) (car o) (current-output-port)))
        (sxml (if (and (pair? sxml) (null? (cddr sxml)) (eq? '*TOP* (car sxml)))
                       (cadr sxml)
                       sxml)))
    (let lp ((sxml sxml))
      (cond
       ((pair? sxml)
        (let ((tag (car sxml)))
          (cond
           ;; skip headers and the menu
           ((or (memq tag '(head style script !DOCTYPE))
                (and (eq? 'div tag)
                     (pair? (cdr sxml))
                     (pair? (cadr sxml))
                     (eq? '@ (car (cadr sxml)))
                     (equal? '(id . "menu") (assq 'id (cdr (cadr sxml)))))))
           ;; recurse other tags, appending newlines for new sections
           ((symbol? tag)
            (if (memq tag '(h1 h2 h3 h4 h5 h6))
                (newline-1 out))
            (for-each
             lp
             (if (and (pair? (cdr sxml)) (eq? '@ (cadr sxml)))
                 (cddr sxml)
                 (cdr sxml)))
            (if (memq tag '(p li br h1 h2 h3 h4 h5 h6))
                (newline-1 out)))
           (else
            (for-each lp sxml)))))
       ((null? sxml))
       (else (display-1 sxml out))))))

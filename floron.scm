(define *date-file* "date.txt")

(define (read-file f)
  (let ((r (with-input-from-file f read)))
    ; (write r) (newline)
    r))

(define (write-to-file file proc)
  ; (write (list file proc))
  ; (newline)
  ; (display proc)
  (with-output-to-file file
    (lambda () (display proc))))

; (define (atom? x)
;   (not (or (pair? x) (null? x))))

(define key car)
(define value cadr)

(define (fetch k table)
  (cond ((null? table); '())
         (error (string-append "Key not found: " (symbol->string k))))
        ((eqv? (key (car table)) k) (value (car table)))
        (else (fetch k (cdr table)))))

(define (replace old new ls)
  (cond ((null? ls) '())
        ((atom? ls)
         (if (eqv? old ls) new ls))
        ((pair? ls)
         (cons (replace old new (car ls))
               (replace old new (cdr ls))))))

(define (multireplace table ls)
  (if (null? table) ls
    (multireplace (cdr table)
      (let ((entry (car table)))
        (replace (key entry) (value entry) ls)))))

;; Post helpers;
(define (make-post-info title date id body)	
  (list (string->symbol "#<post>") title date id body))

(define (post-title obj) (cadr obj))
(define (post-date obj) (caddr obj))
(define (post-id obj) (list-ref obj 3))
(define (post-body obj) (list-ref obj 4))
(define (post? obj) (and (pair? obj) (= (length obj) 5) 
			   (eq? (car obj) (symbol->string "#<post>"))))

(define (padded num)
  (let ((str (number->string num)))
    (if (= 1 (string-length str))
      (string-append "0" str) str)))

(define (seconds->ymd)
  (with-input-from-file *date-file* read))

(define (make-post post)
  (let ((title (fetch 'title post))
        (date  (fetch 'date post))
        (id    (fetch 'id post)))
    (make-post-info title date id
      (with-input-from-file 
        (string-append (post-dir config) "\\" id "\\post.md")
        markdown->sxml))))

;; Blog helpers
(define (make-blog-info title author) 
  (list (string->symbol "#<blog>") title author))

(define (blog-title obj) (cadr obj))
(define (blog-author obj) (caddr obj))
(define (blog? obj) (and (pair? obj) (= (length obj) 3) 
			 (eq? (car obj) (symbol->string "#<blog>"))))

(define (make-blog config)
  (let ((title  (fetch 'title config))
        (author (fetch 'author config)))
    (make-blog-info title author)))

;; Configuration
(define (make-config blog posts post-dir out-dir) 
  (list (string->symbol "#<config>") blog posts post-dir out-dir))

(define (config-blog obj) (cadr obj))
(define (config-posts obj) (caddr obj))
(define (post-dir obj) (list-ref obj 3))
(define (out-dir obj) (list-ref obj 4))
(define (config? obj) (and (pair? obj) (= (length obj) 5) 
			   (eq? (car obj) (symbol->string "#<config>"))))

(define (load-config)
  (let ((cfg (read-file "config.scm")))
    (let ((config (fetch 'config cfg))
          (posts (fetch 'posts cfg)))
    ; (display `((cfg ,cfg) (posts ,posts) (config ,config))) (newline)
      (make-config (make-blog config) posts
                   (fetch 'post-dir config)
                   (fetch 'out-dir config)))))

;; Interesting stuff
(define (render page bindings)
  (multireplace bindings (read-file page)))

(define (page-title page root)
  (string-append page " | " root))

(define (make-link file)
  (string-append "\\" file))

(define (render-post post)
  (render "templ\\post.scm"
    `((post-body  ,(post-body post))
      (post-date  ,(post-date post))
      (post-title ,(post-title post)))))

(define (render-post-page blog post)
  (render "templ\\layout.scm"
    `((page-title   ,(page-title (post-title post) (blog-title blog)))
      (blog-title   ,(blog-title blog))
      (blog-author  ,(blog-author blog))
      (blog-content ,(render-post post)))))

(define (render-index-item post)
  (render "templ\\index.scm"
    `((post-title ,(fetch 'title post))
      (post-date  ,(seconds->ymd))
      (post-desc  ,(fetch 'description post))
      (post-link  ,(make-link (fetch 'id post))))))

(define (render-rss-item post)
  (render "templ\\rssitem.scm"
     `((item-title ,(fetch 'title post))
       (item-desc ,(fetch 'description post))
       (item-link ,(make-link (fetch 'id post))))))

(define (render-index blog posts)
  (render "templ\\layout.scm"
    `((page-title   ,(page-title "Index" (blog-title blog)))
      (blog-title   ,(blog-title blog))
      (blog-author  ,(blog-author blog))
      (blog-content ,(map render-index-item posts)))))

(define (render-rss blog posts)
  (render "templ\\rss.scm"
     `((rss-title ,(blog-title blog))
       (rss-item-list ,(map render-rss-item posts)))))

(define (sxml->xml-1 s) 
  (let ((r (sxml->xml s)))
    (display "SXML->XML: ")
    (write s)
    (display " ==> ") 
    (write r)
    (display (string #\newline #\newline))
    r))

;; Start
(define config (load-config))

(define (render-blog)
  (let ((posts (map make-post (config-posts config)))
        (out   (out-dir config)))
    ;(for-each write (list posts out config)) (newline)
    ;(create-directory out 493)
    (write-to-file (string-append out "\\index.html")
                   (let ((xml (sxml->xml (render-index (config-blog config)
                                                       (config-posts config)))))
		     ; (display xml) (newline) 
		     xml))

    (write-to-file (string-append out "\\rss.xml")
                   (let ((xml (sxml->xml (render-rss (config-blog config)
                                                     (config-posts config)))))
		     (string-append 
		       "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
		       xml)))	
    (for-each
      (lambda (post)
        (let ((path (string-append out "\\" (post-id post)))
              (page (render-post-page (config-blog config) post)))
          ; (create-directory path 493)
          (write-to-file (string-append path "\\index.html")
                         (sxml->xml page))))
      posts)))

; (render-blog)

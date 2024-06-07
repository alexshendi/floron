(html
  (head
    (meta (@ (charset "UTF-8")))
    (title page-title)
    (link (@ (rel stylesheet)
             (href "/blog/style.css"))))
  (body
    (header (a (@ (href "/blog/")) blog-title))
    (main blog-content)
    (footer "(c) " blog-author " 2024, powered by "
            (a (@ (href "https://github.com/steinuil/floron"))
               "Floron"))))

;;; example-theme.el --- sexp-HTML templates for denden's example site -*- lexical-binding: t; -*-

;;; Commentary:

;; Templates for denden/example: a page skeleton, a single post (title
;; and date above its content), and a post list. No JavaScript, no menu
;; dropdowns, one stylesheet. `example-site.el' supplies the config data
;; and page-dispatch logic on top of this.

;;; Code:

(require 'cl-lib)
(require 'denden)

(cl-defun example-theme-header (&key site-title menu-items)
  "The page header: SITE-TITLE linking home, then a MENU-ITEMS nav.
MENU-ITEMS is a list of (LABEL . URL) pairs."
  (list 'header nil
        (list 'a '(:class "title" :href "/")
              (list 'h2 nil site-title))
        (append (list 'nav nil)
                (mapcar (lambda (item) (list 'a (list :href (cdr item)) (car item)))
                        menu-items))))

(defun example-theme-footer ()
  "The made-with line, crediting both denden.el and its Bear Blog inspiration."
  (list 'footer nil
        "Made with " '(a (:href "https://github.com/idlip/denden") "denden.el")
        ", styled after " '(a (:href "https://github.com/janraasch/hugo-bearblog") "Hugo Bear Blog")
        "."))

(cl-defun example-theme-baseof (&key language body page-title site-title menu-items rss-url
                                     stylesheet-href)
  "The page skeleton for LANGUAGE and BODY, as a single html node."
  (list 'html (list :lang (or language "en"))
        (append
         (list 'head nil
               '(meta (:charset "utf-8"))
               '(meta (:name "viewport" :content "width=device-width, initial-scale=1.0"))
               (list 'title nil page-title))
         (when rss-url
           (list (list 'link (list :rel "alternate" :type "application/atom+xml"
                                    :href rss-url :title site-title))))
         (when stylesheet-href
           (list (list 'link (list :rel "stylesheet" :href stylesheet-href)))))
        (list 'body nil
              (example-theme-header :site-title site-title :menu-items menu-items)
              (list 'main nil body)
              (example-theme-footer))))

(cl-defun example-theme-post-page (&key title date body-html)
  "A single post's body: TITLE and DATE above its own BODY-HTML."
  (list 'div nil
        (list 'h1 nil title)
        (when date
          (list 'p '(:class "post-date")
                (list 'i nil (list 'time (list :datetime date) date))))
        (list 'div nil (list 'raw-html nil body-html))))

(defun example-theme-post-list-item (page)
  "One PAGE (a `denden-collect-page-metadata' plist) as a post-list <li>."
  (list 'li nil
        (list 'span nil
              (list 'i nil (list 'time (list :datetime (plist-get page :date)) (plist-get page :date))))
        (list 'a (list :href (concat "/" (plist-get page :url))) (plist-get page :title))))

(defun example-theme-post-list (posts)
  "POSTS (newest first) as the blog's listing page body."
  (list 'div nil
        (append (list 'ul '(:class "blog-posts"))
                (if posts
                    (mapcar #'example-theme-post-list-item posts)
                  (list '(li nil "No posts yet"))))))

(provide 'example-theme)
;;; example-theme.el ends here

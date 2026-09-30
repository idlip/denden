;;; example-site.el --- a Bear-Blog-style example built on denden.el -*- lexical-binding: t; -*-

;;; Commentary:

;; A minimal site layer: a home page, an about page, a blog listing, and
;; a couple of posts. One stylesheet, no JavaScript, no tags or
;; taxonomy pages -- in the spirit of
;; https://github.com/janraasch/hugo-bearblog/, though not a literal
;; port (that repo is a Hugo theme; denden has no theme layer separate
;; from a site's own theme file).
;;
;; Wires up exactly the contract denden.el's own commentary describes:
;; `denden-content-directory', `denden-publishing-directory',
;; `denden-project-root-directory', `denden-full-build-function' and
;; `denden-build-function', plus `denden-post-directory' for
;; `denden-new'.

;;; Code:

(require 'cl-lib)
(require 'denden)
(require 'example-theme)

(defconst example-repository-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "This example project's own root, denden/example/.")

(defconst example-content-directory (expand-file-name "content" example-repository-directory))

(defconst example-blog-directory (expand-file-name "blog" example-content-directory))

(defconst example-publishing-directory (expand-file-name "public" example-repository-directory))

(defconst example-site-title "A Plain Blog")

(defconst example-site-description "A minimal blog built with denden.el.")

(defconst example-author-name "Jane Doe")

(defconst example-menu-items '(("Home" . "/") ("Blog" . "/blog/") ("About" . "/about/"))
  "(LABEL . URL) pairs for the header nav.")

;; sidenote is an inline macro, same reasoning as site.el's own registration:
;; note/tip/warn are block-level special blocks already; a sidenote sits
;; mid-sentence, so it needs its own {{{sidenote(text)}}} macro instead.
(add-to-list 'org-export-global-macros
             (cons "sidenote" "@@html:<aside class=\"sidenote\">@@$1@@html:</aside>@@"))

(defun example-base-url ()
  "This build's base URL: the preview env var when set, else a placeholder."
  (or (getenv "DENDEN_PREVIEW_BASE_URL") "https://example.com"))

(defun example--file-title (filename)
  "FILENAME's #+TITLE:, or its base name."
  (or (cadr (assoc "TITLE" (with-temp-buffer
                              (insert-file-contents filename)
                              (org-mode)
                              (org-collect-keywords '("TITLE")))))
      (file-name-base filename)))

(defun example--file-date (filename)
  "FILENAME's #+DATE:, as an ISO string, or nil."
  (denden--org-date-to-iso
   (cadr (assoc "DATE" (with-temp-buffer
                          (insert-file-contents filename)
                          (org-mode)
                          (org-collect-keywords '("DATE")))))))

(defun example--export-body (filename all-pages)
  "FILENAME's body, exported through the denden-html backend."
  (with-temp-buffer
    (insert-file-contents filename)
    (org-mode)
    (org-export-as 'denden-html nil nil t (denden-export-options-with-pages all-pages))))

(defun example--export-body-node (filename all-pages)
  "FILENAME's exported body, as a raw-html node ready to splice into a
template. `denden-serialize-node' only emits HTML verbatim for a
`raw-html' node -- an exported string passed as a plain child is
escaped as text instead."
  (list 'raw-html nil (example--export-body filename all-pages)))

(cl-defun example-wrap-page (&key title url body rss-url)
  "Wrap BODY in `example-theme-baseof' with this site's own chrome data."
  (example-theme-baseof
   :body body :site-title example-site-title :menu-items example-menu-items
   :rss-url rss-url :stylesheet-href "/style.css"
   :page-title (if (string-empty-p (or url ""))
                    example-site-title
                  (format "%s | %s" title example-site-title))))

(defun example--write-page (page output)
  "Serialize PAGE, an `example-theme-baseof' node, to OUTPUT, doctype included."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert "<!doctype html>")
    (insert (denden-serialize-node (denden-normalise-node page)))))

(defun example--blog-file-p (filename)
  "Non-nil if FILENAME sits under `example-blog-directory'."
  (file-in-directory-p filename example-blog-directory))

(defun example--index-file-p (filename)
  "Non-nil if FILENAME is a site or section index."
  (member (file-name-base filename) '("_index" "index")))

(defun example--post-pages (all-pages)
  "ALL-PAGES under the blog section, newest first, the listing page itself
excluded."
  (denden-sort-pages-by-date-desc
   (seq-remove (lambda (page) (example--index-file-p (plist-get page :source)))
               (denden-pages-in-section all-pages "blog"))))

(defun example-publish-home-page (plist filename _pub-dir)
  "Publish FILENAME, content/_index.org, as the home page."
  (let ((all-pages (plist-get plist :denden-all-pages))
        (output (denden-output-file-for filename (cons nil plist))))
    (example--write-page
     (example-wrap-page :title "Home" :url "" :body (example--export-body-node filename all-pages))
     output)))

(defun example-publish-plain-page (plist filename pub-dir)
  "Publish FILENAME as a plain page: a title and its content, no post chrome."
  (let* ((all-pages (plist-get plist :denden-all-pages))
         (output (denden-output-file-for filename (cons nil plist)))
         (title (example--file-title filename))
         (url (denden-pretty-url (file-relative-name output pub-dir))))
    (example--write-page
     (example-wrap-page :title title :url url :body (example--export-body-node filename all-pages))
     output)))

(defun example-publish-post-page (plist filename pub-dir)
  "Publish FILENAME, a content/blog/*.org post, title and date above its
content."
  (let* ((all-pages (plist-get plist :denden-all-pages))
         (output (denden-output-file-for filename (cons nil plist)))
         (title (example--file-title filename))
         (date (example--file-date filename))
         (url (denden-pretty-url (file-relative-name output pub-dir))))
    (example--write-page
     (example-wrap-page
      :title title :url url
      :body (example-theme-post-page :title title :date date
                                      :body-html (example--export-body filename all-pages)))
     output)))

(defun example-publish-blog-list-page (plist filename pub-dir)
  "Publish FILENAME, content/blog/_index.org, as the post listing."
  (let* ((all-pages (plist-get plist :denden-all-pages))
         (output (denden-output-file-for filename (cons nil plist)))
         (url (denden-pretty-url (file-relative-name output pub-dir))))
    (example--write-page
     (example-wrap-page :title "Blog" :url url :rss-url "/index.xml"
                         :body (example-theme-post-list (example--post-pages all-pages)))
     output)))

(defun example-publish-page (plist filename pub-dir)
  "Dispatch FILENAME to the right example-publish-* function."
  (cond
   ((and (example--index-file-p filename) (example--blog-file-p filename))
    (example-publish-blog-list-page plist filename pub-dir))
   ((example--index-file-p filename)
    (example-publish-home-page plist filename pub-dir))
   ((example--blog-file-p filename)
    (example-publish-post-page plist filename pub-dir))
   (t (example-publish-plain-page plist filename pub-dir))))

;;;; org-publish project definition

(setq org-publish-project-alist
      (list
       (list "example-org"
             :base-directory example-content-directory
             :base-extension "org"
             :recursive t
             :publishing-directory example-publishing-directory
             :publishing-function 'example-publish-page
             :denden-output-extension ".html")
       (list "example-static"
             :base-directory (expand-file-name "assets" example-repository-directory)
             :base-extension 'any
             :recursive t
             :publishing-directory example-publishing-directory
             :publishing-function 'org-publish-attachment)
       (list "example" :components '("example-org" "example-static"))))

(defun example--inject-all-pages ()
  "Compute this build's page metadata once, stashed for reuse across pages."
  (let* ((leaves (org-publish-expand-projects (list (assoc "example-org" org-publish-project-alist))))
         (all-pages (denden-collect-page-metadata leaves))
         (entry (assoc "example-org" org-publish-project-alist)))
    (setcdr entry (plist-put (copy-sequence (cdr entry)) :denden-all-pages all-pages))
    all-pages))

(defun example--page-permalink (page)
  "PAGE's absolute permalink under this build's base URL."
  (concat (string-remove-suffix "/" (example-base-url)) "/" (plist-get page :url)))

(defun example--feed-pages (all-pages)
  "ALL-PAGES's blog posts, projected to `denden-atom-feed's own page shape."
  (mapcar (lambda (page)
            (list :title (plist-get page :title) :permalink (example--page-permalink page)
                  :date (plist-get page :date) :lastmod (plist-get page :date)
                  :content-html (example--export-body (plist-get page :source) all-pages)))
          (example--post-pages all-pages)))

(defun example-build-auxiliary-outputs (all-pages pub-dir)
  "Write the Atom feed, sitemap, and robots.txt into PUB-DIR."
  (let* ((base-url (example-base-url))
         (self-url (concat (string-remove-suffix "/" base-url) "/index.xml")))
    (with-temp-file (expand-file-name "index.xml" pub-dir)
      (insert (denden-atom-feed :pages (example--feed-pages all-pages) :site-title example-site-title
                                 :description example-site-description :base-url base-url
                                 :self-url self-url :author-name example-author-name)))
    (with-temp-file (expand-file-name "sitemap.xml" pub-dir)
      (insert (denden-sitemap-xml
               (mapcar (lambda (p) (list :permalink (example--page-permalink p) :lastmod (plist-get p :date)))
                       all-pages))))
    (with-temp-file (expand-file-name "robots.txt" pub-dir)
      (insert (denden-robots-txt base-url)))))

(defun example-build (&optional force)
  "Build the example site synchronously, using the cache unless FORCE."
  (let ((all-pages (example--inject-all-pages)))
    (denden-build-project (assoc "example" org-publish-project-alist) force)
    (example-build-auxiliary-outputs all-pages example-publishing-directory)))

(defconst example-async-init-file
  (expand-file-name "example-async-init.el" example-repository-directory))

(defun example-build-async (&optional force on-done)
  "Build the example site in a separate process, non-blocking."
  (let ((pub-dir example-publishing-directory)
        (org-export-async-init-file example-async-init-file))
    (org-export-async-start
        (or on-done (lambda (_) nil))
      `(let ((all-pages (example--inject-all-pages)))
         (denden-build-project (assoc "example" org-publish-project-alist) ,force)
         (example-build-auxiliary-outputs all-pages ,pub-dir)))))

(defun example-build-file-async (file on-done)
  "Rebuild just FILE in a separate process."
  (let ((org-export-async-init-file example-async-init-file))
    (org-export-async-start
        (or on-done (lambda (_) nil))
      `(progn
         (example--inject-all-pages)
         (let ((org-publish-use-timestamps-flag nil))
           (org-publish-file ,file (assoc "example-org" org-publish-project-alist)))
         nil))))

;;;; Wire this example into denden's generic customize/command surface

(setq denden-content-directory example-content-directory)
(setq denden-post-directory example-blog-directory)
(setq denden-publishing-directory example-publishing-directory)
(setq denden-project-root-directory example-repository-directory)
(setq denden-full-build-function #'example-build)
(setq denden-build-function #'example-build-async)
(setq denden-build-file-function #'example-build-file-async)

(provide 'example-site)
;;; example-site.el ends here

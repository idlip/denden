;;; denden-test.el --- Tests for denden.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the public functions of denden.el.  They need no
;; network and no real site.  Run them with `just denden-test'.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'denden)

;;;; Templates

(ert-deftest denden-test-serialize-escapes-text ()
  "Text children are escaped, quotes included."
  (should (equal (denden-serialize-node '(p nil "a < b & \"c\""))
                 "<p>a &lt; b &amp; &quot;c&quot;</p>")))

(ert-deftest denden-test-serialize-attribute-shapes ()
  "A keyword plist and an alist give the same attributes."
  (should (equal (denden-serialize-node '(a (:href "/x" :class "y") "z"))
                 "<a href=\"/x\" class=\"y\">z</a>"))
  (should (equal (denden-serialize-node '(a ((href . "/x") (class . "y")) "z"))
                 "<a href=\"/x\" class=\"y\">z</a>")))

(ert-deftest denden-test-serialize-attribute-values ()
  "Nil leaves an attribute out, t writes it alone, others are escaped."
  (should (equal (denden-serialize-node
                  '(input (:type "checkbox" :checked t :disabled nil :title "a > b")))
                 "<input type=\"checkbox\" checked title=\"a &gt; b\" />")))

(ert-deftest denden-test-serialize-void-and-normal-elements ()
  "Void elements close themselves; others always get a closing tag."
  (should (equal (denden-serialize-node '(br nil)) "<br />"))
  (should (equal (denden-serialize-node '(script (:src "a.js")))
                 "<script src=\"a.js\"></script>"))
  (should-error (denden-serialize-node '(img nil "child"))))

(ert-deftest denden-test-serialize-raw-html-and-nil-children ()
  "Raw HTML is not escaped, and nil children are skipped."
  (should (equal (denden-serialize-node
                  '(div nil (raw-html nil "<b>x</b>") nil "y"))
                 "<div><b>x</b>y</div>")))

(ert-deftest denden-test-serialize-bad-attributes ()
  "A malformed attribute list signals an error."
  (should-error (denden-serialize-node '(p (:class) "x")))
  (should-error (denden-serialize-node '(p (:class "a" id "b") "x")))
  (should-error (denden-serialize-node '(p ("class" . "a") "x"))))

;;;; Sites

(ert-deftest denden-test-site-defaults ()
  "A site made with no fields has the documented defaults."
  (let ((site (denden-site-create)))
    (should (equal (denden-site-content-directory site) "content"))
    (should (equal (denden-site-taxonomies site) '("tags")))
    (should (equal (denden-site-feed-entry-count site) 20))))

(ert-deftest denden-test-define-site-registers ()
  "`denden-define-site' sets the variable, registers it and fills the root."
  (let ((denden-sites nil)
        (default-directory "/tmp/site/"))
    (defvar denden-test--site)
    (eval '(denden-define-site denden-test--site
             "A test site."
             :title "Test")
          t)
    (should (equal (denden-site-title denden-test--site) "Test"))
    (should (eq (alist-get 'denden-test--site denden-sites) denden-test--site))
    (should (denden-site-root-directory denden-test--site))
    (eval '(denden-define-site denden-test--site "A test site." :title "Again") t)
    (should (equal (denden-site-title denden-test--site) "Again"))
    (should (= (length denden-sites) 1))))

(ert-deftest denden-test-base-url ()
  "The base URL ends in a slash, and the preview variable replaces it."
  (let ((site (denden-site-create :base-url "https://example.org"))
        (process-environment (copy-sequence process-environment)))
    (setenv denden-preview-base-url-variable nil)
    (should (equal (denden-base-url site) "https://example.org/"))
    (setenv denden-preview-base-url-variable "")
    (should (equal (denden-base-url site) "https://example.org/"))
    (setenv denden-preview-base-url-variable "http://localhost:8000")
    (should (equal (denden-base-url site) "http://localhost:8000/"))))

;;;; Pages

(ert-deftest denden-test-page-record ()
  "A page keeps its fields and a free-form meta plist."
  (let ((page (denden-page-create :url "log/x/" :title "X"
                                  :meta '(:no-comment t))))
    (should (equal (denden-page-url page) "log/x/"))
    (should (plist-get (denden-page-meta page) :no-comment))
    (setf (denden-page-body page) "<p>x</p>")
    (should (equal (denden-page-body page) "<p>x</p>"))))

;;;; Collecting pages

(defconst denden-test--fixture-site
  (expand-file-name "fixtures/site/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root folder of the small site the page tests read.")

(defun denden-test--site ()
  "Return a site for the fixture folder, with tags and refs as taxonomies."
  (denden-site-create :root-directory denden-test--fixture-site
                      :taxonomies '("tags" "refs")))

(defun denden-test--pages (&optional include-drafts)
  "Return the fixture pages, with drafts when INCLUDE-DRAFTS is non-nil."
  (let ((denden-lastmod-from-git nil)
        (denden-include-drafts include-drafts))
    (denden-collect-pages (denden-test--site))))

(defun denden-test--page (url pages)
  "Return the page of PAGES whose URL is URL."
  (seq-find (lambda (page) (equal (denden-page-url page) url)) pages))

(ert-deftest denden-test-collect-urls-and-skips ()
  "Pages get pretty URLs; drafts and dot folders are left out."
  (should (equal (mapcar #'denden-page-url (denden-test--pages))
                 '("about/" "" "log/first-post/" "log/" "log/second/" "log/third/")))
  (should (member "log/draft/" (mapcar #'denden-page-url (denden-test--pages t)))))

(ert-deftest denden-test-collect-fields ()
  "Keywords fill the page fields, and other keywords go to meta."
  (let ((page (denden-test--page "log/first-post/" (denden-test--pages))))
    (should (equal (denden-page-title page) "First Post"))
    (should (equal (format-time-string "%F" (denden-page-date page)) "2024-02-03"))
    (should (equal (denden-page-section page) "log"))
    (should (equal (denden-page-description page) "The first one."))
    (should (equal (denden-page-aliases page) '("posts/first")))
    (should (equal (denden-page-taxonomy-terms page "tags") '("emacs" "org")))
    (should (equal (denden-page-taxonomy-terms page "refs") nil))
    (should (equal (plist-get (denden-page-meta page) :no-comment) "t"))
    (should (string-search "Not a title" (denden-page-text page)))))

(ert-deftest denden-test-collect-defaults ()
  "A page with no title uses its file name, and no date falls back."
  (let ((page (denden-test--page "about/" (denden-test--pages))))
    (should (equal (denden-page-title page) "about"))
    (should (equal (denden-page-layout page) "page"))
    (should-not (denden-page-date page))
    (should-not (denden-page-lastmod page))
    (should (equal (denden-page-section page) ""))))

(ert-deftest denden-test-collect-bad-date ()
  "A date that does not exist stops collecting and names the file."
  (ert-with-temp-directory root
                           (make-directory (expand-file-name "content" root))
                           (with-temp-file (expand-file-name "content/bad.org" root)
                             (insert "#+date: [2024-13-45 Mon]\n"))
                           (let* ((denden-lastmod-from-git nil)
                                  (error-data (should-error (denden-collect-pages
                                                             (denden-site-create :root-directory root)))))
                             (should (string-search "bad.org" (cadr error-data))))))

(ert-deftest denden-test-git-table-outside-git ()
  "Outside a git repository the last-modified table is nil."
  (ert-with-temp-directory directory
                           (should-not (denden--git-lastmod-table directory))))

;;;; Page lists

(ert-deftest denden-test-sort-and-group ()
  "Pages sort newest first, undated last, and group by year."
  (let ((pages (denden-test--pages)))
    (should (equal (mapcar #'denden-page-url
                           (seq-take (denden-sort-pages-by-date pages) 3))
                   '("log/third/" "log/second/" "log/first-post/")))
    (should (equal (mapcar #'denden-page-title (denden-sort-pages-by-title pages))
                   '("about" "First Post" "Home" "Log" "Second" "Third")))
    (should (equal (mapcar (lambda (group) (cons (car group) (length (cdr group))))
                           (denden-group-pages-by-year pages))
                   '(("2025" . 2) ("2024" . 1) ("" . 3))))))

(ert-deftest denden-test-sections-and-terms ()
  "Sections, term counts and term filters read the collected pages."
  (let ((pages (denden-test--pages)))
    (should (equal (denden-sections pages) '("log")))
    (should (= (length (denden-pages-in-section pages "log")) 4))
    (should (equal (denden-term-counts pages "tags") '(("emacs" . 2) ("org" . 2))))
    (should (equal (mapcar #'denden-page-url (denden-pages-with-term pages "refs" "book"))
                   '("log/second/")))))

(ert-deftest denden-test-previous-and-next ()
  "Previous is the older page and next the newer one, in one section."
  (let* ((pages (denden-test--pages))
         (neighbours (denden-previous-and-next (denden-test--page "log/second/" pages)
                                               pages)))
    (should (equal (denden-page-url (car neighbours)) "log/first-post/"))
    (should (equal (denden-page-url (cdr neighbours)) "log/third/"))))

(ert-deftest denden-test-backlinks-and-permalink ()
  "Backlinks read the links slot; permalinks join the base URL."
  (let* ((pages (denden-test--pages))
         (target (denden-test--page "log/second/" pages))
         (source (denden-test--page "log/third/" pages)))
    (setf (denden-page-links source) '("log/second/"))
    (should (equal (denden-backlinks target pages) (list source)))
    (should (equal (denden-page-permalink (denden-site-create :base-url "https://x.org")
                                          target)
                   "https://x.org/log/second/"))
    (should (eq (denden-page-for-source (denden-page-source target) pages) target))))

;;;; Export

(defun denden-test--export (url)
  "Export the fixture page at URL and return (BODY PAGE PAGES)."
  (let* ((denden-lastmod-from-git nil)
         (site (denden-test--site))
         (pages (denden-collect-pages site))
         (page (denden-test--page url pages)))
    (list (denden-export-page site page pages) page pages)))

(ert-deftest denden-test-export-headings ()
  "Headings get ids from their text, made unique, and a # link."
  (let ((body (car (denden-test--export "log/third/"))))
    (should (string-search "<h2 id=\"getting-started\">Getting started<a class=\"heading-anchor\" href=\"#getting-started\"" body))
    (should (string-search "<h2 id=\"getting-started-2\">" body))))

(ert-deftest denden-test-export-page-links ()
  "Links to pages use the current title and are recorded for backlinks."
  (pcase-let ((`(,body ,page ,pages) (denden-test--export "log/third/")))
    (should (string-search "<a href=\"/log/second/\">Second</a>" body))
    (should (string-search "<a href=\"/log/second/\"><b>bold</b> words</a>" body))
    (should (string-search "<a href=\"/log/first-post/\">First Post</a>" body))
    (should (equal (seq-take (denden-page-links page) 2) '("log/second/" "log/first-post/")))
    (should (equal (denden-backlinks (denden-test--page "log/second/" pages) pages)
                   (list page)))
    (should (equal (denden-page-body page) body))))

(ert-deftest denden-test-export-external-links ()
  "Bare URLs show a short label, and links to other sites are recorded."
  (pcase-let ((`(,body ,page ,_pages) (denden-test--export "log/third/")))
    (should (string-search ">example.org/docs</a>" body))
    (should (equal (denden-page-external-links page)
                   '(("https://www.example.org/docs/")
                     ("https://example.org/a" . "a site"))))))

(ert-deftest denden-test-link-label-rules ()
  "The site's label rules apply after the host and path are tidied."
  (let ((site (denden-site-create
               :link-label-rules (list (cons (rx bos "github.com/" (group (+ anything)))
                                             "github:\\1")))))
    (should (equal (denden-link-label site "https://www.github.com/idlip/") "github:idlip"))
    (should (equal (denden-link-label site "https://x.org/a#b:~:text=c") "x.org/a#b"))))

(ert-deftest denden-test-export-records-unresolved-links ()
  "Links to static files and to nothing are recorded for the build check."
  (pcase-let ((`(,body ,page ,_pages) (denden-test--export "log/third/")))
    (should (equal (denden-page-links page)
                   '("log/second/" "log/first-post/" "images/cat.png" "nowhere.org")))
    (should (string-search "<a href=\"nowhere.html\">" body))))

(ert-deftest denden-test-export-code-blocks ()
  "Code blocks run only when the site allows it."
  (let* ((denden-lastmod-from-git nil)
         (text "#+begin_src emacs-lisp :exports results\n(+ 1 2)\n#+end_src\n"))
    (should-not (string-search ">3<" (denden-export-org-string
                                      (denden-test--site) text nil)))
    (should (string-search "\n3\n" (denden-export-org-string
                                    (denden-site-create :root-directory denden-test--fixture-site
                                                        :evaluate-code t)
                                    text nil)))))

(ert-deftest denden-test-export-images-and-figures ()
  "Images load lazily from site paths, and captions keep Org's figure number."
  (let* ((denden-figure-numbers t)
         (body (car (denden-test--export "log/third/"))))
    (should (string-search "<img src=\"/images/cat.png\" alt=\"cat.png\" loading=\"lazy\" decoding=\"async\">" body))
    (should-not (string-search "file://" body))
    (should (string-search "<figcaption><span class=\"figure-number\">Figure 1: </span>A cat</figcaption>" body))
    (should-not (string-search "<p loading" body))))

(ert-deftest denden-test-cosmetic-options ()
  "Anchor text, figure numbers, lazy images and code colours follow the options."
  (let* ((denden-heading-anchor-text "¶")
         (denden-figure-numbers nil)
         (denden-lazy-images nil)
         (denden-code-highlighting nil)
         (body (car (denden-test--export "log/third/"))))
    (should (string-search "aria-label=\"Link to this section\">¶</a></h2>" body))
    (should (string-search "<figcaption>A cat</figcaption>" body))
    (should-not (string-search "loading=" body))
    (should-not (string-search "org-string" body)))
  (let* ((denden-heading-anchor-text nil)
         (body (car (denden-test--export "log/third/"))))
    (should-not (string-search "heading-anchor" body))))

(ert-deftest denden-test-export-blocks ()
  "Callouts, checkboxes and highlighted code come out as the site expects."
  (let ((body (car (denden-test--export "log/third/"))))
    (should (string-search "<div class=\"callout\" role=\"note\" data-callout=\"note\">\n<p>Note</p>" body))
    (should (string-search "<input type='checkbox' checked='checked' />" body))
    (should (string-search "<pre tabindex=\"0\" class=\"src src-emacs-lisp\">" body))
    (should (string-search "<span class=\"org-string\">" body))))

(ert-deftest denden-test-export-org-string ()
  "A fragment resolves links but adds them to no page."
  (let* ((denden-lastmod-from-git nil)
         (site (denden-test--site))
         (pages (denden-collect-pages site))
         (html (denden-export-org-string site "See [[file:log/second.org]]." pages)))
    (should (string-search "<a href=\"/log/second/\">Second</a>" html))
    (should-not (seq-some #'denden-page-links pages))))

;;;; Build

(defmacro denden-test--with-build (bindings &rest body)
  "Build the fixture site into a temporary folder, then run BODY.
BINDINGS are extra fields for the site, such as (:layouts ...).  BODY
sees `site' and `public', the publishing folder."
  (declare (indent 1) (debug t))
  `(ert-with-temp-directory temporary
                            (let* ((denden-lastmod-from-git nil)
                                   (inhibit-message t)
                                   (public (expand-file-name "public" temporary))
                                   (site (denden-site-create :root-directory denden-test--fixture-site
                                                             :taxonomies '("tags" "refs")
                                                             :title "Fixture"
                                                             :publishing-directory public
                                                             ,@bindings)))
                              ,@body)))

(defun denden-test--read (file)
  "Return the text of FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest denden-test-build-writes-site ()
  "A build writes each page as index.html, and copies static files."
  (denden-test--with-build ()
    (should (equal (denden-build site) (directory-file-name public)))
    (let ((home (denden-test--read (expand-file-name "index.html" public)))
          (post (denden-test--read (expand-file-name "log/second/index.html" public))))
      (should (string-prefix-p "<!doctype html>\n<html lang=\"en\">" home))
      (should (string-search "<title>Fixture</title>" home))
      (should (string-search "<title>Second | Fixture</title>" post))
      (should (string-search "<article class=\"h-entry\"><h1 class=\"p-name\">Second</h1><p><time class=\"dt-published\" datetime=\"2025-01-10\">2025-01-10</time></p><p class=\"terms\"><a class=\"p-category\" href=\"/tags/emacs/\">#emacs</a> <a class=\"p-category\" href=\"/refs/book/\">#book</a></p><div class=\"e-content\">" post))
      (should-not (string-search "\"author\"" post))
      (should (string-search "<link rel=\"canonical\" href=\"https://example.org/log/second/\" />" post))
      (should (string-search "\"@type\":\"BlogPosting\"" post)))
    (should (file-exists-p (expand-file-name "images/cat.png" public)))
    (should (file-exists-p (expand-file-name "log/index.html" public)))
    (should-not (file-exists-p (expand-file-name "log/draft/index.html" public)))))

(ert-deftest denden-test-build-is-clean ()
  "A second build removes files the first wrote and the second does not."
  (denden-test--with-build ()
    (denden-build site)
    (with-temp-file (expand-file-name "stale.html" public) (insert "old"))
    (denden-build site)
    (should-not (file-exists-p (expand-file-name "stale.html" public)))
    (should-not (file-exists-p (concat public ".new")))
    (should-not (file-exists-p (concat public ".old")))))

(ert-deftest denden-test-build-failure-keeps-old-site ()
  "A failing build or a broken link with `error' keeps the old site."
  (denden-test--with-build ()
    (denden-build site)
    (setf (denden-site-layouts site) '(("page" . (lambda (&rest _) (error "Boom")))))
    (should-error (denden-build site))
    (setf (denden-site-layouts site) nil)
    (let ((denden-broken-link-action 'error))
      (should-error (denden-build site)))
    (should (file-exists-p (expand-file-name "about/index.html" public)))
    (should-not (file-exists-p (concat public ".new")))))

(ert-deftest denden-test-build-layouts-and-outputs ()
  "Layouts render their pages, and outputs add files the link check knows."
  (denden-test--with-build
      (:layouts '(("page" . (lambda (_site page _pages)
                              `(p nil ,(concat "Layout: " (denden-page-url page))))))
       :outputs (list (lambda (_site pages)
                        (list (cons "count.txt" (format "%d" (length pages)))))))
    (denden-build site)
    (should (string-search "<main><p>Layout: about/</p></main>"
                           (denden-test--read (expand-file-name "about/index.html" public))))
    (should (equal (denden-test--read (expand-file-name "count.txt" public)) "6"))
    (should-not (file-exists-p (expand-file-name "feed.xml" public)))))

(ert-deftest denden-test-build-clashes ()
  "Two files with one URL stop the build; an output may replace a static file."
  (denden-test--with-build
      (:outputs (list (lambda (_site _pages) (list (cons "log/second/" "x")))))
    (should-error (denden-build site)))
  (denden-test--with-build
      (:outputs (list (lambda (_site _pages) (list (cons "images/cat.png" "x")))))
    (denden-build site)
    (should (equal (denden-test--read (expand-file-name "images/cat.png" public)) "x"))))

(ert-deftest denden-test-build-refuses-unsafe-folder ()
  "The publishing folder may not hold or sit in the content folder."
  (denden-test--with-build ()
    (setf (denden-site-publishing-directory site) denden-test--fixture-site)
    (should-error (denden-build site))
    (setf (denden-site-publishing-directory site)
          (expand-file-name "content/out" denden-test--fixture-site))
    (should-error (denden-build site))))

(ert-deftest denden-test-check-links ()
  "The link check reports links to files the build did not write."
  (ert-with-temp-directory directory
                           (make-directory (expand-file-name "log/x" directory) t)
                           (with-temp-file (expand-file-name "log/x/index.html" directory))
                           (with-temp-file (expand-file-name "rss.xml" directory))
                           (let ((page (denden-page-create :source "a.org"
                                                           :links '("log/x/" "log/x" "rss.xml" "log/x/#part")))
                                 (denden-broken-link-action 'error))
                             (denden--check-links (list page) directory)
                             (setf (denden-page-links page) '("nowhere.org"))
                             (should-error (denden--check-links (list page) directory)))))

;;;; Outputs

(defun denden-test--exported-pages ()
  "Return the fixture site and its pages, every body exported, as (SITE . PAGES)."
  (let* ((denden-lastmod-from-git nil)
         (site (denden-test--site))
         (pages (denden-collect-pages site)))
    (dolist (page pages)
      (denden-export-page site page pages))
    (cons site pages)))

(ert-deftest denden-test-plain-text-and-excerpt ()
  "Plain text drops tags and anchors; the excerpt prefers the description."
  (should (equal (denden-html-plain-text
                  "<h2>Hi<a class=\"heading-anchor\" href=\"#hi\">#</a></h2>\n<p>a  &amp;\n b</p>")
                 "Hi a & b"))
  (pcase-let ((`(,_site . ,pages) (denden-test--exported-pages)))
    (should (equal (denden-page-excerpt (denden-test--page "log/first-post/" pages))
                   "The first one."))
    (should (equal (denden-page-excerpt (denden-test--page "log/second/" pages))
                   "More text."))
    (let ((denden-excerpt-paragraphs 1))
      (should (string-prefix-p "See Second and bold words and First Post."
                               (denden-page-excerpt (denden-test--page "log/third/" pages)))))))

(ert-deftest denden-test-atom-feed ()
  "The feed lists dated pages newest first, with escaped HTML content."
  (pcase-let* ((`(,site . ,pages) (denden-test--exported-pages))
               (feed (cdar (denden-feed-output site pages))))
    (should (string-prefix-p "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<feed xmlns=\"http://www.w3.org/2005/Atom\">" feed))
    (should (string-search "<link rel=\"self\" type=\"application/atom+xml\" href=\"https://example.org/feed.xml\" />" feed))
    (should (string-search "<updated>2025-03-01T00:00:00Z</updated>" feed))
    (should (< (string-search "<title>Third</title>" feed)
               (string-search "<title>Second</title>" feed)
               (string-search "<title>First Post</title>" feed)))
    (should-not (string-search "<title>about</title>" feed))
    (should (string-search "<category term=\"emacs\" label=\"emacs\" scheme=\"https://example.org/tags/\"></category>" feed))
    (should (string-search "<summary>See Second and bold words and First Post. A static file: ." feed))
    (should-not (string-search "Note</summary>" feed))
    (should (string-search "<content type=\"html\">&lt;p&gt;" feed))
    (setf (denden-site-feed-entry-count site) 1)
    (should-not (string-search "<title>Second</title>"
                               (cdar (denden-feed-output site pages))))))

(ert-deftest denden-test-sitemap-robots-redirects ()
  "The sitemap lists every page; robots names it; aliases redirect."
  (pcase-let ((`(,site . ,pages) (denden-test--exported-pages)))
    (let ((sitemap (cdar (denden-sitemap-output site pages))))
      (should (string-search "<loc>https://example.org/log/second/</loc>" sitemap))
      (should (string-search "<loc>https://example.org/</loc>" sitemap)))
    (should (equal (cdar (denden-robots-output site pages))
                   "User-agent: *\nAllow: /\n\nSitemap: https://example.org/sitemap.xml\n"))
    (let ((redirect (assoc "posts/first/" (denden-redirect-outputs site pages))))
      (should redirect)
      (should (string-search "<meta http-equiv=\"refresh\" content=\"0; url=/log/first-post/\" />" (cdr redirect)))
      (should (string-search "<a href=\"/log/first-post/\">First Post</a>" (cdr redirect))))))

(ert-deftest denden-test-search-index ()
  "The search index has one entry per sentence, with title and URL."
  (pcase-let* ((`(,site . ,pages) (denden-test--exported-pages))
               (entries (json-parse-string
                         (cdar (denden-search-index-output
                                site (list (denden-test--page "log/second/" pages))))
                         :object-type 'alist)))
    (should (equal (aref entries 0)
                   '((title . "Second") (url . "/log/second/") (text . "More text."))))
    (should (equal (denden--sentences "One. Two! Is it? ok. Last")
                   '("One." "Two!" "Is it?" "Last")))))

(ert-deftest denden-test-build-default-outputs ()
  "A default build writes the feed, sitemap, robots.txt and redirects, and links the feed."
  (denden-test--with-build ()
    (denden-build site)
    (dolist (file '("feed.xml" "sitemap.xml" "robots.txt" "posts/first/index.html"
                    "tags/index.html" "tags/emacs/index.html" "refs/book/index.html"))
      (should (file-exists-p (expand-file-name file public))))
    (should (string-search "<link rel=\"alternate\" type=\"application/atom+xml\" title=\"Fixture\" href=\"/feed.xml\" />"
                           (denden-test--read (expand-file-name "index.html" public))))
    (should (string-search "<meta name=\"description\" content=\"More text.\" />"
                           (denden-test--read (expand-file-name "log/second/index.html" public))))))

(ert-deftest denden-test-standard-keywords ()
  "Org's author, language and subtitle keywords reach the default page."
  (let ((page (denden-page-create :url "x/" :title "X" :body "<p>x</p>"
                                  :meta '(:author "Ada" :language "de" :subtitle "Sub")))
        (site (denden-site-create :author "Site Author")))
    (should (equal (denden-page-author site page) "Ada"))
    (should (equal (denden-page-language site page) "de"))
    (let ((html (denden-render-page site page (list page))))
      (should (string-search "<html lang=\"de\">" html))
      (should (string-search "<meta name=\"author\" content=\"Ada\" />" html))
      (should (string-search "<p class=\"subtitle\">Sub</p>" html)))
    (should (equal (denden-page-author site (denden-page-create)) "Site Author"))))

(ert-deftest denden-test-export-org-strings ()
  "Many fragments export together and come back one by one."
  (let* ((denden-lastmod-from-git nil)
         (site (denden-test--site))
         (pages (denden-collect-pages site)))
    (pcase-let ((`(,one ,empty ,two)
                 (denden-export-org-strings site '("*one*" "" "two [[file:log/second.org]]") pages)))
      (should (string-search "<b>one</b>" one))
      (should (equal empty ""))
      (should (string-search "two <a href=\"/log/second/\">Second</a>" two))
      (should-not (string-search "one" two)))))

(ert-deftest denden-test-export-variables ()
  "Code in a page sees the site and its pages while it exports."
  (let* ((denden-lastmod-from-git nil)
         (site (denden-site-create :root-directory denden-test--fixture-site :evaluate-code t))
         (pages (denden-collect-pages site)))
    (should (string-search
             "\n6\n"
             (denden-export-org-string
              site "#+begin_src emacs-lisp :exports results\n(length denden-current-pages)\n#+end_src\n" pages)))
    (should-not denden-current-pages)))

(provide 'denden-test)
;;; denden-test.el ends here

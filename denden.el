;;; denden.el --- Build a website from Org files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dilip

;; Author: Dilip <dilip@fossunited.org>
;; Maintainer: Dilip <dilip@fossunited.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (org "9.8") (htmlize "1.56"))
;; URL: https://github.com/idlip/denden
;; Keywords: hypermedia, outlines

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Denden is a small library for building a website from Org files.  It
;; uses Org's own export and publishing code, and adds what a site needs
;; on top: pages with metadata, layouts, feeds, a sitemap and pretty URLs.
;;
;; A site is one definition:
;;
;;   (denden-define-site my-blog
;;     "My personal blog."
;;     :title "My blog"
;;     :base-url "https://example.org/")
;;
;; Then M-x denden-build writes the site.  Templates are plain Lisp
;; lists, such as (p (:class "note") "Hello"), turned into HTML by
;; `denden-serialize-node'.  Every function that renders returns text
;; and writes nothing, so the same functions can build files or answer
;; a request.
;;
;; The name comes from the Japanese "denden mushi", a snail.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'org)
(require 'org-element)
(require 'ox-html)
(require 'url-parse)
(require 'url-util)

;;;; Options

(defgroup denden nil
  "Build a website from Org files."
  :group 'org-export
  :prefix "denden-"
  :link '(url-link "https://github.com/idlip/denden"))

(defcustom denden-include-drafts nil
  "Non-nil means build pages whose file sets #+draft: t.
A preview build may bind this to t; a real build leaves it nil."
  :type 'boolean
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-broken-link-action 'warn
  "What a build does with a link to a file the build did not write.
After a build writes every file, it checks each link between pages,
to static files and to outputs such as feeds.  `warn' shows a
message for each link to nothing, and `error' stops the build."
  :type '(choice (const :tag "Show a message" warn)
                 (const :tag "Stop the build" error))
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-lastmod-from-git t
  "Non-nil means take each page's last-modified date from git.
A page with no git history, or every page when this is nil, uses its
#+date: instead, or has no last-modified date.  The date of the file
is never used: it changes with every copy, so builds of the same
content would differ."
  :type 'boolean
  :group 'denden
  :package-version '(denden . "0.1.0"))

;; Cosmetic options: small touches on the markup Denden writes.  Each
;; default follows current practice; change them to taste.

(defcustom denden-heading-anchor-text "#"
  "Text of the link at the end of each heading, or nil for no link.
The link points to the heading itself, so a reader can copy the
address of a section.  Try \"¶\", \"§\" or an icon from your font."
  :type '(choice (string :tag "Text")
                 (const :tag "No link" nil))
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-heading-anchor-label "Link to this section"
  "Name that screen readers give the link at the end of each heading.
See `denden-heading-anchor-text'."
  :type 'string
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-code-highlighting 'css
  "How code blocks are coloured.
`css' writes classes such as org-keyword, and a style sheet gives
them colours, so colours can follow light and dark modes and change
without a new build.  To get a style sheet from your Emacs theme,
use \\[org-html-htmlize-generate-css].
`inline-css' writes the colours of your current Emacs theme into
each page, as Org does by default.
nil writes plain code with no colours."
  :type '(choice (const :tag "Classes, coloured by a style sheet" css)
                 (const :tag "Colours of the Emacs theme, in the page" inline-css)
                 (const :tag "No colours" nil))
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-figure-numbers t
  "Non-nil means show \"Figure 1:\" before each image caption, as Org does.
Org's links to a named figure use this number as their text, so
keep it when your text refers to figures."
  :type 'boolean
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-lazy-images t
  "Non-nil means images load only when they come near the screen.
This makes long pages with many images open faster.  An image whose
#+attr_html: sets :loading keeps its own value."
  :type 'boolean
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-page-title-format "%s | %s"
  "Format of the title in the browser tab, for every page but the home page.
The first %s is the page title and the second the site title.  Use
\"%1$s\" alone for the page title only, or \"%2$s: %1$s\" to
put the site first.  The home page shows the site title."
  :type 'string
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-excerpt-paragraphs 2
  "Number of paragraphs in a page's excerpt.
A page with no #+description: uses its first paragraphs as its
description, for search engines, link previews and feeds."
  :type 'natnum
  :group 'denden
  :package-version '(denden . "0.1.0"))

(defcustom denden-date-format "%F"
  "Format of dates shown on pages, for `format-time-string'.
The default, such as 2024-02-03, reads the same in every language.
For example, \"%e %B %Y\" gives 3 February 2024."
  :type 'string
  :group 'denden
  :package-version '(denden . "0.1.0"))

;;;; Sites

(cl-defstruct (denden-site (:constructor denden-site-create)
                           (:copier nil))
  "One website: where its files are and how its pages are rendered.
Every field has a default, so (denden-site-create) is a working site.
Folder fields are relative to ROOT-DIRECTORY."
  (title "My site" :documentation "Title used in the page shell and feeds.")
  (base-url "https://example.org/"
            :documentation "Address of the site.  See `denden-base-url'.")
  (description "" :documentation "Default meta description and feed subtitle.")
  (author user-full-name :documentation "Author named in feeds and meta tags.")
  (language "en" :documentation "Language of the pages, such as \"en\".")
  (root-directory nil
                  :documentation "Folder the other folders are relative to.
When nil, it is the folder of the file that defines the site.")
  (content-directory "content" :documentation "Folder with the Org files.")
  (static-directory "static" :documentation "Folder copied as it is.")
  (publishing-directory "public" :documentation "Folder the site is written to.")
  (taxonomies '("tags")
              :documentation "Keywords whose values are terms, such as tags.")
  (layouts nil
           :documentation "Alist of (LAYOUT . FUNCTION) for #+layout: values.
FUNCTION takes the site, the page and all pages, and returns the
node of the page's main part.")
  (default-layout #'denden-render-plain-page
                  :documentation "Render function for pages with no known layout.")
  (page-shell #'denden-default-page-shell
              :documentation "Function that wraps a page's main part in a page.")
  (outputs (list #'denden-feed-output #'denden-term-outputs #'denden-sitemap-output
                 #'denden-robots-output #'denden-redirect-outputs)
           :documentation "Functions that return extra files as (URL . TEXT).
Each takes the site and all pages.  Add `denden-search-index-output'
for a search index.")
  (menu nil
        :documentation "Links in the header of the default page, as (LABEL . URL).")
  (stylesheets nil
               :documentation "Addresses of the style sheets the default page links.")
  (footer nil
          :documentation "Function of the site and a page that returns more footer nodes.
The default page shell puts them after the author and the feed link;
nil adds nothing.")
  (feed-entry-count 20
                    :documentation "Number of entries in each feed, or nil for all.")
  (link-label-rules nil
                    :documentation "Alist of (REGEXP . REPLACEMENT) for bare URL labels.")
  (callout-types '(("note" . "Note") ("tip" . "Tip") ("warn" . "Warning"))
                 :documentation "Alist of special block types shown as callouts.")
  (evaluate-code nil
                 :documentation "Non-nil means run code blocks while exporting.
Only set it for content you wrote: the code runs without asking."))

(defvar denden-sites nil
  "Alist of (NAME . SITE) for every site made by `denden-define-site'.")

(defun denden--register-site (name site file)
  "Store SITE under NAME in `denden-sites' and return SITE.
When SITE has no root directory, use the folder of FILE, the file
that defines the site, or `default-directory' when FILE is nil."
  (unless (denden-site-root-directory site)
    (setf (denden-site-root-directory site)
          (if file (file-name-directory file) default-directory)))
  (setf (alist-get name denden-sites) site)
  site)

(defmacro denden-define-site (name docstring &rest fields)
  "Define NAME as a variable that holds a site, and register the site.
DOCSTRING documents the variable.  FIELDS are keyword arguments for
`denden-site-create', such as :title and :base-url.  A field that is
not given keeps its default.  Evaluating the form again replaces the
site, so a changed definition takes effect at once.

For example:

  (denden-define-site my-blog
    \"My personal blog.\"
    :title \"My blog\"
    :base-url \"https://example.org/\")"
  (declare (doc-string 2) (indent defun)
           (debug (&define name stringp &rest sexp)))
  `(progn
     (defvar ,name nil ,docstring)
     (setq ,name (denden--register-site ',name
                                        (denden-site-create ,@fields)
                                        (or load-file-name buffer-file-name)))
     ',name))

(defconst denden-preview-base-url-variable "DENDEN_PREVIEW_BASE_URL"
  "Environment variable that replaces every site's base URL when set.
A preview build sets it to a local address, such as
\"http://localhost:8000/\", so links and feeds point at the preview.")

(defconst denden-feed-url "feed.xml"
  "URL of the feed that `denden-feed-output' writes, relative to the site.")

(defun denden-base-url (site)
  "Return the base URL of SITE, always ending in a slash.
When the environment variable named by
`denden-preview-base-url-variable' is set and not empty, return its
value instead."
  (let ((preview (getenv denden-preview-base-url-variable)))
    (file-name-as-directory
     (if (and preview (not (string-empty-p preview)))
         preview
       (denden-site-base-url site)))))

;;;; Pages

(cl-defstruct (denden-page (:constructor denden-page-create)
                           (:copier nil))
  "One page of a site, collected from its Org file before rendering."
  (source nil :documentation "Absolute file name of the Org file.")
  (text nil :documentation "Text of the Org file, read once per build.")
  (url nil :documentation "Address relative to the site, such as \"log/x/\".")
  (title nil :documentation "Title from #+title:, else the file name.")
  (date nil :documentation "Date from #+date:, as a Lisp time value.")
  (lastmod nil :documentation "Date of the last change, as a Lisp time value.")
  (description nil :documentation "Text from #+description:.")
  (section nil :documentation "First folder of the URL, such as \"log\".")
  (layout nil :documentation "Value of #+layout:, or nil.")
  (draft nil :documentation "Non-nil when the file sets #+draft: t.")
  (terms nil :documentation "Alist of (TAXONOMY . TERMS), such as tags.")
  (aliases nil :documentation "Old URLs that redirect to this page.")
  (links nil :documentation "Site URLs this page links to, such as \"log/x/\".
They include pages, static files and outputs; the build checks that
each one was written.")
  (external-links nil
                  :documentation "List of (URL . TEXT) for links to other sites.
TEXT is the link's description as plain text, or nil when it has none.")
  (body nil :documentation "Body HTML, filled in the first time it is needed.")
  (meta nil :documentation "Plist of other #+KEYWORD: values, for site code."))

;; Pages are collected once per build.  Each Org file is read from disk
;; once, and its text is kept in the page, so the export later works
;; from memory.  Collecting reads only keywords, which needs no full
;; parse; the one full parse of each file is its export.

(defun denden--git-lastmod-table (directory)
  "Return a hash table from each file under DIRECTORY to its last commit time.
It uses one git call for all files.  Return nil when DIRECTORY is not
in a git repository or git is not installed."
  (let ((default-directory (file-name-as-directory directory))
        (table (make-hash-table :test #'equal))
        (time nil))
    (condition-case nil
        (dolist (line (process-lines "git" "log" "--relative" "--name-only"
                                     "--format=commit:%cs" "--" "."))
          (cond
           ((string-prefix-p "commit:" line)
            (setq time (date-to-time (substring line (length "commit:")))))
           ((string-empty-p line))
           (t (let ((file (expand-file-name line)))
                (unless (gethash file table)
                  (puthash file time table))))))
      (error (setq table nil)))
    table))

(defun denden--slugify-path (relative-path)
  "Return RELATIVE-PATH with each part made lowercase, safe for a URL.
Spaces and underscores become hyphens, other characters outside a to z,
0 to 9 and the hyphen are removed, and repeated hyphens are joined."
  (mapconcat
   (lambda (part)
     (let* ((text (downcase part))
            (text (replace-regexp-in-string (rx (+ (in " _"))) "-" text))
            (text (replace-regexp-in-string (rx (not (in "a-z0-9-"))) "" text))
            (text (replace-regexp-in-string (rx (>= 2 "-")) "-" text)))
       (string-trim text (rx (+ "-")) (rx (+ "-")))))
   (split-string relative-path "/")
   "/"))

(defun denden--page-url (relative-file)
  "Return the site URL of RELATIVE-FILE, an Org file under the content folder.
\"log/My Post.org\" gives \"log/my-post/\", \"log/_index.org\" gives
\"log/\", and \"index.org\" gives the empty string, the home page."
  (let* ((name (file-name-sans-extension relative-file))
         (directory (or (file-name-directory name) ""))
         (base (file-name-nondirectory name)))
    (if (member base '("index" "_index"))
        (denden--slugify-path directory)
      (concat (denden--slugify-path name) "/"))))

(defun denden--parse-date (text file)
  "Return the time value of the date TEXT, written in FILE.
TEXT is an Org timestamp such as \"[2024-02-03 Sat]\".  Signal an
error naming FILE when TEXT is not a real date, such as 2024-13-45."
  (let ((time (ignore-errors (date-to-time text)))
        (written (and (string-match
                       (rx (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
                       text)
                      (match-string 1 text))))
    (unless (and time written (equal written (format-time-string "%F" time)))
      (error "Denden: %s has the date %S, which is not a real date" file text))
    time))

(defun denden--buffer-keywords ()
  "Return the Org keywords of the current buffer as an alist.
Each entry is (NAME . VALUE), NAME in capitals.  A keyword written
more than once has its values joined with a space, as Org does for
#+title:.  Keywords inside blocks do not count."
  (let ((keywords nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-keyword-regexp nil t)
        (let ((element (org-element-at-point)))
          (when (org-element-type-p element 'keyword)
            (let* ((name (org-element-property :key element))
                   (value (org-element-property :value element))
                   (entry (assoc name keywords)))
              (if entry
                  (setcdr entry (concat (cdr entry) " " value))
                (push (cons name value) keywords)))))))
    (nreverse keywords)))

(defconst denden--page-keywords
  '("TITLE" "DATE" "DESCRIPTION" "LAYOUT" "DRAFT" "ALIASES")
  "Keywords that fill fields of a page record rather than its meta plist.")

(defun denden--true-p (value)
  "Return non-nil when the keyword VALUE means true, such as t or true."
  (and value (member (downcase (string-trim value)) '("t" "true" "yes"))))

(defun denden--meta-key (name)
  "Return the meta plist key for the keyword NAME, so NO_COMMENT gives :no-comment."
  (intern (concat ":" (string-replace "_" "-" (downcase name)))))

(defun denden--read-page (site file content-directory lastmod-table)
  "Return the page record of FILE, an Org file of SITE.
CONTENT-DIRECTORY is the absolute content folder.  LASTMOD-TABLE maps
files to their last commit time, or is nil."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (let* ((keywords (denden--buffer-keywords))
           (relative (file-relative-name file content-directory))
           (date-text (cdr (assoc "DATE" keywords)))
           (date (and date-text (denden--parse-date date-text file)))
           (taxonomies (denden-site-taxonomies site)))
      (denden-page-create
       :source file
       :text (buffer-string)
       :url (denden--page-url relative)
       :title (or (cdr (assoc "TITLE" keywords)) (file-name-base file))
       :date date
       :lastmod (or (and lastmod-table (gethash file lastmod-table)) date)
       :description (cdr (assoc "DESCRIPTION" keywords))
       :section (if (string-search "/" relative)
                    (car (split-string relative "/"))
                  "")
       :layout (cdr (assoc "LAYOUT" keywords))
       :draft (and (denden--true-p (cdr (assoc "DRAFT" keywords))) t)
       :terms (mapcar (lambda (taxonomy)
                        (cons taxonomy
                              (split-string (or (cdr (assoc (upcase taxonomy) keywords))
                                                ""))))
                      taxonomies)
       :aliases (split-string (or (cdr (assoc "ALIASES" keywords)) ""))
       :meta (cl-loop for (name . value) in keywords
                      unless (or (member name denden--page-keywords)
                                 (member (downcase name) taxonomies))
                      append (list (denden--meta-key name) value))))))

(defun denden--site-directory (site field)
  "Return the absolute folder of SITE named by FIELD.
FIELD is one of the symbols content, static or publishing.  The
folder is relative to the site's root directory."
  (expand-file-name
   (pcase field
     ('content (denden-site-content-directory site))
     ('static (denden-site-static-directory site))
     ('publishing (denden-site-publishing-directory site))
     (_ (error "Denden: unknown folder %S" field)))
   (or (denden-site-root-directory site) default-directory)))

(defun denden-collect-pages (site)
  "Return the pages of SITE, one for each Org file in its content folder.
Files and folders whose names start with a dot are skipped.  Pages
with #+draft: t are left out unless `denden-include-drafts' is
non-nil.  The pages come in the order of their file names."
  (let* ((content-directory (file-name-as-directory
                             (denden--site-directory site 'content)))
         (lastmod-table (and denden-lastmod-from-git
                             (denden--git-lastmod-table content-directory)))
         (files (seq-remove
                 (lambda (file)
                   (seq-some (lambda (part) (string-prefix-p "." part))
                             (split-string (file-relative-name file content-directory)
                                           "/")))
                 (directory-files-recursively content-directory
                                              (rx ".org" eos)))))
    (seq-remove (lambda (page)
                  (and (denden-page-draft page) (not denden-include-drafts)))
                (mapcar (lambda (file)
                          (denden--read-page site file content-directory lastmod-table))
                        (sort files)))))

;;;; Page lists

(defun denden-sort-pages-by-date (pages)
  "Return a copy of PAGES sorted by date, newest first.
Pages with no date come last.  Pages with the same date keep their
order."
  (sort pages :key (lambda (page) (or (denden-page-date page) 0))
        :lessp #'time-less-p :reverse t))

(defun denden-sort-pages-by-title (pages)
  "Return a copy of PAGES sorted by title, ignoring case."
  (sort pages :key (lambda (page) (downcase (denden-page-title page)))))

(defun denden-pages-in-section (pages section)
  "Return the PAGES whose section is SECTION, such as \"log\"."
  (seq-filter (lambda (page) (equal (denden-page-section page) section)) pages))

(defun denden-sections (pages)
  "Return the names of the sections of PAGES, each once, in order of first use.
Pages at the top of the content folder have no section and add none."
  (seq-uniq (seq-remove #'string-empty-p (mapcar #'denden-page-section pages))))

(defun denden-page-taxonomy-terms (page taxonomy)
  "Return the terms of PAGE in TAXONOMY, such as its tags."
  (cdr (assoc taxonomy (denden-page-terms page))))

(defun denden-pages-with-term (pages taxonomy term)
  "Return the PAGES that have TERM in TAXONOMY.
For example, (denden-pages-with-term pages \"tags\" \"emacs\")."
  (seq-filter (lambda (page)
                (member term (denden-page-taxonomy-terms page taxonomy)))
              pages))

(defun denden-term-counts (pages taxonomy)
  "Return how many PAGES use each term of TAXONOMY.
The result is a list of (TERM . COUNT), the most used first, and
terms with the same count in alphabetical order."
  (let ((counts nil))
    (dolist (page pages)
      (dolist (term (denden-page-taxonomy-terms page taxonomy))
        (cl-incf (alist-get term counts 0 nil #'equal))))
    (sort counts :key (lambda (entry) (list (- (cdr entry)) (car entry))))))

(defun denden-group-pages-by-year (pages)
  "Return PAGES grouped by the year of their date, newest year first.
The result is a list of (YEAR . PAGES), YEAR a string such as
\"2024\", and each group sorted newest first.  Pages with no date
are grouped under the empty string, last."
  (let ((groups nil))
    (dolist (page (denden-sort-pages-by-date pages))
      (let ((year (if (denden-page-date page)
                      (format-time-string "%Y" (denden-page-date page))
                    "")))
        (if (equal (caar groups) year)
            (push page (cdar groups))
          (push (list year page) groups))))
    (nreverse (mapcar (lambda (group) (cons (car group) (nreverse (cdr group))))
                      groups))))

(defun denden-backlinks (page pages)
  "Return the PAGES that link to PAGE, newest first.
Links are known once page bodies are exported, so call this after
the export step of a build."
  (denden-sort-pages-by-date
   (seq-filter (lambda (other)
                 (and (not (eq other page))
                      (member (denden-page-url page) (denden-page-links other))))
               pages)))

(defun denden-previous-and-next (page pages)
  "Return the pages of PAGES before and after PAGE in its section, by date.
The result is (PREVIOUS . NEXT): PREVIOUS is the next older page and
NEXT the next newer one.  Either is nil at the end of the section."
  (let* ((siblings (denden-sort-pages-by-date
                    (denden-pages-in-section pages (denden-page-section page))))
         (index (seq-position siblings page #'eq)))
    (when index
      (cons (nth (1+ index) siblings)
            (and (> index 0) (nth (1- index) siblings))))))

(defun denden-page-author (site page)
  "Return the author of PAGE: its #+author:, else the author of SITE."
  (or (plist-get (denden-page-meta page) :author) (denden-site-author site)))

(defun denden-page-language (site page)
  "Return the language of PAGE: its #+language:, else the language of SITE."
  (or (plist-get (denden-page-meta page) :language) (denden-site-language site)))

(defun denden-page-for-source (file pages)
  "Return the page of PAGES made from the Org FILE, or nil."
  (let ((file (expand-file-name file)))
    (seq-find (lambda (page) (equal (denden-page-source page) file)) pages)))

(defun denden-page-permalink (site page)
  "Return the full address of PAGE in SITE, such as \"https://example.org/log/x/\"."
  (concat (denden-base-url site) (denden-page-url page)))

;;;; Templates

;; A template is a Lisp list (TAG ATTRIBUTES . CHILDREN).  ATTRIBUTES is a
;; keyword plist such as (:class "note"), or an alist such as
;; ((class . "note")), the shape `dom.el' and `libxml-parse-html-region'
;; use.  A child is a string, another node, or nil, which is skipped so
;; that (when CONDITION NODE) can sit inside a template.  The node
;; (raw-html nil STRING) puts STRING in the page without escaping it.
;;
;; `dom-print' is not used because it writes <script src="a.js" />, which
;; is not valid HTML for elements that need a closing tag.

(defconst denden-void-elements
  '(area base br col embed hr img input link meta source track wbr)
  "HTML elements that have no closing tag and no children.")

(defun denden--attribute-alist (attributes)
  "Return ATTRIBUTES, a keyword plist or an alist, as an alist.
Signal an error that names ATTRIBUTES when its shape is wrong."
  (cond
   ((null attributes) nil)
   ((not (proper-list-p attributes))
    (error "Denden: attributes %S are a dotted pair, not a list of pairs"
           attributes))
   ((keywordp (car attributes))
    (unless (cl-evenp (length attributes))
      (error "Denden: attribute plist %S has an odd number of elements"
             attributes))
    (cl-loop for (key value) on attributes by #'cddr
             unless (keywordp key)
             do (error "Denden: attribute plist %S has the key %S, not a keyword"
                       attributes key)
             collect (cons (intern (substring (symbol-name key) 1)) value)))
   (t
    (dolist (pair attributes attributes)
      (unless (and (consp pair) (symbolp (car pair)) (not (keywordp (car pair))))
        (error "Denden: attribute alist %S has the entry %S, not (NAME . VALUE)"
               attributes pair))))))

(defun denden--serialize-attribute (pair)
  "Return the HTML text of the attribute PAIR, (NAME . VALUE).
A nil VALUE leaves the attribute out and returns nil.  The value t
writes NAME alone, as a boolean attribute such as hidden."
  (pcase-let ((`(,name . ,value) pair))
    (cond
     ((null value) nil)
     ((eq value t) (symbol-name name))
     (t (format "%s=\"%s\"" name
                (url-insert-entities-in-string (format "%s" value)))))))

(defun denden-serialize-node (node)
  "Return the HTML text of NODE, a template node or a string.
A string is escaped.  The node (raw-html nil STRING ...) returns its
strings as they are.  Void elements such as img are written as
<img />; every other element gets a closing tag.

For example:

  (denden-serialize-node \\='(p (:class \"note\") \"Hello\" (br nil)))
  => \"<p class=\\\"note\\\">Hello<br /></p>\""
  (cond
   ((stringp node) (url-insert-entities-in-string node))
   ((eq (car node) 'raw-html) (apply #'concat (cddr node)))
   (t
    (pcase-let* ((`(,tag ,attributes . ,children) node)
                 (attribute-text
                  (mapconcat (lambda (pair) (concat " " pair))
                             (delq nil (mapcar #'denden--serialize-attribute
                                               (denden--attribute-alist attributes)))
                             ""))
                 (children (delq nil children)))
      (if (memq tag denden-void-elements)
          (if children
              (error "Denden: void element %s was given children %S" tag children)
            (format "<%s%s />" tag attribute-text))
        (format "<%s%s>%s</%s>" tag attribute-text
                (mapconcat #'denden-serialize-node children "")
                tag))))))

;;;; Export backend

;; `denden-html' is Org's `html' backend with a few transcoders replaced.
;; Its parse tree filter gives every heading a stable id before ox-html
;; writes it, so the table of contents and links agree with the headings.  The link
;; transcoder resolves links to other pages of the site, shows their
;; current title, and records each page's links for backlinks.

(defun denden-heading-id (text)
  "Return a stable id for a heading with TEXT, such as \"getting-started\"."
  (let* ((id (downcase text))
         (id (replace-regexp-in-string (rx (+ (not (in "a-z0-9")))) "-" id))
         (id (string-trim id (rx (+ "-")) (rx (+ "-")))))
    (if (string-empty-p id) "section" id)))

(defun denden--prepare-tree (tree _backend _info)
  "Give each heading of TREE a unique id.
A heading keeps its CUSTOM_ID or ID property when it has one; other
headings get an id from their text, with -2, -3 and so on added when
two headings share it."
  (let ((seen (make-hash-table :test #'equal)))
    (org-element-map tree '(headline inlinetask)
      (lambda (headline)
        (let* ((id (or (org-element-property :CUSTOM_ID headline)
                       (org-element-property :ID headline)
                       (denden-heading-id (org-element-property :raw-value headline))))
               (count (cl-incf (gethash id seen 0))))
          (org-element-put-property headline :CUSTOM_ID
                                    (if (= count 1) id (format "%s-%d" id count)))))))
  tree)

(defun denden-headline (headline contents info)
  "Transcode HEADLINE with CONTENTS and INFO, adding a link to its id.
The link sits at the end of the heading, so a reader can copy the
address of a section.  See `denden-heading-anchor-text'."
  (let* ((html (org-html-headline headline contents info))
         (level (+ (org-export-get-relative-level headline info)
                   (1- (plist-get info :html-toplevel-hlevel))))
         (closing (format "</h%d>" level))
         (position (string-search closing html)))
    (if (or (not denden-heading-anchor-text)
            (org-export-low-level-p headline info)
            (> level 6)
            (not position))
        html
      (concat (substring html 0 position)
              (denden-serialize-node
               `(a (:class "heading-anchor"
                    :href ,(concat "#" (org-element-property :CUSTOM_ID headline))
                    :aria-label ,denden-heading-anchor-label)
                   ,denden-heading-anchor-text))
              (substring html position)))))

(defun denden-src-block (src-block contents info)
  "Transcode SRC-BLOCK with CONTENTS and INFO, coloured as chosen.
`denden-code-highlighting' says how code is coloured.  The block
takes keyboard focus, so a reader can scroll a wide block without a
mouse."
  (let ((org-html-htmlize-output-type denden-code-highlighting)
        (org-html-htmlize-font-prefix "org-"))
    (string-replace "<pre " "<pre tabindex=\"0\" "
                    (org-html-src-block src-block contents info))))

(defun denden-link-label (site url)
  "Return URL as short text for a link that has no description.
The text is the host without \"www.\" and the path without a final
slash.  Then each (REGEXP . REPLACEMENT) of the link label rules of
SITE is applied in order, such as a rule that turns
\"github.com/user\" into \"github:user\"."
  (let* ((parsed (url-generic-parse-url url))
         (host (string-remove-prefix "www." (or (url-host parsed) "")))
         (target (let ((target (url-target parsed)))
                   ;; A "#:~:text=" fragment only tells the browser what to
                   ;; highlight, so it is not part of the label.
                   (and target (substring target 0 (string-search ":~:" target)))))
         (label (decode-coding-string
                 (url-unhex-string
                  (concat host
                          (string-remove-suffix "/" (or (url-filename parsed) ""))
                          (and target (not (string-empty-p target))
                               (concat "#" target))))
                 'utf-8)))
    (dolist (rule (denden-site-link-label-rules site) label)
      (setq label (replace-regexp-in-string (car rule) (cdr rule) label t)))))

(defun denden--link-target (path pages)
  "Return the target of PATH, the path of an internal link, as a site URL.
PATH is either a site path such as /log/x/ or a relative Org file
such as other.org.  When it names a page of PAGES, return that page;
otherwise return the URL it names, which the build checks later."
  (if (string-prefix-p "/" path)
      (let ((url (string-remove-prefix "/" (string-remove-suffix ".html" path))))
        (or (seq-find (lambda (page)
                        (equal (string-remove-suffix "/" (denden-page-url page))
                               (string-remove-suffix "/" url)))
                      pages)
            url))
    ;; The export binds `default-directory' to the folder of the text.
    (or (denden-page-for-source (expand-file-name path) pages)
        path)))

(defun denden--record-external-link (link page)
  "Add LINK, a link to another site, to the external links of PAGE."
  (let* ((url (org-element-property :raw-link link))
         (text (and (org-element-contents link)
                    (string-trim (org-no-properties
                                  (org-link-display-format
                                   (org-element-interpret-data
                                    (org-element-contents link))))))))
    (unless (assoc url (denden-page-external-links page))
      (push (cons url (and text (not (string-empty-p text)) text))
            (denden-page-external-links page)))))

(defun denden-link (link contents info)
  "Transcode LINK with CONTENTS and INFO, resolving links between pages.
A link to another page, written as [[file:other.org]] or
[[/log/other/]], points to that page's URL and shows its current
title, unless its description has markup.  Every internal link is
recorded in the page, so the build can check it and find backlinks.
A bare URL to another site shows `denden-link-label' as its text."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (page (plist-get info :denden-page))
         (target (and (equal type "file")
                      (or (string-prefix-p "/" path) (string-suffix-p ".org" path))
                      (denden--link-target path (plist-get info :denden-pages)))))
    (when (and page target)
      (cl-pushnew (if (denden-page-p target) (denden-page-url target) target)
                  (denden-page-links page) :test #'equal))
    (when (and page (member type '("http" "https")))
      (denden--record-external-link link page))
    (if (denden-page-p target)
        (format "<a href=\"/%s\">%s</a>"
                (denden-page-url target)
                (if (and contents (string-search "<" contents))
                    contents
                  (org-html-encode-plain-text (denden-page-title target))))
      (let ((bare (and (member type '("http" "https"))
                       (or (null contents)
                           (equal (string-trim contents)
                                  (org-element-property :raw-link link))))))
        (denden--lazy-image
         ;; ox-html writes a site path such as /images/a.png as file:///images/a.png.
         (string-replace
          "\"file:///" "\"/"
          (org-html-link link
                         (if bare
                             (org-html-encode-plain-text
                              (denden-link-label (plist-get info :denden-site)
                                                 (org-element-property :raw-link link)))
                           contents)
                         info)))))))

(defun denden--lazy-image (html)
  "Return HTML with lazy loading added when it is a single img tag.
The browser then loads the image only when it comes near the screen.
An img that sets loading itself, through #+attr_html:, is kept.  See
`denden-lazy-images'."
  (if (and denden-lazy-images
           (string-prefix-p "<img " html)
           (string-suffix-p ">" html)
           (not (string-search " loading=" html)))
      (concat (string-remove-suffix ">" html) " loading=\"lazy\" decoding=\"async\">")
    html))

(defun denden-paragraph (paragraph contents info)
  "Transcode PARAGRAPH with CONTENTS and INFO, with figure numbers as chosen.
See `denden-figure-numbers'."
  (let ((html (org-html-paragraph paragraph contents info)))
    (if denden-figure-numbers
        html
      (replace-regexp-in-string
       (rx "<span class=\"figure-number\">" (+ (not (in "<"))) "</span>")
       "" html t t))))

(defun denden-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK with CONTENTS and INFO, showing callouts.
A block whose type is one of the site's callout types, such as
#+begin_note, becomes a labelled note; any other block is written
as ox-html writes it."
  (let* ((type (downcase (org-element-property :type special-block)))
         (label (cdr (assoc type (denden-site-callout-types
                                  (plist-get info :denden-site))))))
    (if label
        (format (concat "<div class=\"callout\" role=\"note\" data-callout=\"%s\">\n"
                        "<p>%s</p>\n%s</div>")
                type label (or contents ""))
      (org-html-special-block special-block contents info))))

(org-export-define-derived-backend 'denden-html 'html
  :translate-alist '((headline . denden-headline)
                     (link . denden-link)
                     (paragraph . denden-paragraph)
                     (special-block . denden-special-block)
                     (src-block . denden-src-block))
  :filters-alist '((:filter-parse-tree . denden--prepare-tree)))

(defun denden--export-options (site page pages)
  "Return the export options for PAGE of SITE, given all PAGES.
PAGE may be nil when exporting a fragment."
  (list :with-toc nil
        :section-numbers nil
        :headline-levels 6
        ;; Org's own links to headings: mark a broken one, or stop.
        :with-broken-links (and (not (eq denden-broken-link-action 'error)) 'mark)
        :html-doctype "html5"
        :html-html5-fancy t
        :html-checkbox-type 'html
        :denden-site site
        :denden-page page
        :denden-pages pages))

(defvar denden-current-site nil
  "The site whose page is being exported, or nil outside an export.
Code blocks in a page can read it, with `denden-current-page' and
`denden-current-pages', to show data from the site, for example:

  #+begin_src emacs-lisp :exports results :results value html
    (denden-serialize-node
     (denden-render-page-list
      (seq-take (denden-sort-pages-by-date denden-current-pages) 3)))
  #+end_src

These variables are bound only while a page exports.")

(defvar denden-current-page nil
  "The page being exported, or nil.  See `denden-current-site'.")

(defvar denden-current-pages nil
  "All pages of the site being exported, or nil.  See `denden-current-site'.")

(defun denden--export-text (text directory options)
  "Return the body HTML of the Org TEXT, exported with OPTIONS.
DIRECTORY is the folder relative links and #+INCLUDE: files are
read from.  While it exports, `denden-current-site' and its two
companions hold the site, the page and the pages from OPTIONS."
  (with-temp-buffer
    (insert text)
    (let* ((default-directory (file-name-as-directory directory))
           (site (plist-get options :denden-site))
           (denden-current-site site)
           (denden-current-page (plist-get options :denden-page))
           (denden-current-pages (plist-get options :denden-pages))
           (org-export-use-babel (denden-site-evaluate-code site))
           (org-confirm-babel-evaluate (not (denden-site-evaluate-code site))))
      (delay-mode-hooks (org-mode))
      (org-export-as 'denden-html nil nil t options))))

(defun denden-export-page (site page pages)
  "Export the body of PAGE of SITE to HTML, and store it in PAGE.
PAGES are all pages of the site, used to resolve links.  The export
also stores the links of PAGE, for `denden-backlinks'.  Return the
body HTML."
  (setf (denden-page-links page) nil
        (denden-page-external-links page) nil)
  (let ((body (denden--export-text (denden-page-text page)
                                   (file-name-directory (denden-page-source page))
                                   (denden--export-options site page pages))))
    (setf (denden-page-links page) (nreverse (denden-page-links page))
          (denden-page-external-links page) (nreverse (denden-page-external-links page))
          (denden-page-body page) body)))

(defconst denden--fragment-boundary "<!-- denden fragment boundary -->"
  "HTML comment that `denden-export-org-strings' puts between fragments.")

(defun denden-export-org-strings (site texts pages &optional page)
  "Return the HTML of each of TEXTS, pieces of Org written for SITE.
All TEXTS are exported together, which is much faster than one export
each, so use this for many short pieces, such as the descriptions in
a list.  Each piece must be a paragraph or less; a heading in one
would take in the pieces after it.  PAGES and PAGE are as in
`denden-export-org-string'."
  (let* ((separator (concat "\n\n#+html: " denden--fragment-boundary "\n\n"))
         (html (denden-export-org-string site (string-join texts separator) pages page)))
    (mapcar #'string-trim (split-string html (rx (literal denden--fragment-boundary))))))

(defun denden-export-org-string (site text pages &optional page)
  "Return the HTML of TEXT, a piece of Org written for SITE.
PAGES are all pages of the site, used to resolve links.  When PAGE is
given, relative links in TEXT are read from its folder.  Links in
TEXT are not added to any page."
  (denden--export-text text
                       (if page
                           (file-name-directory (denden-page-source page))
                         (denden--site-directory site 'content))
                       (denden--export-options site nil pages)))

;;;; Pages as HTML

(defun denden-date-node (time)
  "Return a time node that shows TIME as `denden-date-format' says.
It is marked dt-published, the published date of an h-entry."
  `(time (:class "dt-published" :datetime ,(format-time-string "%F" time))
         ,(format-time-string denden-date-format time)))

(defun denden-section-index-p (page)
  "Return non-nil when PAGE is the index page of its section, such as log/."
  (let ((section (denden-page-section page)))
    (and (not (string-empty-p section))
         (equal (denden-page-url page) (concat section "/")))))

(defun denden-term-url (taxonomy term)
  "Return the URL of the page of TERM in TAXONOMY, such as \"tags/emacs/\"."
  (concat (denden--slugify-path taxonomy) "/" (denden--slugify-path term) "/"))

(defun denden-render-terms (site page)
  "Return the terms of PAGE in the taxonomies of SITE, as category links.
Each term links to its page when SITE has `denden-term-outputs';
otherwise the terms are plain text.  The result is nil when PAGE has
no terms."
  (let* ((linked (memq #'denden-term-outputs (denden-site-outputs site)))
         (terms (mapcan (lambda (taxonomy)
                          (mapcar (lambda (term)
                                    (if linked
                                        `(a (:class "p-category"
                                             :href ,(concat "/" (denden-term-url taxonomy term)))
                                            ,(concat "#" term))
                                      `(span (:class "p-category") ,(concat "#" term))))
                                  (denden-page-taxonomy-terms page taxonomy)))
                        (denden-site-taxonomies site))))
    (and terms `(p (:class "terms") ,@(cdr (mapcan (lambda (term) (list " " term)) terms))))))

(defun denden-render-page-list (pages)
  "Return a list of PAGES, newest first, each with its date and title.
It is marked up as an h-feed of h-entries (see microformats.org)."
  `(ul (:class "h-feed")
       ,@(mapcar (lambda (page)
                   `(li (:class "h-entry")
                        ,(and (denden-page-date page)
                              (denden-date-node (denden-page-date page)))
                        " "
                        (a (:class "u-url p-name"
                            :href ,(concat "/" (denden-page-url page)))
                           ,(denden-page-title page))))
                 (denden-sort-pages-by-date pages))))

(defun denden-render-plain-page (site page pages)
  "Return the main part of PAGE of SITE: title, subtitle, date, terms, body.
This is the default layout of a site.  It is marked up as an h-entry
\(see microformats.org), so feed readers and IndieWeb tools can read
it.  On the index page of a section, such as log/ from
log/_index.org, the body is followed by the list of the section's
other pages from PAGES."
  (let ((date (denden-page-date page)))
    `(article (:class "h-entry")
              (h1 (:class "p-name") ,(denden-page-title page))
              ,(let ((subtitle (plist-get (denden-page-meta page) :subtitle)))
                 (and subtitle `(p (:class "subtitle") ,subtitle)))
              ,(and date `(p nil ,(denden-date-node date)))
              ,(denden-render-terms site page)
              (div (:class "e-content")
                   (raw-html nil ,(or (denden-page-body page) "")))
              ,(and (denden-section-index-p page)
                    (denden-render-page-list
                     (remove page (denden-pages-in-section
                                   pages (denden-page-section page))))))))

(defun denden--meta-description (page)
  "Return the excerpt of PAGE on one line, at most 160 characters, or nil.
Search engines show about that much."
  (let ((excerpt (denden-page-excerpt page)))
    (and excerpt
         (let ((text (string-trim
                      (replace-regexp-in-string (rx (+ (in " \t\n\r"))) " " excerpt))))
           (if (> (length text) 160) (concat (substring text 0 159) "…") text)))))

(defun denden-seo-head-nodes (site page &optional image-url)
  "Return the head nodes that describe PAGE of SITE to search engines.
They are the meta description (the excerpt, at most 160
characters) and author, the canonical address, Open Graph and
Twitter tags, and for a dated page a JSON-LD BlogPosting.
IMAGE-URL, when given, is the full address of the page's preview
image."
  (let* ((url (denden-page-permalink site page))
         (title (denden-page-title page))
         (description (denden--meta-description page))
         (date (denden-page-date page))
         (published (and date (format-time-string "%F" date)))
         (modified (and (denden-page-lastmod page)
                        (format-time-string "%F" (denden-page-lastmod page))))
         (tags (denden-page-taxonomy-terms page "tags"))
         (author (denden-page-author site page)))
    (append
     (and description `((meta (:name "description" :content ,description))))
     (and (not (string-empty-p author)) `((meta (:name "author" :content ,author))))
     `((link (:rel "canonical" :href ,url))
       (meta (:property "og:title" :content ,title))
       (meta (:property "og:type" :content ,(if date "article" "website")))
       (meta (:property "og:url" :content ,url))
       (meta (:property "og:site_name" :content ,(denden-site-title site)))
       (meta (:property "og:locale" :content ,(denden-site-language site))))
     (and description `((meta (:property "og:description" :content ,description))))
     (and published `((meta (:property "article:published_time" :content ,published))))
     (and published modified
          `((meta (:property "article:modified_time" :content ,modified))))
     (and published
          (mapcar (lambda (tag) `(meta (:property "article:tag" :content ,tag))) tags))
     (and image-url
          `((meta (:property "og:image" :content ,image-url))
            (meta (:property "og:image:alt" :content ,title))
            (meta (:name "twitter:image" :content ,image-url))))
     `((meta (:name "twitter:card"
              :content ,(if image-url "summary_large_image" "summary"))))
     (and published
          `((script (:type "application/ld+json")
                    (raw-html
                     nil
                     ,(json-serialize
                       `((@context . "https://schema.org")
                         (@type . "BlogPosting")
                         (headline . ,title)
                         (datePublished . ,published)
                         ,@(and modified `((dateModified . ,modified)))
                         ,@(and description `((description . ,description)))
                         ,@(and (not (string-empty-p author))
                                `((author . ((@type . "Person") (name . ,author)))
                                  (publisher . ((@type . "Person") (name . ,author)))))
                         (mainEntityOfPage . ((@type . "WebPage") (@id . ,url)))
                         (url . ,url)
                         ,@(and image-url `((image . [,image-url])))
                         ,@(and tags `((keywords . ,(string-join tags ", ")))))))))))))

(defun denden-default-page-shell (site page main _pages)
  "Return the whole page node of PAGE of SITE around MAIN, its main part.
The head has the character set, the viewport, the title,
`denden-seo-head-nodes', the site's style sheets and, when the site
has a feed, its link.  The header links home and shows the site's
menu.  The footer names the author as an h-card, links the feed, and
ends with the nodes of the site's footer function, if it has one.
The markup is plain and semantic, so a classless style sheet styles
it well.  _PAGES is not used here; a site's own shell may use it."
  `(html (:lang ,(denden-page-language site page))
         (head nil
               (meta (:charset "utf-8"))
               (meta (:name "viewport" :content "width=device-width, initial-scale=1"))
               (title nil ,(if (string-empty-p (denden-page-url page))
                               (denden-site-title site)
                             (format denden-page-title-format
                                     (denden-page-title page) (denden-site-title site))))
               ,@(denden-seo-head-nodes site page)
               ,(and (memq #'denden-feed-output (denden-site-outputs site))
                     `(link (:rel "alternate" :type "application/atom+xml"
                             :title ,(denden-site-title site)
                             :href ,(concat "/" denden-feed-url))))
               ,@(mapcar (lambda (url) `(link (:rel "stylesheet" :href ,url)))
                         (denden-site-stylesheets site)))
         (body nil
               (header nil
                       (a (:href "/") ,(denden-site-title site))
                       ,(and (denden-site-menu site)
                             `(nav nil ,@(mapcar (lambda (item)
                                                   `(a (:href ,(cdr item)) ,(car item)))
                                                 (denden-site-menu site)))))
               (main nil ,main)
               (footer nil
                       ,(and (not (string-empty-p (denden-site-author site)))
                             `(p (:class "h-card")
                                 (a (:class "p-name u-url" :href "/")
                                    ,(denden-site-author site))))
                       ,(and (memq #'denden-feed-output (denden-site-outputs site))
                             `(p nil (a (:href ,(concat "/" denden-feed-url)) "Feed")))
                       ,@(and (denden-site-footer site)
                              (funcall (denden-site-footer site) site page))))))

(defun denden-render-page (site page pages)
  "Return the full HTML of PAGE of SITE, given all PAGES.
The page's layout, or the site's default layout, gives the main
part; the site's page shell wraps it.  Export the page body with
`denden-export-page' first."
  (let* ((layout (or (cdr (assoc (denden-page-layout page) (denden-site-layouts site)))
                     (denden-site-default-layout site)))
         (main (funcall layout site page pages)))
    (concat "<!doctype html>\n"
            (denden-serialize-node
             (funcall (denden-site-page-shell site) site page main pages)))))

(defun denden-render-site (site pages)
  "Return every file of SITE except static files, as a list of (URL . TEXT).
PAGES are the collected pages.  Every body is exported first, so
backlinks are complete when the pages are rendered.  Then each page
is rendered, and each function in the site's outputs adds its files.
A page's URL ends in a slash, such as \"log/x/\"; it is written as
log/x/index.html.  Two files with the same URL stop the build."
  (dolist (page pages)
    (denden-export-page site page pages))
  (let ((files (append
                (mapcar (lambda (page)
                          (cons (denden-page-url page)
                                (denden-render-page site page pages)))
                        pages)
                (mapcan (lambda (output) (funcall output site pages))
                        (denden-site-outputs site)))))
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (file files files)
        (when (gethash (car file) seen)
          (error "Denden: two files have the same URL %S" (car file)))
        (puthash (car file) t seen)))))

;;;; Outputs

;; Outputs are the files a site has besides its pages: feeds, the
;; sitemap, robots.txt, redirects and a search index.  Each function
;; ending in -output or -outputs takes the site and its pages and returns
;; a list of (URL . TEXT); list the ones a site wants in its :outputs.
;; The functions they use, such as `denden-atom-feed', return text, so a
;; site can build its own outputs from them.

(defun denden--parse-html (html)
  "Return the DOM of the HTML text, as `libxml-parse-html-region' gives it."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun denden--dom-text (node)
  "Return the text of the DOM NODE, with runs of white space made one space."
  (string-trim (replace-regexp-in-string (rx (+ (in " \t\n\r"))) " "
                                         (string-join (dom-strings node)))))

(defun denden-html-plain-text (html)
  "Return the text of HTML, without tags and with spaces joined.
Heading anchors (see `denden-heading-anchor-text') are left out."
  (let ((dom (denden--parse-html html)))
    (dolist (anchor (dom-by-class dom (rx bos "heading-anchor" eos)))
      (dom-remove-node dom anchor))
    (denden--dom-text dom)))

(defun denden--prose-paragraphs (node)
  "Return the p elements under NODE in order, leaving out figures and callouts.
A figure holds an image and its caption, and a callout a label and
a note, which are not the prose of the page."
  (cond
   ((not (consp node)) nil)
   ((eq (dom-tag node) 'figure) nil)
   ((equal (dom-attr node 'class) "callout") nil)
   ((eq (dom-tag node) 'p) (list node))
   (t (mapcan #'denden--prose-paragraphs (dom-children node)))))

(defun denden-page-excerpt (page)
  "Return a short description of PAGE as plain text.
It is the page's #+description:, or else its first paragraphs (see
`denden-excerpt-paragraphs'), separated by a blank line.  Export the
page first; a page with no body and no description gives nil."
  (or (denden-page-description page)
      (let ((body (denden-page-body page)))
        (and body
             (let* ((paragraphs (seq-take (denden--prose-paragraphs
                                           (denden--parse-html body))
                                          denden-excerpt-paragraphs))
                    (text (mapconcat #'denden--dom-text paragraphs "\n\n")))
               (and (not (string-empty-p text)) text))))))

(defun denden--feed-time (time)
  "Return TIME as an Atom date, midnight UTC of its day.
Denden's dates are days, so the time of day is always midnight."
  (format-time-string "%FT00:00:00Z" time))

(defun denden--atom-entry (site page)
  "Return the Atom entry node of PAGE of SITE.
A page with its own #+author: names that author in its entry."
  (let ((url (denden-page-permalink site page)))
    `(entry nil
            (title nil ,(denden-page-title page))
            (link (:rel "alternate" :type "text/html" :href ,url))
            (id nil ,url)
            ,(and (denden-page-date page)
                  `(published nil ,(denden--feed-time (denden-page-date page))))
            (updated nil ,(denden--feed-time (or (denden-page-lastmod page)
                                                 (denden-page-date page))))
            ,(let ((author (plist-get (denden-page-meta page) :author)))
               (and author `(author nil (name nil ,author))))
            ,@(mapcar (lambda (tag)
                        `(category (:term ,(downcase tag) :label ,tag
                                    :scheme ,(concat (denden-base-url site) "tags/"))))
                      (denden-page-taxonomy-terms page "tags"))
            ,(let ((summary (denden-page-excerpt page)))
               (and summary `(summary nil ,summary)))
            (content (:type "html") ,(or (denden-page-body page) "")))))

(cl-defun denden-atom-feed (site pages &key title url self-url icon rights)
  "Return an Atom feed of SITE with an entry for each of PAGES.
PAGES are written in the order given; sort them first, for example
with `denden-sort-pages-by-date'.  The keys are optional: TITLE
\(the site title by default), URL (the page the feed belongs to, the
site's address by default), SELF-URL (the feed's own address), ICON
\(the address of an icon) and RIGHTS (a copyright line)."
  (let ((url (or url (denden-base-url site))))
    (concat
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
     (denden-serialize-node
      `(feed (:xmlns "http://www.w3.org/2005/Atom")
             (title nil ,(or title (denden-site-title site)))
             ,(and (not (string-empty-p (denden-site-description site)))
                   `(subtitle nil ,(denden-site-description site)))
             (link (:rel "alternate" :type "text/html" :href ,url))
             ,(and self-url
                   `(link (:rel "self" :type "application/atom+xml" :href ,self-url)))
             (id nil ,(or self-url url))
             ,(and pages
                   `(updated nil ,(denden--feed-time
                                   (car (sort (mapcar (lambda (page)
                                                        (or (denden-page-lastmod page)
                                                            (denden-page-date page) 0))
                                                      pages)
                                              :lessp #'time-less-p :reverse t)))))
             ,(and (not (string-empty-p (denden-site-author site)))
                   `(author nil (name nil ,(denden-site-author site))))
             (generator nil "Denden")
             ,(and icon `(icon nil ,icon))
             ,(and rights `(rights nil ,rights))
             ,@(mapcar (lambda (page) (denden--atom-entry site page)) pages)))
     "\n")))

(defun denden-feed-output (site pages)
  "Return the site feed of SITE as one (URL . TEXT), in a list.
The feed has the dated pages of PAGES, newest first, at most the
site's feed entry count."
  (let* ((dated (denden-sort-pages-by-date (seq-filter #'denden-page-date pages)))
         (count (denden-site-feed-entry-count site)))
    (list (cons denden-feed-url
                (denden-atom-feed site (if count (seq-take dated count) dated)
                                  :self-url (concat (denden-base-url site)
                                                    denden-feed-url))))))

(defun denden-term-outputs (site pages)
  "Return the pages of the taxonomies of SITE, as (URL . TEXT).
For each taxonomy with terms in PAGES, such as tags, there is a page
that lists its terms with their counts, at tags/, and a page per
term that lists its pages, newest first, at tags/emacs/.  They go
through the site's page shell and default layout, like other pages."
  (let ((generated nil))
    (dolist (taxonomy (denden-site-taxonomies site))
      (let ((counts (denden-term-counts pages taxonomy)))
        (when counts
          (push (denden-page-create
                 :url (concat (denden--slugify-path taxonomy) "/")
                 :title (capitalize taxonomy)
                 :body (denden-serialize-node
                        `(ul (:class "terms")
                             ,@(mapcar (lambda (entry)
                                         `(li nil
                                              (a (:href ,(concat "/" (denden-term-url taxonomy (car entry))))
                                                 ,(concat "#" (car entry)))
                                              ,(format " (%d)" (cdr entry))))
                                       (sort counts :key #'car)))))
                generated)
          (pcase-dolist (`(,term . ,_count) counts)
            (push (denden-page-create
                   :url (denden-term-url taxonomy term)
                   :title (concat "#" term)
                   :body (denden-serialize-node
                          (denden-render-page-list (denden-pages-with-term pages taxonomy term))))
                  generated)))))
    (mapcar (lambda (page) (cons (denden-page-url page) (denden-render-page site page pages)))
            (nreverse generated))))

(defun denden-sitemap-xml (site pages)
  "Return a sitemap of PAGES of SITE, for search engines."
  (concat
   "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\n"
   (denden-serialize-node
    `(urlset (:xmlns "http://www.sitemaps.org/schemas/sitemap/0.9")
             ,@(mapcar (lambda (page)
                         `(url nil
                               (loc nil ,(denden-page-permalink site page))
                               ,(and (denden-page-lastmod page)
                                     `(lastmod nil ,(denden--feed-time
                                                     (denden-page-lastmod page))))))
                       pages)))
   "\n"))

(defun denden-sitemap-output (site pages)
  "Return the sitemap of SITE for PAGES as one (URL . TEXT), in a list."
  (list (cons "sitemap.xml" (denden-sitemap-xml site pages))))

(defun denden-robots-txt (site)
  "Return a robots.txt for SITE that allows everything and names the sitemap."
  (format "User-agent: *\nAllow: /\n\nSitemap: %ssitemap.xml\n" (denden-base-url site)))

(defun denden-robots-output (site _pages)
  "Return the robots.txt of SITE as one (URL . TEXT), in a list.
_PAGES is not used."
  (list (cons "robots.txt" (denden-robots-txt site))))

(defun denden-redirect-html (site page)
  "Return a page that sends the reader on to PAGE of SITE.
Search engines are told the real address and not to index this one.
The page also has a plain link, for readers whose browser does not
follow the redirect."
  (let ((url (denden-page-permalink site page))
        (path (concat "/" (denden-page-url page))))
    (concat
     "<!doctype html>\n"
     (denden-serialize-node
      `(html (:lang ,(denden-site-language site))
             (head nil
                   (meta (:charset "utf-8"))
                   (title nil ,(denden-page-title page))
                   (link (:rel "canonical" :href ,url))
                   (meta (:name "robots" :content "noindex"))
                   (meta (:http-equiv "refresh" :content ,(concat "0; url=" path))))
             (body nil
                   (p nil (a (:href ,path) ,(denden-page-title page)))))))))

(defun denden-redirect-outputs (site pages)
  "Return a redirect page of SITE for each alias of PAGES, as (URL . TEXT).
An alias such as \"posts/first\" is written as posts/first/index.html."
  (mapcan (lambda (page)
            (mapcar (lambda (alias)
                      (cons (file-name-as-directory (string-trim alias "/" "/"))
                            (denden-redirect-html site page)))
                    (denden-page-aliases page)))
          pages))

(defun denden--sentences (text)
  "Return the sentences of TEXT, each ending in its . ! or ?.
Sentences of three characters or fewer are left out."
  (let ((sentences nil)
        (start 0))
    (while (string-match (rx (in ".!?") (+ (in " \t\n"))) text start)
      (push (substring text start (1+ (match-beginning 0))) sentences)
      (setq start (match-end 0)))
    (push (substring text start) sentences)
    (seq-filter (lambda (sentence) (> (length sentence) 3))
                (mapcar #'string-trim (nreverse sentences)))))

(defun denden-search-index-json (pages)
  "Return a search index of PAGES as JSON text.
It is an array with one object per sentence: its page title, the
page URL from the site root, and the sentence."
  (json-serialize
   (vconcat
    (mapcan (lambda (page)
              (mapcar (lambda (sentence)
                        `((title . ,(denden-page-title page))
                          (url . ,(concat "/" (denden-page-url page)))
                          (text . ,sentence)))
                      (denden--sentences
                       (denden-html-plain-text (or (denden-page-body page) "")))))
            pages))))

(defun denden-search-index-output (_site pages)
  "Return the search index of PAGES as one (URL . TEXT), in a list.
_SITE is not used."
  (list (cons "search-index.json" (denden-search-index-json pages))))

;;;; Build

;; A build is always full and clean.  It writes the whole site into a new
;; folder next to the publishing folder, and only when every file is
;; written does it replace the old site.  A failed build leaves the old
;; site as it was.

(defun denden--url-file (url)
  "Return the file name, relative to the site, that URL is written to.
A URL that ends in a slash, or is empty, names a folder, so its
file is index.html in that folder."
  (if (or (string-empty-p url) (string-suffix-p "/" url))
      (concat url "index.html")
    url))

(defun denden--check-publishing-directory (site directory)
  "Signal an error unless DIRECTORY is safe for SITE to replace.
A build deletes the old publishing folder, so it must not be the
site's root folder, hold the content or static folder, or sit inside
one of them."
  (dolist (field '(content static))
    (let ((protected (file-name-as-directory (denden--site-directory site field)))
          (publishing (file-name-as-directory directory)))
      (when (or (string-prefix-p publishing protected)
                (string-prefix-p protected publishing))
        (error "Denden: the publishing folder %s overlaps the %s folder %s"
               directory field protected))))
  (when (file-equal-p directory (denden-site-root-directory site))
    (error "Denden: the publishing folder %s is the site's root folder" directory)))

(defun denden--written-urls (directory)
  "Return a hash table of every URL a reader can open in DIRECTORY.
Each file counts by its own name, and a folder's index.html also
counts as the folder, with and without the final slash."
  (let ((urls (make-hash-table :test #'equal)))
    (dolist (file (directory-files-recursively directory ""))
      (let ((relative (file-relative-name file directory)))
        (puthash relative t urls)
        (when (equal (file-name-nondirectory relative) "index.html")
          (let ((folder (string-remove-suffix "index.html" relative)))
            (puthash folder t urls)
            (puthash (string-remove-suffix "/" folder) t urls)))))
    urls))

(defun denden--check-links (pages directory)
  "Check that every internal link of PAGES names a file in DIRECTORY.
Each link to nothing is reported as `denden-broken-link-action' says:
a message, or one error that lists them all."
  (let ((urls (denden--written-urls directory))
        (problems nil))
    (dolist (page pages)
      (dolist (link (denden-page-links page))
        (let ((url (car (split-string link (rx (in "#?"))))))
          (unless (gethash url urls)
            (push (format "%s links to %s, which the build did not write"
                          (denden-page-source page) link)
                  problems)))))
    (setq problems (nreverse problems))
    (when problems
      (if (eq denden-broken-link-action 'error)
          (error "Denden: broken links:\n%s" (string-join problems "\n"))
        (dolist (problem problems)
          (message "Denden: %s" problem))))))

(defun denden--write-site (site files directory)
  "Write the static folder of SITE, then FILES, into DIRECTORY.
FILES is a list of (URL . TEXT).  DIRECTORY must not exist yet.  A
file of FILES may replace a static file of the same name, such as a
site's own version of a file that came with a theme; the build says
so in a message."
  (let ((static (denden--site-directory site 'static)))
    (make-directory directory t)
    (when (file-directory-p static)
      (copy-directory static (file-name-as-directory directory) nil t t))
    (pcase-dolist (`(,url . ,text) files)
      (let ((file (expand-file-name (denden--url-file url) directory)))
        (when (file-exists-p file)
          (message "Denden: %s replaces the static file of the same name" url))
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region text nil file nil 'silent))))))

(defun denden--read-site ()
  "Ask for one of `denden-sites' and return it.
When only one site is defined, return it without asking."
  (pcase denden-sites
    ('nil (user-error "No site is defined; see `denden-define-site'"))
    (`((,_name . ,site)) site)
    (_ (alist-get (intern (completing-read "Build site: "
                                           (mapcar #'car denden-sites) nil t))
                  denden-sites))))

;;;###autoload
(defun denden-build (site)
  "Build SITE: write every page, output and static file into a clean folder.
The new site is written next to the publishing folder, its internal
links are checked, and only then does it replace the old site.  A
failed build, or a broken link when `denden-broken-link-action' is
`error', leaves the old site as it was.
Interactively, ask for a site defined with `denden-define-site',
or use the only one.  Return the publishing folder."
  (interactive (list (denden--read-site)))
  (let* ((start (float-time))
         (directory (directory-file-name (denden--site-directory site 'publishing)))
         (new-directory (concat directory ".new"))
         (old-directory (concat directory ".old")))
    (denden--check-publishing-directory site directory)
    (dolist (leftover (list new-directory old-directory))
      (when (file-exists-p leftover)
        (delete-directory leftover t)))
    (let* ((pages (denden-collect-pages site))
           (files (denden-render-site site pages)))
      (condition-case problem
          (progn
            (denden--write-site site files new-directory)
            (denden--check-links pages new-directory))
        (error
         (when (file-exists-p new-directory)
           (delete-directory new-directory t))
         (signal (car problem) (cdr problem))))
      (when (file-exists-p directory)
        (rename-file directory old-directory))
      (rename-file new-directory directory)
      (when (file-exists-p old-directory)
        (delete-directory old-directory t))
      (message "Denden: built %d pages and %d other files into %s in %.1f s"
               (length pages) (- (length files) (length pages)) directory
               (- (float-time) start))
      directory)))

(provide 'denden)
;;; denden.el ends here

;;; denden.el --- core static-site library on top of ox-publish -*- lexical-binding: t; -*-

;; Core static-site library: project definition and config, file collection,
;; the derived ox-html backend, the sexp-HTML serializer, template
;; resolution, incremental build and the async driver, Atom feeds, sitemap,
;; robots, aliases and redirects, taxonomy collection, listing and grouping
;; helpers, the output linter, search index generation, OG card mechanism,
;; and the user-facing M-x command surface. A site layer supplies its own
;; templates, content, and site-specific rendering on top of this.
;;
;; A site layer is expected to set: `denden-content-directory',
;; `denden-publishing-directory', `denden-project-root-directory',
;; `denden-full-build-function' and `denden-build-function'. Optionally:
;; `denden-post-directory' (used only by `denden-new'). Everything below
;; that reads one of those variables works for any site, not just one
;; in particular.
;;
;;; Commentary:
;;
;; denden name is derived from japanese term 'でん' for snail
;;
;; Tried to not build yet another package, but based on ox-publish and all native libs
;; just added helper commands that completes for enhancements. Following same suite as VOMPECC.
;;
;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'ox-html)
(require 'ox-publish)
(require 'filenotify)

(defgroup denden nil
  "Emacs/Org static site builder."
  :group 'applications)

(defconst denden-repository-directory
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Repo root, captured from this file's own location at load time.")

;;;; Sexp-HTML serializer
;;
;; denden templates are written as keyword plists, for example:
;;
;;   (div (:class "menu-bar" :role "navigation")
;;     (a (:href "/") "home"))
;;
;; `denden-normalise-node' turns the keyword-plist attribute form into the
;; canonical dom.el node shape (TAG ATTRIBUTES . CHILDREN), where ATTRIBUTES
;; is an alist of (SYMBOL . VALUE). Canonical nodes are what
;; `denden-serialize-node' turns into an HTML string, and what
;; `libxml-parse-html-region' hands back when parsing HTML, so the same node
;; shape flows through templates, the serializer, and the differ and linter.
;;
;; We do not use `dom-print', `esxml-to-xml' or `shr-dom-to-xml': all three
;; get void and non-void elements wrong for HTML. `denden-serialize-node' is
;; the fix: void elements are always emitted self-closing, everything else
;; always gets an explicit closing tag, never a self-closing one.

(defconst denden-void-elements
  '(area base br col embed hr img input link meta source track wbr)
  "HTML elements with no closing tag or children; serialized self-closing,
like <br />.")


(defun denden--keyword-to-attribute-symbol (keyword)
  "Turn attribute KEYWORD such as :class into the plain symbol class."
  (intern (substring (symbol-name keyword) 1)))

(defun denden-normalise-attributes (attribute-spec)
  "Convert ATTRIBUTE-SPEC, an alist or a keyword plist, to a canonical
alist of (SYMBOL . VALUE)."
  (cond
   ((null attribute-spec) nil)
   ((not (proper-list-p attribute-spec))
    (error "Denden: malformed attribute spec %S is a dotted pair, not a list of pairs -- did you forget to wrap it in a list?"
           attribute-spec))
   ((keywordp (car attribute-spec))
    (unless (cl-evenp (length attribute-spec))
      (error "Denden: attribute plist %S has an odd number of elements"
             attribute-spec))
    (cl-loop for (key value) on attribute-spec by #'cddr
             do (unless (keywordp key)
                  (error "Denden: attribute plist %S mixes keyword and non-keyword keys at %S"
                         attribute-spec key))
             collect (cons (denden--keyword-to-attribute-symbol key) value)))
   (t
    (dolist (pair attribute-spec)
      (unless (and (consp pair) (symbolp (car pair)) (not (keywordp (car pair))))
        (error "Denden: attribute alist %S contains a malformed entry %S"
               attribute-spec pair)))
    attribute-spec)))

(defun denden-normalise-node (node)
  "Normalise template-form NODE, (TAG ATTRIBUTE-SPEC . CHILDREN), to dom.el's
(TAG ATTRIBUTE-ALIST . CHILDREN) shape."
  (if (stringp node)
      node
    (cl-destructuring-bind (tag attribute-spec . children) node
      (append (list tag (denden-normalise-attributes attribute-spec))
              (mapcar #'denden-normalise-node children)))))

(defun denden--escape-text (text)
  "Escape TEXT for use as HTML character data."
  (let ((text (replace-regexp-in-string "&" "&amp;" text)))
    (setq text (replace-regexp-in-string "<" "&lt;" text))
    (setq text (replace-regexp-in-string ">" "&gt;" text))
    text))

(defun denden--escape-attribute-value (value)
  "Escape VALUE for use inside a double-quoted HTML attribute."
  (let ((text (replace-regexp-in-string "&" "&amp;" value)))
    (setq text (replace-regexp-in-string "\"" "&quot;" text))
    (setq text (replace-regexp-in-string "<" "&lt;" text))
    text))

(defun denden--serialize-attribute (pair)
  "Serialize one canonical (NAME . VALUE) attribute PAIR. A nil VALUE omits
it; t emits a bare boolean attribute."
  (let ((name (symbol-name (car pair)))
        (value (cdr pair)))
    (cond
     ((null value) nil)
     ((eq value t) name)
     (t (format "%s=\"%s\"" name (denden--escape-attribute-value (format "%s" value)))))))

(defun denden--serialize-attributes (attributes)
  "Serialize canonical ATTRIBUTES alist to a leading-space-joined string."
  (let ((parts (delq nil (mapcar #'denden--serialize-attribute attributes))))
    (if parts (concat " " (string-join parts " ")) "")))

(defun denden-serialize-node (node)
  "Serialize canonical dom.el NODE to an HTML string. A `raw-html' node emits
its string verbatim, unescaped."
  (cond
   ((stringp node) (denden--escape-text node))
   ((eq (car node) 'raw-html) (mapconcat #'identity (cddr node) ""))
   (t
    (cl-destructuring-bind (tag attributes . children) node
      (let ((tag-name (symbol-name tag))
            (attribute-string (denden--serialize-attributes attributes)))
        (if (memq tag denden-void-elements)
            (progn
              (when children
                (error "Denden: void element %s was given children %S"
                       tag-name children))
              (format "<%s%s />" tag-name attribute-string))
          (format "<%s%s>%s</%s>"
                  tag-name attribute-string
                  (mapconcat #'denden-serialize-node children "")
                  tag-name)))))))

;;;; One-shot HTML to template-form sexp converter
;;
;; A one-shot tool: convert an existing rendered HTML file into template-form
;; sexp so it can be hand-edited into a denden template. `libxml-parse-
;; html-region' hands back dom.el nodes with plain symbol tags and an alist
;; of (symbol . string) attributes already; `denden-html-to-template'
;; rewrites that attribute alist as a keyword plist to match the
;; template-form convention and drops whitespace-only text-node children.

(defun denden--attribute-symbol-to-keyword (symbol)
  "Turn a plain attribute SYMBOL such as class into the keyword :class."
  (intern (concat ":" (symbol-name symbol))))

(defun denden--whitespace-only-string-p (text)
  "Return non-nil if TEXT is a string containing only whitespace."
  (and (stringp text) (string-match-p "\\`[ \t\n\r]*\\'" text)))

(defun denden-html-to-template (node)
  "Convert libxml dom.el NODE into denden template-form sexp, dropping
whitespace-only text nodes."
  (cond
   ((denden--whitespace-only-string-p node) nil)
   ((stringp node) node)
   (t (cl-destructuring-bind (tag attributes . children) node
        (let ((plist (cl-loop for (name . value) in attributes
                               append (list (denden--attribute-symbol-to-keyword name) value)))
              (kept-children (delq nil (mapcar #'denden-html-to-template children))))
          (append (list tag plist) kept-children))))))

(defun denden-convert-html-string (html-string)
  "Parse HTML-STRING and return its top-level node as template-form sexp."
  (with-temp-buffer
    (insert html-string)
    (denden-html-to-template
     (libxml-parse-html-region (point-min) (point-max)))))

(defun denden-convert-html-file (file)
  "Read FILE and return its contents as template-form sexp, for pasting into a
template."
  (with-temp-buffer
    (insert-file-contents file)
    (denden-html-to-template
     (libxml-parse-html-region (point-min) (point-max)))))

;;;; Derived ox-html backend
;;
;; Derives `denden-html' from `html' (`org-export-define-derived-backend'),
;; overriding the transcoders headline, src-block, item, link and paragraph.
;;
;; Stable heading ids (honour :CUSTOM_ID: or :ID: when present, slugify the
;; heading text otherwise, dedupe collisions with a numeric suffix) are NOT
;; implemented by forking `org-html-headline'. That function is only one of
;; roughly twenty call sites in ox-html.el that all resolve ids through the
;; shared helper `org-html--reference' -- the table of contents and internal
;; link resolution both call it directly, bypassing the headline transcoder
;; entirely. `denden--advise-html-reference' fixes this once, for every
;; consumer, by advising the shared helper instead.
;;
;; src-block binds `org-html-htmlize-output-type' to `css' for the duration,
;; so highlighted code always comes out as CSS classes, never inline
;; style="" colors. htmlize derives those classes from font-lock face
;; names: `font-lock-keyword-face' strips the "font-lock-" prefix and the
;; "-face" suffix, then gets `org-html-htmlize-font-prefix' ("org-" by
;; default) applied, so it becomes .org-keyword. `denden-highlight-
;; stylesheet' below turns a class-to-color alist into the CSS that gives
;; those classes meaning; the site layer supplies the alist that points them
;; at its own color tokens.

(defvar denden--heading-ids nil
  "Hash table of heading ids already used in the current export, reset per
file.")


(defun denden--reset-heading-ids (backend)
  "Clear `denden--heading-ids' before an export with a denden-html BACKEND."
  (when (org-export-derived-backend-p backend 'denden-html)
    (setq denden--heading-ids (make-hash-table :test 'equal))))

(add-hook 'org-export-before-parsing-functions #'denden--reset-heading-ids)

(defun denden--slugify-heading-text (text)
  "Slugify TEXT into a lowercase, hyphen-separated id, or \"section\" if
nothing alphanumeric survives."
  (let* ((slug (downcase text))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
         (slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug)))
    (if (string-empty-p slug) "section" slug)))

(defun denden--unique-heading-id (candidate)
  "Return CANDIDATE, or CANDIDATE with a numeric suffix if it repeats."
  (unless denden--heading-ids
    (setq denden--heading-ids (make-hash-table :test 'equal)))
  (let ((seen-count (gethash candidate denden--heading-ids 0)))
    (puthash candidate (1+ seen-count) denden--heading-ids)
    (if (zerop seen-count) candidate (format "%s-%d" candidate (1+ seen-count)))))

(defun denden-heading-id (datum)
  "DATUM's stable HTML id: its :CUSTOM_ID or :ID if present, else a deduped
slug. Caches the result on DATUM."
  (or (org-element-property :denden-id datum)
      (let* ((custom-id (org-element-property :CUSTOM_ID datum))
             (id-property (org-element-property :ID datum))
             (id (cond
                  (custom-id (denden--unique-heading-id custom-id))
                  (id-property (denden--unique-heading-id id-property))
                  (t (denden--unique-heading-id
                      (denden--slugify-heading-text
                       (org-element-property :raw-value datum)))))))
        (org-element-put-property datum :denden-id id)
        id)))

(defun denden--advise-html-reference (original-function datum info &optional named-only)
  "Redirect headline and inlinetask id computation to `denden-heading-id',
within a denden-html export only."
  (if (and (memq (org-element-type datum) '(headline inlinetask))
           (org-export-derived-backend-p (plist-get info :back-end) 'denden-html))
      (denden-heading-id datum)
    (funcall original-function datum info named-only)))

(advice-add 'org-html--reference :around #'denden--advise-html-reference)

(defun denden-headline (headline contents info)
  "Transcode HEADLINE like `org-html-headline'; id logic lives in the
`org-html--reference' advice instead."
  (org-html-headline headline contents info))

(defun denden-src-block (src-block contents info)
  "Transcode SRC-BLOCK like `org-html-src-block', but always as CSS classes,
never inline style colors."
  (let ((org-html-htmlize-output-type 'css)
        (org-html-htmlize-font-prefix "org-"))
    (org-html-src-block src-block contents info)))

(defun denden-highlight-stylesheet (class-color-alist)
  "Return a CSS stylesheet string from CLASS-COLOR-ALIST, entries of
(CLASS-NAME . COLOR-VALUE)."
  (mapconcat
   (lambda (entry) (format ".%s { color: %s; }" (car entry) (cdr entry)))
   class-color-alist
   "\n"))

(defun denden-item (item contents info)
  "Transcode ITEM like `org-html-item'. A placeholder for a future inline
list-item convention."
  (org-html-item item contents info))

(defun denden--resolve-site-path (raw-path all-pages)
  "Resolve RAW-PATH, a bare site-root [[/path]] link, against ALL-PAGES's :url
values. Return the matching page or nil."
  (let ((key (string-trim-left
              (string-trim-right (string-trim-right raw-path "\\.html\\'") "/")
              "/")))
    (seq-find (lambda (p) (equal (string-trim-right (plist-get p :url) "/") key)) all-pages)))

(defun denden--strip-file-uri-prefix (html)
  "HTML with any \"file://\" prefix removed from a site-root-absolute href or
src."
  (replace-regexp-in-string "\\(href\\|src\\)=\"file://\\(/[^\"]*\\)\"" "\\1=\"\\2\"" html))

(defun denden--escape-attribute-ampersands (html)
  "HTML with a bare & inside any href or src attribute value escaped to &amp;."
  (replace-regexp-in-string
   "\\(href\\|src\\)=\"\\([^\"]*\\)\""
   (lambda (whole)
     (string-match "\\(href\\|src\\)=\"\\([^\"]*\\)\"" whole)
     (format "%s=\"%s\"" (match-string 1 whole)
             (replace-regexp-in-string
              "&\\(amp;\\|lt;\\|gt;\\|quot;\\|#[0-9]+;\\|#x[0-9a-fA-F]+;\\)?"
              (lambda (entity) (if (> (length entity) 1) entity "&amp;"))
              (match-string 2 whole))))
   html))

(defun denden--fix-link-html (html)
  "HTML with both `denden--strip-file-uri-prefix' and
`denden--escape-attribute-ampersands' applied."
  (denden--escape-attribute-ampersands (denden--strip-file-uri-prefix html)))

(defun denden-link (link contents info)
  "Transcode LINK like `org-html-link', but a resolvable [[/path]] link uses
the target page's live title as text."
  (let ((type (org-element-property :type link))
        (raw-path (org-element-property :path link))
        (all-pages (plist-get info :denden-all-pages)))
    (if (and all-pages (equal type "file") (string-prefix-p "/" raw-path)
             (not (and contents (string-match-p "<" contents))))
        (let ((target (denden--resolve-site-path raw-path all-pages)))
          (if target
              (format "<a href=\"%s\">%s</a>" raw-path (org-html-encode-plain-text (plist-get target :title)))
            (denden--fix-link-html (org-html-link link contents info))))
      (denden--fix-link-html (org-html-link link contents info)))))

(defconst denden-callout-types '("note" "tip" "warn")
  "Special-block types `denden-special-block' renders as a styled callout div.
Any other type passes through unchanged.")


(defun denden-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK as a callout div when TYPE is in
`denden-callout-types', else like `org-html-special-block'."
  (let ((type (downcase (org-element-property :type special-block))))
    (if (member type denden-callout-types)
        (format "<div class=\"callout callout-%s\">\n%s</div>" type (or contents ""))
      (org-html-special-block special-block contents info))))

(defconst denden-html-default-options
  '(:with-broken-links mark :with-toc nil :section-numbers nil)
  "Ext-plist every denden-html export call should pass; turns off ox-html's
default table of contents and section numbers.")


(defun denden-export-options-with-pages (all-pages)
  "`denden-html-default-options' plus :denden-all-pages ALL-PAGES, for
`denden-link's [[/path]] resolution."
  (append denden-html-default-options (list :denden-all-pages all-pages)))

(defun denden-paragraph (paragraph contents info)
  "Transcode PARAGRAPH like `org-html-paragraph', dropping the \"Figure N:\"
caption prefix and fixing image src."
  (denden--fix-link-html
   (replace-regexp-in-string
    "<span class=\"figure-number\">Figure [0-9]+: *</span>"
    ""
    (org-html-paragraph paragraph contents info))))

(org-export-define-derived-backend 'denden-html 'html
  :translate-alist
  '((headline . denden-headline)
    (item . denden-item)
    (link . denden-link)
    (paragraph . denden-paragraph)
    (special-block . denden-special-block)
    (src-block . denden-src-block)))

;;;; Build driver on top of ox-publish
;;
;; Wires ox-publish's own project/publish/cache machinery to the
;; denden-html backend, and adds the one thing ox-publish has no equivalent
;; for: a sweep step that deletes output files a build no longer writes.
;; Async building reuses `org-export-async-start' directly rather than
;; `org-publish-all', so the sweep can run inside the child process too,
;; after publishing finishes there.

(setq org-export-async-init-file
      (expand-file-name "denden-async-init.el" denden-repository-directory))

(defun denden-publish-to-html (plist filename _pub-dir)
  "Publish FILENAME with the denden-html backend to its pretty-URL path, with
no site chrome."
  (let ((output (denden-output-file-for filename (cons nil plist))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert (with-temp-buffer
                (insert-file-contents filename)
                (org-mode)
                (org-export-as 'denden-html nil nil nil plist))))
    output))

(defun denden-slugify-path (relative-path)
  "RELATIVE-PATH with every \"/\"-separated component slugified to lowercase
[a-z0-9-], Hugo-style."
  (mapconcat
   (lambda (component)
     (let* ((lower (downcase component))
            (hyphenated (replace-regexp-in-string "[ _]+" "-" lower))
            (stripped (replace-regexp-in-string "[^a-z0-9-]" "" hyphenated))
            (collapsed (replace-regexp-in-string "-\\{2,\\}" "-" stripped)))
       (string-trim collapsed "-+" "-+")))
   (split-string relative-path "/") "/"))

(defun denden--pretty-output-relative-path (slugged)
  "Map SLUGGED, an already-slugified relative path, to its pretty-URL output
shape."
  (if (equal (file-name-nondirectory slugged) "index")
      slugged
    (concat (file-name-as-directory slugged) "index")))

(defun denden-output-file-for (source-file project)
  "Return the absolute output path SOURCE-FILE maps to under PROJECT. Custom
publishing functions must call this, not `org-export-output-file-name'."
  (let* ((base-dir (file-name-as-directory (org-publish-property :base-directory project)))
         (pub-dir (file-name-as-directory (org-publish-property :publishing-directory project)))
         (relative (file-relative-name source-file base-dir))
         (output-extension (org-publish-property :denden-output-extension project))
         (no-ext (file-name-sans-extension relative))
         ;; An _index.org names a section/site index, mapping to that
         ;; directory's plain index.html, not a literal _index.html.
         (index-fixed (if (equal (file-name-nondirectory no-ext) "_index")
                           (concat (or (file-name-directory no-ext) "") "index")
                         no-ext))
         (slugged (denden-slugify-path index-fixed))
         (pretty (denden--pretty-output-relative-path slugged)))
    (expand-file-name (if output-extension (concat pretty output-extension) relative) pub-dir)))

(defun denden-pretty-url (output-relative-path)
  "Render OUTPUT-RELATIVE-PATH as a pretty-URL href: its directory portion,
trailing slash kept."
  (string-remove-suffix "index.html" output-relative-path))


(defun denden-expected-output-files (leaf-projects)
  "Return the output paths LEAF-PROJECTS should produce, one per source file."
  (let (files)
    (dolist (project leaf-projects)
      (dolist (source (org-publish-get-base-files project))
        (push (denden-output-file-for source project) files)))
    files))

(defun denden-sweep-output-directory (pub-dir expected-files)
  "Delete every file under PUB-DIR absent from EXPECTED-FILES, and any
directory left empty by it."
  (let ((expected (mapcar #'expand-file-name expected-files)))
    (dolist (file (directory-files-recursively pub-dir ".*" nil))
      (unless (member (expand-file-name file) expected)
        (delete-file file)))
    (dolist (dir (sort (directory-files-recursively pub-dir ".*" t)
                        (lambda (a b) (> (length a) (length b)))))
      (when (and (file-directory-p dir)
                 (null (directory-files dir nil directory-files-no-dot-files-regexp)))
        (delete-directory dir)))))

(defun denden-sweep-projects (leaf-projects)
  "Sweep every :publishing-directory LEAF-PROJECTS writes to."
  (let ((by-dir (make-hash-table :test 'equal)))
    (dolist (leaf leaf-projects)
      (let ((dir (file-name-as-directory (org-publish-property :publishing-directory leaf))))
        (puthash dir (cons leaf (gethash dir by-dir)) by-dir)))
    (maphash
     (lambda (dir leaves)
       (when (file-directory-p dir)
         (denden-sweep-output-directory dir (denden-expected-output-files leaves))))
     by-dir)))

(defun denden-build-project (project &optional force)
  "Build PROJECT synchronously, then sweep its output. With FORCE, ignore
ox-publish's timestamp cache."
  (let ((leaves (org-publish-expand-projects (list project)))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (save-window-excursion
      (let ((org-publish-use-timestamps-flag (not force)))
        (when force (org-publish-remove-all-timestamps))
        (org-publish-projects leaves)))
    (denden-sweep-projects leaves)))

(defun denden-build-project-async (project &optional force)
  "Build PROJECT in a separate Emacs process, then sweep there. Returns as
soon as the child process starts."
  (let ((leaves (org-publish-expand-projects (list project))))
    (org-export-async-start
        (lambda (_) nil)
      `(let ((org-publish-use-timestamps-flag ,(not force)))
         (when ',force (org-publish-remove-all-timestamps))
         (org-publish-projects ',leaves)
         (denden-sweep-projects ',leaves)))))

(defun denden-git-lastmod (file)
  "Return FILE's last commit date as \"YYYY-MM-DD\", or nil if it has no git
history."
  (let ((default-directory (file-name-directory file)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "log" "-1" "--format=%cs"
                                  "--" (file-name-nondirectory file)))
        (let ((output (string-trim (buffer-string))))
          (unless (string-empty-p output) output))))))

(defun denden--org-date-to-iso (date-string)
  "Convert org timestamp DATE-STRING, e.g. \"[2024-02-03 Sat]\", to
\"2024-02-03\". Nil in, nil out."
  (and date-string (format-time-string "%Y-%m-%d" (org-time-string-to-time date-string))))

(defun denden-format-iso-date (iso-date format-string)
  "Render ISO-DATE (\"YYYY-MM-DD\") via FORMAT-STRING (`format-time-string'
syntax). Returns \"\" if ISO-DATE is nil."
  (if (and iso-date (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" iso-date))
      (format-time-string format-string
                          (encode-time 0 0 0 (string-to-number (match-string 3 iso-date))
                                       (string-to-number (match-string 2 iso-date))
                                       (string-to-number (match-string 1 iso-date))))
    ""))

(defun denden--relative-path-section (relative-path)
  "Return RELATIVE-PATH's top-level directory, or \"\" if it has none."
  (if (string-match-p "/" relative-path) (car (split-string relative-path "/")) ""))

(defun denden--source-section (source base-dir)
  "Return SOURCE's top-level directory relative to BASE-DIR, or \"\" when
SOURCE sits directly in it."
  (denden--relative-path-section (file-relative-name source base-dir)))

(defun denden-file-keyword-true-p (file keyword)
  "Return non-nil if FILE's #+KEYWORD: value is \"t\" or \"true\" (case
insensitive)."
  (let ((value (cadr (assoc keyword (with-temp-buffer
                                       (insert-file-contents file)
                                       (org-mode)
                                       (org-collect-keywords (list keyword)))))))
    (and value (member (downcase value) '("t" "true")))))

(defun denden-file-is-draft-p (file)
  "Return non-nil if FILE's #+draft: keyword is true."
  (denden-file-keyword-true-p file "DRAFT"))

(defun denden-draft-exclude-regexp (base-directory)
  "An org-publish :exclude regexp matching every #+draft: t file under
BASE-DIRECTORY, or nil if there are none."
  (let ((drafts (seq-filter #'denden-file-is-draft-p
                            (directory-files-recursively base-directory "\\.org\\'"))))
    (when drafts
      (mapconcat (lambda (file) (regexp-quote (file-relative-name file base-directory)))
                 drafts "\\|"))))

(defun denden-sort-pages-by-date-desc (pages)
  "Return PAGES sorted by :date descending; a page with no :date sorts last."
  (sort (copy-sequence pages)
        (lambda (a b) (string> (or (plist-get a :date) "") (or (plist-get b :date) "")))))

(defun denden-sort-pages-by-title (pages)
  "Return PAGES sorted by :title ascending, case-insensitively."
  (sort (copy-sequence pages)
        (lambda (a b) (string< (downcase (plist-get a :title)) (downcase (plist-get b :title))))))

(defun denden-page-lastmod (page)
  "Return PAGE's Lastmod: `denden-git-lastmod' on its :source, or its :date."
  (or (denden-git-lastmod (plist-get page :source)) (plist-get page :date)))

(defun denden-html-word-count (html)
  "Return HTML's word count: strip tags, then split on whitespace."
  (let ((text (with-temp-buffer
                (insert html)
                (goto-char (point-min))
                (while (re-search-forward "<[^>]+>" nil t) (replace-match " "))
                (buffer-string))))
    (length (split-string text nil t))))

(defun denden--file-word-count (file)
  "Return the word count of FILE's exported body."
  (denden-html-word-count
   (with-temp-buffer
     (insert-file-contents file)
     (org-mode)
     (org-export-as 'denden-html nil nil t denden-html-default-options))))

(defun denden-collect-page-metadata (leaf-projects)
  "Return a metadata plist for every page LEAF-PROJECTS publish, one per
source file."
  (let (pages)
    (dolist (project leaf-projects)
      (when (org-publish-property :denden-output-extension project)
        (let ((base-dir (file-name-as-directory (org-publish-property :base-directory project)))
              (pub-dir (file-name-as-directory (org-publish-property :publishing-directory project))))
          (dolist (source (org-publish-get-base-files project))
            (let* ((keywords (with-temp-buffer
                               (insert-file-contents source)
                               (org-mode)
                               (org-collect-keywords '("TITLE" "TAGS[]" "REFS[]" "DATE" "ALIASES[]"))))
                   (output (denden-output-file-for source project))
                   (word-count (denden--file-word-count source)))
              (push (list :url (denden-pretty-url (file-relative-name output pub-dir))
                          :title (or (cadr (assoc "TITLE" keywords)) (file-name-base source))
                          :tags (split-string (or (cadr (assoc "TAGS[]" keywords)) ""))
                          :refs (split-string (or (cadr (assoc "REFS[]" keywords)) ""))
                          :date (denden--org-date-to-iso (cadr (assoc "DATE" keywords)))
                          :section (denden--source-section source base-dir)
                          :source source
                          :aliases (split-string (or (cadr (assoc "ALIASES[]" keywords)) ""))
                          :wordcount word-count
                          :readingtime (max 1 (round (/ word-count 200.0))))
                    pages))))))
    (nreverse pages)))

(defun denden-pages-in-section (pages section)
  "Return only the PAGES whose :section is SECTION."
  (seq-filter (lambda (page) (equal (plist-get page :section) section)) pages))

(defun denden-pages-with-tag (pages tag)
  "Return only the PAGES whose :tags include TAG."
  (seq-filter (lambda (page) (member tag (plist-get page :tags))) pages))

(defun denden-group-pages-by-year (pages)
  "Group page-metadata PAGES by the year of :date, newest year and page first.
A page with no :date groups under \"\"."
  (let* ((sorted (denden-sort-pages-by-date-desc pages))
         (groups nil))
    (dolist (page sorted)
      (let ((year (if (plist-get page :date) (substring (plist-get page :date) 0 4) "")))
        (if (and groups (equal (caar groups) year))
            (setcdr (car groups) (cons page (cdar groups)))
          (push (cons year (list page)) groups))))
    (mapcar (lambda (group) (cons (car group) (nreverse (cdr group)))) (nreverse groups))))

(defun denden--taxonomy-counts (pages field)
  "Return (:tag :count) plists for every value across PAGES's FIELD (:tags or
:refs), sorted by count then name."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (page pages)
      (dolist (value (plist-get page field))
        (puthash value (1+ (gethash value counts 0)) counts)))
    (let (result)
      (maphash (lambda (value count) (push (list :tag value :count count) result)) counts)
      (sort result (lambda (a b)
                     (if (= (plist-get a :count) (plist-get b :count))
                         (string< (plist-get a :tag) (plist-get b :tag))
                       (> (plist-get a :count) (plist-get b :count))))))))

(defun denden-tag-counts (pages)
  "Return (:tag :count) plists for every tag across PAGES's :tags, sorted by
count then name."
  (denden--taxonomy-counts pages :tags))

(defun denden-ref-counts (pages)
  "Return (:tag :count) plists for every value across PAGES's :refs, same
shape as `denden-tag-counts'."
  (denden--taxonomy-counts pages :refs))

(defun denden-collect-body-links (body-html)
  "Return deduped (:href :text :internal) plists for http(s):// and /-prefixed
links in BODY-HTML."
  (let ((dom (with-temp-buffer (insert body-html) (libxml-parse-html-region (point-min) (point-max))))
        (seen (make-hash-table :test 'equal))
        (refs nil))
    (dolist (a (dom-by-tag dom 'a))
      (let ((href (dom-attr a 'href)))
        (when (and href
                   (or (string-prefix-p "http://" href) (string-prefix-p "https://" href)
                       (string-prefix-p "/" href))
                   (not (gethash href seen)))
          (puthash href t seen)
          (push (list :href href
                      :text (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (dom-texts a)))
                      :internal (string-prefix-p "/" href))
                refs))))
    (nreverse refs)))

(defun denden-pages-with-ref (pages ref)
  "Return only the PAGES whose :refs include REF."
  (seq-filter (lambda (page) (member ref (plist-get page :refs))) pages))

(defun denden--item-link (item)
  "Return the link inside ITEM's :tag secondary string, or nil."
  (car (org-element-map (org-element-property :tag item) 'link #'identity)))

(defun denden--link-text (link)
  "Return LINK's bracket description as raw text, or its raw-link if none."
  (let ((begin (org-element-property :contents-begin link))
        (end (org-element-property :contents-end link)))
    (if (and begin end) (string-trim (buffer-substring-no-properties begin end))
      (org-element-property :raw-link link))))

(defun denden-export-org-fragment (text)
  "Export TEXT, a raw Org inline fragment, to HTML via denden-html, with the
wrapping <p> stripped."
  (if (string-empty-p text)
      text
    (let ((html (string-trim (org-export-string-as text 'denden-html t denden-html-default-options))))
      (if (and (string-prefix-p "<p>" html) (string-suffix-p "</p>" html))
          (substring html 3 -4)
        html))))

(defun denden-parse-topic-item (item)
  "Return (:url :name :desc :parts) for descriptive-list ITEM, split on \" |
\" in its body."
  (let* ((link (denden--item-link item))
         (begin (org-element-property :contents-begin item))
         (end (org-element-property :contents-end item))
         (raw (if (and begin end) (string-trim (buffer-substring-no-properties begin end)) ""))
         (segments (mapcar #'string-trim (split-string raw " | "))))
    (list :url (if link (org-element-property :raw-link link) "")
          :name (if link (denden--link-text link) "")
          :desc (denden-export-org-fragment (or (car segments) ""))
          :parts (cdr segments))))

(defun denden--topic-list-group (headline)
  "Build one (:label :items) group plist from HEADLINE."
  (list :label (org-element-property :raw-value headline)
        :items (seq-mapcat
                (lambda (plain-list)
                  (if (eq (org-element-property :type plain-list) 'descriptive)
                      (mapcar #'denden-parse-topic-item (org-element-map plain-list 'item #'identity))
                    nil))
                (org-element-map (org-element-contents headline) 'plain-list #'identity
                                  nil nil 'headline))))

(defun denden-parse-topic-list (file)
  "Parse FILE (a #+layout: topic-list content file) into (:label :items)
groups, one per top-level heading."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'headline #'denden--topic-list-group nil nil 'headline)))

(defun denden-topic-list-intro-html (file)
  "Return the exported body of FILE up to, but not including, its first H2
section."
  (let* ((body-html (with-temp-buffer
                       (insert-file-contents file)
                       (org-mode)
                       (org-export-as 'denden-html nil nil t denden-html-default-options)))
         (split-point (string-match "<div id=\"outline-container-" body-html)))
    (if split-point (substring body-html 0 split-point) body-html)))

;;;; Structural page differ
;;
;; Compares normalised skeletons -- heading tree, link targets, image
;; sources, landmark roles, visible text -- ignoring ids, class names and
;; whitespace. Written for the Hugo-vs-denden parity sweep; kept as a
;; general-purpose comparison tool, exposed via `denden-diff'.

(defun denden--normalise-whitespace (text)
  "Return TEXT with whitespace runs collapsed to single spaces and trimmed."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text)))

(defun denden--page-skeleton (dom)
  "Return a normalised skeleton plist for DOM: headings, links, images,
landmarks, and visible text."
  (list
   :headings (mapcar (lambda (h) (cons (dom-tag h) (denden--normalise-whitespace (dom-texts h))))
                      (dom-search dom (lambda (el) (string-match-p "\\`h[1-6]\\'" (symbol-name (dom-tag el))))))
   :links (mapcar (lambda (a) (dom-attr a 'href)) (dom-by-tag dom 'a))
   :images (mapcar (lambda (img) (dom-attr img 'src)) (dom-by-tag dom 'img))
   :landmarks (sort (mapcar (lambda (el) (symbol-name (dom-tag el)))
                            (dom-search dom (lambda (el) (memq (dom-tag el) '(nav main header footer aside)))))
                     #'string-lessp)
   :text (denden--normalise-whitespace (dom-texts dom))))

(defun denden--parse-html-file (file)
  "Parse FILE's contents as HTML and return the dom.el tree."
  (with-temp-buffer
    (insert-file-contents file)
    (libxml-parse-html-region (point-min) (point-max))))

(defun denden-diff-pages (old-file new-file)
  "Compare OLD-FILE and NEW-FILE's normalised skeletons. Return nil if they
match, else the differing fields."
  (let ((old (denden--page-skeleton (denden--parse-html-file old-file)))
        (new (denden--page-skeleton (denden--parse-html-file new-file)))
        (diffs nil))
    (dolist (field '(:headings :links :images :landmarks :text))
      (let ((old-value (plist-get old field))
            (new-value (plist-get new field)))
        (unless (equal old-value new-value)
          (push (list field old-value new-value) diffs))))
    (nreverse diffs)))

;;;; Accessibility/structure/perf-budget linter
;;
;; Permanent accessibility gate, unlike the differ above: exactly one h1,
;; every img has alt, no empty href, heading levels never skip, every
;; internal link resolves to a known output file, page weight within
;; budget.

(defconst denden-page-weight-budget-bytes 100000
  "Maximum byte size allowed for one rendered HTML page.")

(defun denden--heading-level (element)
  "Return ELEMENT's heading level, 1 to 6, if it is h1 through h6, else nil."
  (and (consp element)
       (let ((name (symbol-name (dom-tag element))))
         (when (string-match "\\`h\\([1-6]\\)\\'" name)
           (string-to-number (match-string 1 name))))))

(defun denden-lint-html-string (html &optional known-output-paths)
  "Return a list of problem strings found in HTML, a full page source. Checks
internal links against KNOWN-OUTPUT-PATHS if given."
  (let* ((dom (with-temp-buffer (insert html) (libxml-parse-html-region (point-min) (point-max))))
         (problems nil))
    (let ((byte-size (string-bytes html)))
      (when (> byte-size denden-page-weight-budget-bytes)
        (push (format "page weight %d bytes exceeds budget of %d" byte-size denden-page-weight-budget-bytes)
              problems)))
    (unless (cl-find-if (lambda (m) (dom-attr m 'charset)) (dom-by-tag dom 'meta))
      (push "no <meta charset> in head -- a browser with no other encoding signal will misdecode every nerd-font glyph" problems))
    (let ((h1-count (length (dom-by-tag dom 'h1))))
      (unless (= h1-count 1)
        (push (format "expected exactly one h1, found %d" h1-count) problems)))
    (dolist (img (dom-by-tag dom 'img))
      (unless (dom-attr img 'alt)
        (push (format "img with no alt: %s" (or (dom-attr img 'src) "(no src)")) problems)))
    (dolist (a (dom-by-tag dom 'a))
      (let ((href (dom-attr a 'href)))
        (when (and href (string-empty-p href))
          (push "a with empty href" problems))))
    (let ((previous-level nil))
      (dolist (heading (dom-search dom #'denden--heading-level))
        (let ((level (denden--heading-level heading)))
          (when (and previous-level (> level (1+ previous-level)))
            (push (format "heading level skips from h%d to h%d" previous-level level) problems))
          (setq previous-level level))))
    (when known-output-paths
      (dolist (a (dom-by-tag dom 'a))
        (let ((href (dom-attr a 'href)))
          (when (and href (string-prefix-p "/" href)
                     (not (member href known-output-paths)))
            (push (format "internal link does not resolve: %s" href) problems)))))
    (nreverse problems)))

(defun denden-lint-file (file &optional known-output-paths)
  "Return `denden-lint-html-string' problems for FILE's contents."
  (denden-lint-html-string
   (with-temp-buffer (insert-file-contents file) (buffer-string))
   known-output-paths))

;;;; Atom feeds, sitemap, robots.txt, alias redirects

(defun denden--atom-datetime (date)
  "Return DATE (\"YYYY-MM-DD\") as an Atom/RFC3339 datetime, midnight UTC."
  (format "%sT00:00:00Z" date))

(defun denden--atom-entry-node (page base-url)
  "Return one <entry> node for PAGE, with BASE-URL for each category's
\"tags/\" scheme URI."
  (append
   (list 'entry nil
         (list 'title nil (plist-get page :title))
         (list 'link (list :rel "alternate" :type "text/html" :href (plist-get page :permalink)))
         (list 'id nil (plist-get page :permalink)))
   (when (plist-get page :date)
     (list (list 'published nil (denden--atom-datetime (plist-get page :date)))))
   (list (list 'updated nil (denden--atom-datetime (plist-get page :lastmod))))
   (mapcar (lambda (tag)
             (list 'category (list :term (downcase tag) :label tag :scheme (format "%stags/" base-url))))
           (plist-get page :tags))
   (list (list 'content '(:type "html")
               (list 'raw-html nil (format "<![CDATA[%s]]>" (plist-get page :content-html)))))))

(cl-defun denden-atom-feed (&key pages site-title description base-url self-url
                                 author-name author-email favicon-url copyright)
  "Return an Atom feed string over PAGES, titled SITE-TITLE, rooted at
BASE-URL. SELF-URL is always the rss.xml permalink."
  (concat
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
   (denden-serialize-node
    (denden-normalise-node
     (append
      (list 'feed '(:xmlns "http://www.w3.org/2005/Atom")
            (list 'title nil site-title)
            (list 'subtitle nil (or description ""))
            (list 'link (list :rel "alternate" :type "text/html" :href base-url))
            (list 'link (list :rel "self" :type "application/atom+xml" :href self-url))
            (list 'id nil base-url))
      (when pages
        (list (list 'updated nil (denden--atom-datetime (plist-get (car pages) :lastmod)))))
      (when author-name
        (list (append (list 'author nil (list 'name nil author-name))
                      (when author-email (list (list 'email nil author-email))))))
      (list (list 'generator '(:uri "https://gohugo.io/") "Hugo"))
      (when favicon-url (list (list 'icon nil favicon-url)))
      (when copyright (list (list 'rights nil copyright)))
      (mapcar (lambda (page) (denden--atom-entry-node page base-url)) pages))))
   "\n"))

(defun denden-sitemap-xml (pages)
  "Return the sitemap.xml body: one <url> per page, with <loc> and <lastmod>
when known."
  (concat
   "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\n"
   (denden-serialize-node
    (denden-normalise-node
     (append
      (list 'urlset '(:xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"))
      (mapcar (lambda (page)
                (append (list 'url nil (list 'loc nil (plist-get page :permalink)))
                        (when (plist-get page :lastmod)
                          (list (list 'lastmod nil (denden--atom-datetime (plist-get page :lastmod)))))))
              pages))))
   "\n"))

(defun denden-robots-txt (base-url)
  "Return robots.txt text: allow everything, and point at the sitemap under
BASE-URL."
  (format "User-agent: *\nAllow: /\n\nSitemap: %ssitemap.xml\n"
          (file-name-as-directory base-url)))

(defun denden-alias-redirect-html (target-permalink)
  "Return a noindex, meta-refresh redirect page to TARGET-PERMALINK."
  (concat
   "<!doctype html>"
   (denden-serialize-node
    (denden-normalise-node
     (list 'html nil
           (list 'head nil
                 (list 'title nil target-permalink)
                 (list 'link (list :rel "canonical" :href target-permalink))
                 '(meta (:name "robots" :content "noindex"))
                 '(meta (:charset "utf-8"))
                 (list 'meta (list :http-equiv "refresh" :content (format "0; url=%s" target-permalink)))))))))

;;;; search-index.json generation
;;
;; One entry per sentence across all pages, orderless multi-word match at
;; query time, the sentence itself is what gets Text-Fragment highlighted
;; on click.

(defun denden-html-plain-text (html)
  "Return HTML with tags stripped and whitespace collapsed. Entities are not
decoded."
  (string-trim
   (replace-regexp-in-string
    "[ \t\n\r]+" " "
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t) (replace-match " "))
      (buffer-string)))))

(defun denden--search-index-sentences (plain-text)
  "Split PLAIN-TEXT into sentences on \".\", \"!\", or \"?\", dropping any 3
characters or shorter."
  (seq-filter (lambda (s) (> (length s) 3))
              (mapcar #'string-trim
                      (split-string (replace-regexp-in-string "\\([.!?]\\)[ \t\n]+" "\\1\1" plain-text) "\1"))))

(defun denden-search-index-json (pages)
  "Return the search-index.json payload: one {title url text} entry per
sentence across PAGES."
  (json-serialize
   (vconcat
    (seq-mapcat
     (lambda (page)
       (mapcar (lambda (sentence)
                 (list :title (plist-get page :title) :url (plist-get page :url) :text sentence))
               (denden--search-index-sentences (plist-get page :plain-text))))
     pages))))

;;;; OG card mechanism: text-wrapping primitives
;;
;; What's generic and reusable across any such card -- greedy word-wrapping
;; to a character width, and truncating a wrapped block to a line limit
;; with an ellipsis -- lives here; a site's actual card markup/colors is
;; its own template.

(defun denden-og-wrap-words (text max-width)
  "Return TEXT greedily word-wrapped to MAX-WIDTH characters per line."
  (let (lines (current ""))
    (dolist (word (split-string text " " t))
      (let ((candidate (if (string-empty-p current) word (concat current " " word))))
        (if (> (length candidate) max-width)
            (progn (when (not (string-empty-p current)) (push current lines))
                   (setq current word))
          (setq current candidate))))
    (unless (string-empty-p current) (push current lines))
    (nreverse lines)))

(defun denden-og-truncate-lines (lines max-lines)
  "Return LINES capped to MAX-LINES, with an ellipsis on the last line if
content was cut."
  (if (<= (length lines) max-lines)
      lines
    (let ((kept (seq-take lines max-lines)))
      (append (seq-take kept (1- max-lines))
              (list (concat (car (last kept)) "…"))))))

;;;; Interactive dev-session helpers
;;
;; Not part of the build itself. `require' is a no-op once a feature is
;; already provided, so editing this file and re-requiring it in a
;; long-running Emacs session does NOT pick up the change; `denden-reload-
;; all' reloads denden/site/theme fresh from disk instead.

;;;###autoload
(defun denden-reload-all ()
  "Reload denden.el, site.el and theme.el fresh from disk, without restarting
Emacs."
  (interactive)
  (dolist (dir '("denden" "site" "theme"))
    (let ((full-dir (expand-file-name dir denden-repository-directory)))
      (add-to-list 'load-path full-dir)
      (dolist (file (directory-files full-dir t "\\.el\\'"))
        (load file nil t)))))

;; Dev-only live reload: `denden-build-dev' rebuilds via `denden-build-
;; function' (the site layer's own async build entry point), and only
;; once it signals done -- proving the child process
;; has actually finished writing, not merely started -- appends a small
;; poll-and-reload script to every output page and refreshes a marker file
;; that script polls. A plain static file server is enough on the serving
;; side; nothing server-side has to push anything.

(defcustom denden-build-function nil
  "Function of (FORCE ON-DONE) that performs an async build with live reload.
Called by `denden-build-dev'/`denden-watch-start'. Must be set by the
site layer, e.g. to a function like `site-build-async'."
  :type 'function :group 'denden)

(defcustom denden-full-build-function nil
  "Function of one argument FORCE that performs a complete synchronous build.
Runs org-publish plus every site-specific auxiliary output. Set by the
site layer, e.g. to a function like `site-build'."
  :type 'function :group 'denden)

(defcustom denden-content-directory nil
  "Directory `denden-list-drafts' scans, and every content file lives under.
Set by the site layer."
  :type 'directory :group 'denden)

(defcustom denden-post-directory nil
  "Directory `denden-new' creates a new post file under.
Set by the site layer, typically a \"posts\" subdirectory of
`denden-content-directory'."
  :type 'directory :group 'denden)

(defcustom denden-publishing-directory nil
  "Directory a build writes its output to. Set by the site layer."
  :type 'directory :group 'denden)

(defcustom denden-project-root-directory nil
  "Project root `denden-find'/`denden-site' operate on.
Set by the site layer."
  :type 'directory :group 'denden)

(defcustom denden-preview-base-url "http://localhost:8000"
  "Base URL `denden-preview-command''s server answers on."
  :type 'string :group 'denden)

(defun denden--preview-port ()
  "The port number in `denden-preview-base-url', or \"8000\" if it has none."
  (if (string-match ":\\([0-9]+\\)\\'" denden-preview-base-url)
      (match-string 1 denden-preview-base-url)
    "8000"))

(defun denden--default-preview-command ()
  "Return the default `denden-preview-command': static-web-server, Nix run, or
Python's http.server, in that order."
  (let ((port (denden--preview-port)))
    (cond
     ((executable-find "static-web-server")
      (format "static-web-server --port %s --page404 404.html" port))
     ((executable-find "nix")
      (format "nix run nixpkgs#static-web-server -- --port %s --page404 404.html" port))
     (t (format "python3 -m http.server %s" port)))))

(defcustom denden-preview-command (denden--default-preview-command)
  "Shell command `denden-build-serve' runs to preview a build.
Runs with `denden-publishing-directory' as its working directory. See
`denden--default-preview-command' for how this default is picked."
  :type 'string :group 'denden)

(defcustom denden-new-post-template "#+title: %s\n#+date: %s\n#+tags[]: %s\n#+draft: %s\n\n"
  "Template `denden-new' formats and inserts into a new post buffer.
Formatted with title, today's date, space-separated tags, and
\"true\"/\"false\"."
  :type 'string :group 'denden)

(defconst denden-livereload-marker-file "denden-livereload-marker.txt"
  "Name of the marker file the live-reload script polls, at the output root.")

(defun denden--livereload-script-tag ()
  "Return the dev-only live-reload <script> tag, which polls
`denden-livereload-marker-file' every second."
  (format "<script>(function(){var last=null;function poll(){fetch(%S,{cache:\"no-store\"}).then(function(r){return r.text()}).then(function(t){if(last!==null&&t!==last){location.reload()}last=t}).catch(function(){})}setInterval(poll,1000)})();</script>"
          (concat "/" denden-livereload-marker-file)))

(defun denden-write-livereload-marker (pub-dir)
  "Write PUB-DIR's live-reload marker file with the current time."
  (with-temp-file (expand-file-name denden-livereload-marker-file pub-dir)
    (insert (format-time-string "%s%N"))))

(defun denden-inject-livereload-file (file)
  "Append the dev-only live-reload script to FILE, an HTML output file."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-max))
    (insert (denden--livereload-script-tag))
    (write-region (point-min) (point-max) file nil 0)))

(defun denden-inject-livereload (pub-dir)
  "Call `denden-inject-livereload-file' on every .html file under PUB-DIR."
  (dolist (file (directory-files-recursively pub-dir "\\.html\\'"))
    (denden-inject-livereload-file file)))

(defconst denden-preview-base-url-envvar "DENDEN_PREVIEW_BASE_URL"
  "Environment variable a site's base-url must consult first, for local
preview.")


(defun denden-build-dev (&optional force on-done)
  "Rebuild for local preview via `denden-build-function', then refresh the
live-reload marker."
  (interactive)
  (unless denden-build-function
    (error "Denden-build-function is not set -- the site layer must set it"))
  (setenv denden-preview-base-url-envvar denden-preview-base-url)
  (funcall denden-build-function
           force
           (lambda (_)
             (denden-write-livereload-marker denden-publishing-directory)
             (denden-inject-livereload denden-publishing-directory)
             (message "Denden: dev rebuild complete, live-reload marker refreshed")
             (when on-done (funcall on-done)))))

(defcustom denden-build-file-function nil
  "Function of (FILE ON-DONE) that rebuilds just FILE, one content source
file, then calls ON-DONE. Set by the site layer, e.g. to
`site-build-file-async'. `denden-watch--on-event' uses this fast path only
when every changed file is a content file. Any other change falls back to
a full rebuild."
  :type 'function :group 'denden)

(defun denden--content-org-file-p (file)
  "Return non-nil if FILE is a *.org file under `denden-content-directory'."
  (and denden-content-directory
       (string-suffix-p ".org" file)
       (file-in-directory-p file denden-content-directory)))

(defun denden-build-dev-file (file &optional on-done)
  "Rebuild just FILE via `denden-build-file-function', then refresh the
live-reload marker."
  (unless denden-build-file-function
    (error "Denden-build-file-function is not set -- the site layer must set it"))
  (setenv denden-preview-base-url-envvar denden-preview-base-url)
  (funcall denden-build-file-function
           file
           (lambda (_)
             (denden-write-livereload-marker denden-publishing-directory)
             (denden-inject-livereload denden-publishing-directory)
             (message "Denden: dev rebuild complete (%s), live-reload marker refreshed"
                       (file-relative-name file denden-repository-directory))
             (when on-done (funcall on-done)))))

(defvar denden-watch--descriptors nil
  "Active `file-notify' descriptors from `denden-watch-start'.")

(defvar denden-watch--timer nil
  "Debounce timer: a rebuild fires this long after the last change.")

(defconst denden-watch-debounce-seconds 0.6
  "Quiet period after a change before `denden-watch-start' rebuilds.")

(defun denden-watch--directories ()
  "Return content/static/denden/site/theme and every directory under them,
including the five bases themselves."
  (seq-filter
   #'file-directory-p
   (seq-mapcat
    (lambda (dir)
      (let ((full (expand-file-name dir denden-repository-directory)))
        (if (file-directory-p full) (cons full (directory-files-recursively full "" t)) nil)))
    '("content" "static" "denden" "site" "theme"))))

(defvar denden-watch--pending-files nil
  "Files touched since the last debounced rebuild fired.")

(defun denden-watch--on-event (event)
  "Record EVENT's file, then debounce: rebuild only changed content files, or
fall back to a full rebuild."
  (let ((file (nth 2 event)))
    (when file (push file denden-watch--pending-files)))
  (when denden-watch--timer (cancel-timer denden-watch--timer))
  (setq denden-watch--timer
        (run-at-time denden-watch-debounce-seconds nil
                      (lambda ()
                        (let ((files (delete-dups denden-watch--pending-files)))
                          (setq denden-watch--pending-files nil)
                          (denden-reload-all)
                          (if (and denden-build-file-function files
                                   (seq-every-p #'denden--content-org-file-p files))
                              (progn
                                (message "Denden: change detected, rebuilding %d file(s)..."
                                         (length files))
                                (dolist (file files) (denden-build-dev-file file)))
                            (message "Denden: change detected, rebuilding...")
                            (denden-build-dev t)))))))

;;;###autoload
(defun denden-watch-start ()
  "Watch content/static/denden/site/theme for changes and auto-rebuild,
debounced with live reload."
  (interactive)
  (denden-watch-stop)
  (dolist (dir (cons denden-repository-directory (denden-watch--directories)))
    (push (file-notify-add-watch dir '(change) #'denden-watch--on-event)
          denden-watch--descriptors))
  (message "Denden: watching for changes (%d directories)" (length denden-watch--descriptors)))

;;;###autoload
(defun denden-watch-stop ()
  "Stop all watches started by `denden-watch-start'."
  (interactive)
  (dolist (descriptor denden-watch--descriptors)
    (ignore-errors (file-notify-rm-watch descriptor)))
  (setq denden-watch--descriptors nil)
  (setq denden-watch--pending-files nil)
  (when denden-watch--timer (cancel-timer denden-watch--timer) (setq denden-watch--timer nil)))

(define-minor-mode denden-auto-rebuild-mode
  "Global minor mode: while enabled, content, site, and theme changes
auto-rebuild with live reload."
  :global t
  :lighter " Denden-Auto"
  (if denden-auto-rebuild-mode (denden-watch-start) (denden-watch-stop)))

;;;; Command-defining macro, for a site layer's own commands

(defmacro denden-define-command (site name arglist docstring &rest body)
  "Define an autoloaded command denden-SITE-NAME that wraps BODY with start
and done messages."
  (declare (indent 3) (doc-string 4))
  (let ((full-name (intern (format "denden-%s-%s" site name))))
    `(progn
       ;;;###autoload
       (defun ,full-name ,arglist
         ,docstring
         (interactive)
         (message "%s..." ',full-name)
         ,@body
         (message "%s done" ',full-name)))))

;;;; User-facing build/content commands

;;;###autoload
(defun denden-build (&optional force)
  "Build the site via `denden-full-build-function', using the timestamp cache
unless FORCE."
  (interactive "P")
  (unless denden-full-build-function
    (error "Denden-full-build-function is not set -- the site layer must set it"))
  (setenv denden-preview-base-url-envvar nil)
  (funcall denden-full-build-function force)
  (message "Denden-build: done"))

;;;###autoload
(defun denden-clean-build ()
  "Clear the publish cache and rebuild everything from scratch."
  (interactive)
  (denden-build t))

(defun denden--start-preview-server ()
  "Serve `denden-publishing-directory' via `denden-preview-command', without
building first."
  (let ((default-directory (file-name-as-directory denden-publishing-directory)))
    (async-shell-command denden-preview-command)))

;;;###autoload
(defun denden-build-serve ()
  "Build the site, then serve it via `denden-preview-command'."
  (interactive)
  (denden-build)
  (denden--start-preview-server))

;;;###autoload
(defun denden-clean ()
  "Delete `denden-publishing-directory' entirely, without rebuilding."
  (interactive)
  (if (and denden-publishing-directory (file-directory-p denden-publishing-directory))
      (progn (delete-directory denden-publishing-directory t)
             (message "Denden-clean: removed %s" denden-publishing-directory))
    (message "Denden-clean: nothing to remove")))

;;;###autoload
(defun denden-lint ()
  "Run `denden-lint-file' over every page under `denden-publishing-directory',
shown in *denden-lint*."
  (interactive)
  (let ((problems nil))
    (dolist (file (directory-files-recursively denden-publishing-directory "\\.html\\'"))
      (dolist (problem (denden-lint-file file))
        (push (format "%s: %s" (file-relative-name file denden-publishing-directory) problem) problems)))
    (with-current-buffer (get-buffer-create "*denden-lint*")
      (erase-buffer)
      (insert (if problems (mapconcat #'identity (nreverse problems) "\n") "No problems found."))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun denden-list-drafts ()
  "List every draft post file under `denden-content-directory'."
  (interactive)
  (let ((files (directory-files-recursively denden-content-directory "\\.org\\'"))
        (lines nil))
    (dolist (file files)
      (when (denden-file-is-draft-p file)
        (push (file-relative-name file denden-content-directory) lines)))
    (setq lines (nreverse lines))
    (with-current-buffer (get-buffer-create "*denden-drafts*")
      (erase-buffer)
      (if lines
          (dolist (line lines) (insert line "\n"))
        (insert "No drafts."))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun denden-diff (old-file new-file)
  "Compare OLD-FILE and NEW-FILE's rendered structure, shown in *denden-diff*."
  (interactive "fFirst file: \nfSecond file: ")
  (let ((diffs (denden-diff-pages old-file new-file)))
    (with-current-buffer (get-buffer-create "*denden-diff*")
      (erase-buffer)
      (if diffs
          (dolist (diff diffs) (insert (format "%S\n\n" diff)))
        (insert "No differences."))
      (display-buffer (current-buffer)))))

(defun denden--all-content-tags ()
  "Return every distinct #+TAGS[] value used across `denden-content-directory'."
  (delete-dups
   (seq-mapcat
    (lambda (file)
      (split-string (or (cadr (assoc "TAGS[]" (with-temp-buffer
                                                  (insert-file-contents file)
                                                  (org-mode)
                                                  (org-collect-keywords '("TAGS[]")))))
                         "")))
    (directory-files-recursively denden-content-directory "\\.org\\'"))))

(defvar org-capture-templates)
(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function org-set-tags-command "org" (&optional arg))

(defun denden--capture-standalone-post (title tags draft)
  "Create a standalone post file for TITLE under `denden-post-directory', via
`org-capture'."
  (require 'org-capture)
  (let* ((slug (denden-slugify-path title))
         (file (expand-file-name (concat slug ".org") denden-post-directory))
         (template (concat (format denden-new-post-template title
                                    (format-time-string "%Y-%m-%d")
                                    (string-join tags " ")
                                    (if draft "true" "false"))
                            "%?")))
    (make-directory (file-name-directory file) t)
    (let ((org-capture-templates
           (list (list "d" "denden new post" 'plain (list 'file file)
                       template :jump-to-captured t :empty-lines-after 1))))
      (org-capture nil "d"))))

;;;###autoload
(defun denden-new ()
  "Create a new post via `org-capture'."
  (interactive)
  (denden--capture-standalone-post
   (read-string "Title: ") (completing-read-multiple "Tags: " (denden--all-content-tags))
   (y-or-n-p "Draft? ")))


(defvar denden-preview--server-started nil
  "Non-nil once `denden-preview-current-file' has started a preview server.")

(defun denden--project-for-file (file)
  "Return the expanded leaf project whose :base-directory contains FILE, or
nil."
  (cl-find-if
   (lambda (project)
     (let ((base (org-publish-property :base-directory project)))
       (and base (file-in-directory-p file (expand-file-name base)))))
   (org-publish-expand-projects org-publish-project-alist)))

(defun denden--preview-url-for (file)
  "Return FILE's pretty-URL path, or nil if FILE has no project."
  (let ((project (denden--project-for-file file)))
    (when project
      (let* ((pub-dir (file-name-as-directory (org-publish-property :publishing-directory project)))
             (output (denden-output-file-for file project)))
        (concat "/" (denden-pretty-url (file-relative-name output pub-dir)))))))

;;;###autoload
(defun denden-preview-current-file ()
  "Rebuild the current file for local preview and open it in a browser, once
per session."
  (interactive)
  (let* ((file (or buffer-file-name (error "This buffer has no file")))
         (url (denden--preview-url-for file)))
    (unless url
      (error "Denden: %s is not part of any known project" file))
    (if denden-preview--server-started
        (denden-build-dev-file file)
      (denden-build-dev
       t
       (lambda ()
         (denden--start-preview-server)
         (setq denden-preview--server-started t)
         (browse-url (concat (string-remove-suffix "/" denden-preview-base-url) url)))))))

;;;###autoload
(defun denden-find ()
  "Find a file under `denden-project-root-directory' (`project-find-file')."
  (interactive)
  (let ((default-directory denden-project-root-directory))
    (project-find-file)))

;;;###autoload
(defun denden-site ()
  "Switch to `denden-project-root-directory' (`project-switch-project')."
  (interactive)
  (project-switch-project denden-project-root-directory))

(provide 'denden)
;;; denden.el ends here

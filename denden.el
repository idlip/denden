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
;; `denden-post-directory' and `denden-container-file' (both used only by
;; `denden-new'). Everything below that reads one of those variables works
;; for any site, not just one in particular.
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
  "HTML elements that never have a closing tag or children.
Serialized self-closing, e.g. <br />, matching the HTML5 void element list.")

(defun denden--keyword-to-attribute-symbol (keyword)
  "Turn attribute KEYWORD such as :class into the plain symbol class."
  (intern (substring (symbol-name keyword) 1)))

(defun denden-normalise-attributes (attribute-spec)
  "Convert ATTRIBUTE-SPEC into the canonical alist of (SYMBOL . VALUE).

ATTRIBUTE-SPEC may already be an alist, for example ((class . \"x\")), or a
keyword plist, for example (:class \"x\" :role \"y\"). Anything else is
most importantly a single dotted pair such as (class . \"x\") where a list
of one pair, ((class . \"x\")), was meant as signals an error instead of
silently producing the wrong tree."
  (cond
   ((null attribute-spec) nil)
   ((not (proper-list-p attribute-spec))
    (error "denden: malformed attribute spec %S is a dotted pair, not a list of pairs -- did you forget to wrap it in a list?"
           attribute-spec))
   ((keywordp (car attribute-spec))
    (unless (cl-evenp (length attribute-spec))
      (error "denden: attribute plist %S has an odd number of elements"
             attribute-spec))
    (cl-loop for (key value) on attribute-spec by #'cddr
             do (unless (keywordp key)
                  (error "denden: attribute plist %S mixes keyword and non-keyword keys at %S"
                         attribute-spec key))
             collect (cons (denden--keyword-to-attribute-symbol key) value)))
   (t
    (dolist (pair attribute-spec)
      (unless (and (consp pair) (symbolp (car pair)) (not (keywordp (car pair))))
        (error "denden: attribute alist %S contains a malformed entry %S"
               attribute-spec pair)))
    attribute-spec)))

(defun denden-normalise-node (node)
  "Normalise template-form NODE into the canonical dom.el shape.

NODE is (TAG ATTRIBUTE-SPEC . CHILDREN); a bare string child is left as a
text node. Returns (TAG ATTRIBUTE-ALIST . NORMALISED-CHILDREN)."
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
  "Serialize one canonical (NAME . VALUE) attribute PAIR, or nil to omit it.

A nil VALUE omits the attribute entirely, for a conditionally-absent
attribute. A VALUE of t emits a bare boolean attribute, e.g. disabled.
Any other VALUE is stringified and quoted."
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
  "Serialize canonical dom.el NODE to an HTML string.

NODE is (TAG ATTRIBUTES . CHILDREN) as produced by `denden-normalise-node',
or a string text node. Void elements (see `denden-void-elements') are
emitted self-closing with no children; every other element always gets an
explicit closing tag, never a self-closing one.

A `raw-html' tag is a second special case: `(raw-html nil STRING)' emits
STRING verbatim, unescaped and with no wrapping element, for splicing in
already-rendered HTML (e.g. an org-export body) that would otherwise get
HTML-escaped as if it were plain text."
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
                (error "denden: void element %s was given children %S"
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
  "Convert libxml dom.el NODE into denden template-form sexp.

Drops whitespace-only text-node children and rewrites the attribute alist
as a keyword plist, so the result reads like a hand-written template rather
than output parsed back out of rendered HTML."
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
  "Read FILE and return its contents as template-form sexp.
One-shot use: paste the result into a template and hand-edit from there."
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
  "Hash table of heading ids already used in the current export.
Keys are id strings, values are how many times each has been seen. Reset
per file by `denden--reset-heading-ids', so dedup never leaks across files.")

(defun denden--reset-heading-ids (backend)
  "Clear `denden--heading-ids' before an export with a denden-html BACKEND.
Hooked onto `org-export-before-parsing-functions', which runs once per file
before any reference is computed, so each file's ids dedupe independently."
  (when (org-export-derived-backend-p backend 'denden-html)
    (setq denden--heading-ids (make-hash-table :test 'equal))))

(add-hook 'org-export-before-parsing-functions #'denden--reset-heading-ids)

(defun denden--slugify-heading-text (text)
  "Slugify TEXT into a lowercase, hyphen-separated identifier fragment.
Falls back to \"section\" if nothing alphanumeric survives, so a heading
that is pure punctuation or a single emoji still gets a usable id."
  (let* ((slug (downcase text))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
         (slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug)))
    (if (string-empty-p slug) "section" slug)))

(defun denden--unique-heading-id (candidate)
  "Return CANDIDATE, or CANDIDATE with a numeric suffix if already used.
Applies uniformly to :CUSTOM_ID:, :ID: and slugified ids alike, so two
headings that accidentally share a hand-written :CUSTOM_ID: still end up
with distinct ids in the page rather than a duplicate DOM id."
  (unless denden--heading-ids
    (setq denden--heading-ids (make-hash-table :test 'equal)))
  (let ((seen-count (gethash candidate denden--heading-ids 0)))
    (puthash candidate (1+ seen-count) denden--heading-ids)
    (if (zerop seen-count) candidate (format "%s-%d" candidate (1+ seen-count)))))

(defun denden-heading-id (datum)
  "Return the stable HTML id for headline or inlinetask DATUM.
Honours :CUSTOM_ID: or :ID: when present, slugifies the heading text
otherwise, and dedupes collisions with a numeric suffix.

Caches the assigned id on DATUM itself, under the property :denden-id,
because `org-html--reference' is called many times for the same heading --
once for its own <h2> tag, again for its Table of Contents entry, again for
any internal link that resolves to it -- and every call must return the
same id."
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
  "Redirect headline and inlinetask id computation to `denden-heading-id'.
Wraps `org-html--reference' (:around advice) so every one of its ~20 call
sites in ox-html.el agrees on the same id for a given heading. Applies only
within a denden-html export and only to headline/inlinetask DATUM; every
other reference type and every other backend falls through to
ORIGINAL-FUNCTION unchanged. NAMED-ONLY is ignored for headline/inlinetask,
matching `org-html--reference' itself."
  (if (and (memq (org-element-type datum) '(headline inlinetask))
           (org-export-derived-backend-p (plist-get info :back-end) 'denden-html))
      (denden-heading-id datum)
    (funcall original-function datum info named-only)))

(advice-add 'org-html--reference :around #'denden--advise-html-reference)

(defun denden-headline (headline contents info)
  "Transcode a HEADLINE element exactly as `org-html-headline' does.
That function computes its id via `org-html--reference', which
`denden--advise-html-reference' redirects to `denden-heading-id' for a
denden-html export, so no separate id logic is needed here."
  (org-html-headline headline contents info))

(defun denden-src-block (src-block contents info)
  "Transcode SRC-BLOCK exactly as `org-html-src-block' does, but always with
`org-html-htmlize-output-type' bound to `css' and
`org-html-htmlize-font-prefix' to \"org-\", regardless of the caller's
global settings, so highlighted code always comes out as class= markup that
a stylesheet can theme -- never inline style= colors baked in at build
time."
  (let ((org-html-htmlize-output-type 'css)
        (org-html-htmlize-font-prefix "org-"))
    (org-html-src-block src-block contents info)))

(defun denden-highlight-stylesheet (class-color-alist)
  "Return a CSS stylesheet string from CLASS-COLOR-ALIST.
Each entry is (CLASS-NAME . COLOR-VALUE): CLASS-NAME is a bare htmlize class
such as \"org-keyword\" (no leading dot), and COLOR-VALUE is any valid CSS
color -- most usefully a custom-property reference such as
\"var(--base0E)\" so the highlighted code follows whatever scheme is
currently active."
  (mapconcat
   (lambda (entry) (format ".%s { color: %s; }" (car entry) (cdr entry)))
   class-color-alist
   "\n"))

(defun denden-item (item contents info)
  "Transcode ITEM exactly as `org-html-item' does, for now.
Placeholder: wired into the derived backend's :translate-alist so a later
inline convention can be read from list items here without re-deriving the
backend."
  (org-html-item item contents info))

(defun denden--resolve-site-path (raw-path all-pages)
  "RAW-PATH (a bare site-root path from an Org [[/path]] link -- with or
without a trailing slash or .html extension) resolved against
ALL-PAGES's own :url values (already slugified pretty URLs,
`denden-collect-page-metadata's own output). Returns the matching page
plist, or nil."
  (let ((key (string-trim-left
              (string-trim-right (string-trim-right raw-path "\\.html\\'") "/")
              "/")))
    (seq-find (lambda (p) (equal (string-trim-right (plist-get p :url) "/") key)) all-pages)))

(defun denden--strip-file-uri-prefix (html)
  "HTML with any \"file://\" prefix removed from an href/src attribute
that is otherwise a site-root-absolute path. `org-export-file-uri' wraps
ANY absolute path this way once `org-html-link' cannot resolve it relative
to :base-directory -- including a path that was never a real local file to
begin with."
  (replace-regexp-in-string "\\(href\\|src\\)=\"file://\\(/[^\"]*\\)\"" "\\1=\"\\2\"" html))

(defun denden--escape-attribute-ampersands (html)
  "HTML with a bare & inside any href/src attribute value escaped to
&amp;. `org-html-link' builds a URL's href/src verbatim from its raw text
(e.g. a query string's own \"&\"), so an unescaped & in an attribute value
is invalid HTML."
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
  "`org-html-link's own output, with the two real gaps against it this
backend patches: `denden--strip-file-uri-prefix' and
`denden--escape-attribute-ampersands'."
  (denden--escape-attribute-ampersands (denden--strip-file-uri-prefix html)))

(defun denden-link (link contents info)
  "Transcode LINK as `org-html-link' does, except a bare site-root path
([[/path]], with or without a trailing slash or .html) that resolves
against :denden-all-pages (an ext-plist property the caller sets, e.g. via
`denden-export-options-with-pages') gets its own visible text replaced with
the target page's live :title -- unconditionally, even over an explicit
given description, UNLESS that description carries its own nested markup
(a real link, bold text, ...), in which case it is left untouched. A path
that does not resolve to a known page (an asset reference) still goes
through `org-html-link', but through `denden--fix-link-html'."
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
  "Special-block types `denden-special-block' renders as a styled callout
div (theme.css's .callout/.callout-TYPE), ported from the Hugo theme's
shortcodes of the same names. Any other special-block type is untouched
-- `org-html-special-block's own default.")

(defun denden-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK (a #+begin_TYPE ... #+end_TYPE block) as
<div class=\"callout callout-TYPE\">CONTENTS</div> when TYPE is one of
`denden-callout-types', matching this site's note/tip/warn callouts
exactly (a real Org special block now, not Hugo shortcode text embedded
in the source -- CONTENTS is already normal Org-exported HTML, so
formatting inside a callout is consistent with the rest of the page).
Any other type falls back to `org-html-special-block' unchanged."
  (let ((type (downcase (org-element-property :type special-block))))
    (if (member type denden-callout-types)
        (format "<div class=\"callout callout-%s\">\n%s</div>" type (or contents ""))
      (org-html-special-block special-block contents info))))

(defconst denden-html-default-options
  '(:with-broken-links mark :with-toc nil :section-numbers nil)
  "Ext-plist every `org-export-as'/`org-export-to-file' call with the
denden-html backend should pass. :with-toc nil and :section-numbers nil
override ox-html's own defaults (both t), which this build's real content
convention never wants unless a page opts in per-file.")

(defun denden-export-options-with-pages (all-pages)
  "`denden-html-default-options' plus :denden-all-pages ALL-PAGES, for
`denden-link's internal bare-path resolution -- every `org-export-as' call
whose body might contain a [[/path]]-style internal link should use this
instead of the bare `denden-html-default-options'."
  (append denden-html-default-options (list :denden-all-pages all-pages)))

(defun denden-paragraph (paragraph contents info)
  "Like `org-html-paragraph', except a standalone image's caption is not
prefixed with \"Figure N: \" (ox-html's own hardcoded behaviour), and its
src is not left as a broken file:// URI when the image was referenced by a
site-root path."
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
  "Publish FILENAME with the denden-html backend, ox-html's own generic
document template (no site chrome), to its own pretty-URL path
(`denden-output-file-for'). PLIST is as in any ox-publish
:publishing-function; PUB-DIR is unused, same reason a real site's own
per-page publishing function ignores its own PUB-DIR argument: it is a
per-file subdirectory Org computes internally, not the project's real
:publishing-directory `denden-output-file-for' needs."
  (let ((output (denden-output-file-for filename (cons nil plist))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert (with-temp-buffer
                (insert-file-contents filename)
                (org-mode)
                (org-export-as 'denden-html nil nil nil plist))))
    output))

(defun denden-slugify-path (relative-path)
  "RELATIVE-PATH with every \"/\"-separated component slugified: lowercased,
runs of whitespace/underscore collapsed to a single hyphen, characters
outside [a-z0-9-] dropped, repeated hyphens collapsed, no leading/trailing
hyphen -- Hugo's own default URL sanitizer, applied per path segment."
  (mapconcat
   (lambda (component)
     (let* ((lower (downcase component))
            (hyphenated (replace-regexp-in-string "[ _]+" "-" lower))
            (stripped (replace-regexp-in-string "[^a-z0-9-]" "" hyphenated))
            (collapsed (replace-regexp-in-string "-\\{2,\\}" "-" stripped)))
       (string-trim collapsed "-+" "-+")))
   (split-string relative-path "/") "/"))

(defun denden--pretty-output-relative-path (slugged)
  "SLUGGED (an already-slugified, extension-stripped relative path)
mapped to the pretty-URL output shape: nested under its own directory as
index.html, unless it already names an index page."
  (if (equal (file-name-nondirectory slugged) "index")
      slugged
    (concat (file-name-as-directory slugged) "index")))

(defun denden-output-file-for (source-file project)
  "Return the absolute output path SOURCE-FILE maps to under PROJECT.
The one place this computation happens -- publishing functions that write
their own output (rather than going through `org-publish-org-to') must
call this too, not `org-export-output-file-name' (which needs a real
buffer-file-name to derive a path and otherwise prompts interactively)."
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
  "OUTPUT-RELATIVE-PATH (as `denden-output-file-for' produces, relative to
the publishing root -- always ending in an index page) rendered as a
pretty-URL href: the directory portion only, trailing slash kept, empty
string for the site root."
  (string-remove-suffix "index.html" output-relative-path))

;;;; Single-big-org-file authoring: post subtrees inside a container file
;;
;; A heading with a non-empty :EXPORT_FILE_NAME: property is a post
;; subtree: its own content exports standalone, to the path
;; EXPORT_FILE_NAME itself names (through the same slugify/pretty-URL
;; pipeline a whole file's own relative path goes through -- EXPORT_FILE_NAME
;; may itself contain "/", e.g. "wander/some-post"). A file with at least
;; one such heading is a container: none of the file's own #+content
;; publishes as a page, only its marked subtrees do -- mirrors ox-hugo's
;; one-post-per-subtree convention (see docs/ox-hugo-subtree-export-
;; research.org), minus its HUGO_SECTION grouping properties (a plain "/"
;; in EXPORT_FILE_NAME does the same job) and minus cross-subtree internal
;; link rewriting: a [[#heading]]/[[id:...]] link from one post subtree to
;; another in the *same* container file resolves as an in-page anchor, not
;; a cross-page link, same known gap ox-hugo needed a whole preprocessing
;; pass to close -- not yet done here.
;;
;; EXPORT_* subtree properties denden reads, each mirroring an existing
;; whole-file #+KEYWORD (all but EXPORT_FILE_NAME optional):
;; EXPORT_TITLE (falls back to the heading text), EXPORT_DATE, EXPORT_TAGS,
;; EXPORT_REFS, EXPORT_ALIASES, EXPORT_DRAFT.

(defun denden--subtree-property (headline name)
  "HEADLINE's own :EXPORT_NAME: property value, or nil."
  (org-element-property (intern (concat ":EXPORT_" name)) headline))

(defun denden--subtree-property-true-p (headline name)
  "Non-nil if HEADLINE's :EXPORT_NAME: property is \"t\"/\"true\"."
  (let ((value (denden--subtree-property headline name)))
    (and value (member (downcase value) '("t" "true")))))

(defun denden--subtree-post-nested-p (headline)
  "Non-nil if HEADLINE (itself a post subtree) has an ancestor heading
that is also a post subtree -- a silent content-duplication footgun
ox-hugo allows and denden rejects instead (see the research doc's
\"Gotchas\", nesting)."
  (let ((node (org-element-property :parent headline)))
    (catch 'found
      (while node
        (when (and (eq (org-element-type node) 'headline)
                   (let ((v (org-element-property :EXPORT_FILE_NAME node)))
                     (and v (not (string-empty-p v)))))
          (throw 'found t))
        (setq node (org-element-property :parent node)))
      nil)))

(defun denden--subtree-post-headlines (file)
  "Return every headline element in FILE with a non-empty
:EXPORT_FILE_NAME: property, in document order. Errors if any is nested
inside another post subtree (see `denden--subtree-post-nested-p'); warns
(does not error) if one has neither :CUSTOM_ID: nor :ID: -- `denden-
heading-id' already privileges those for a stable HTML anchor, and
reusing whichever the author already set means one property doing two
jobs instead of an unstable slugified-text fallback for this one."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((headlines (org-element-map (org-element-parse-buffer) 'headline
                       (lambda (h) (let ((v (org-element-property :EXPORT_FILE_NAME h)))
                                     (and v (not (string-empty-p v)) h))))))
      (dolist (h headlines)
        (when (denden--subtree-post-nested-p h)
          (error "denden: post subtree \"%s\" in %s is nested inside another post subtree -- not supported"
                 (org-element-property :EXPORT_FILE_NAME h) file))
        (unless (or (org-element-property :CUSTOM_ID h) (org-element-property :ID h))
          (message "denden: post subtree \"%s\" in %s has no :CUSTOM_ID:/:ID: -- its HTML anchor id will shift if the heading text ever changes"
                   (org-element-property :EXPORT_FILE_NAME h) file)))
      headlines)))

(defun denden-file-has-subtree-posts-p (file)
  "Non-nil if FILE is a container: at least one heading sets
:EXPORT_FILE_NAME:."
  (and (denden--subtree-post-headlines file) t))

(defun denden--goto-subtree-post (export-file-name)
  "In the current buffer (a container file already inserted, org-mode
active), move point to the post subtree whose own :EXPORT_FILE_NAME: is
EXPORT-FILE-NAME."
  (let ((pos (org-find-property "EXPORT_FILE_NAME" export-file-name)))
    (unless pos
      (error "denden: post subtree %S not found (file changed during build?)" export-file-name))
    (goto-char pos)))

(defun denden-output-file-for-subtree (export-file-name project)
  "Return the absolute output path for a post subtree whose own
:EXPORT_FILE_NAME: is EXPORT-FILE-NAME, under PROJECT -- the subtree
equivalent of `denden-output-file-for', with EXPORT-FILE-NAME (already a
relative path, may itself contain \"/\") standing in for the source
file's own relative path, since a subtree post has no file path of its
own to derive one from."
  (let* ((pub-dir (file-name-as-directory (org-publish-property :publishing-directory project)))
         (output-extension (org-publish-property :denden-output-extension project))
         (slugged (denden-slugify-path export-file-name))
         (pretty (denden--pretty-output-relative-path slugged)))
    (expand-file-name (if output-extension (concat pretty output-extension) export-file-name) pub-dir)))

(defun denden-expected-output-files (leaf-projects)
  "Return the output paths LEAF-PROJECTS (no :components) should produce.
For a project with :denden-output-extension, a container file expands to
one output per non-draft post subtree instead of one for the file itself."
  (let (files)
    (dolist (project leaf-projects)
      (let ((subtree-aware (org-publish-property :denden-output-extension project)))
        (dolist (source (org-publish-get-base-files project))
          (let ((headlines (and subtree-aware (denden--subtree-post-headlines source))))
            (if headlines
                (dolist (h headlines)
                  (unless (denden--subtree-property-true-p h "DRAFT")
                    (push (denden-output-file-for-subtree (org-element-property :EXPORT_FILE_NAME h) project)
                          files)))
              (push (denden-output-file-for source project) files))))))
    files))

(defun denden-sweep-output-directory (pub-dir expected-files)
  "Delete every file under PUB-DIR absent from EXPECTED-FILES.
Also removes directories left empty by that deletion."
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
  "Sweep every :publishing-directory LEAF-PROJECTS (already expanded, no
:components) write to."
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
  "Build PROJECT synchronously, then sweep its output.
With FORCE, ignore ox-publish's timestamp cache and republish everything.
Binds `coding-system-for-read'/`-write' to utf-8-unix for the duration:
a plain interactive Emacs session with no coding-system preference set
hits `select-safe-coding-system-interactively' the moment any page's
nerd-font glyphs get written, which blocks on a minibuffer question this
call site has no reason to expect -- every page declares utf-8 anyway
(theme-baseof's own <meta charset>), so there is nothing to actually ask."
  (let ((leaves (org-publish-expand-projects (list project)))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (save-window-excursion
      (let ((org-publish-use-timestamps-flag (not force)))
        (when force (org-publish-remove-all-timestamps))
        (org-publish-projects leaves)))
    (denden-sweep-projects leaves)))

(defun denden-build-project-async (project &optional force)
  "Build PROJECT in a separate Emacs process, then sweep there.
Non-blocking: returns as soon as the child process starts. Components are
expanded here, in the parent, since the child never sees the parent's full
`org-publish-project-alist' -- only the data spliced into its body."
  (let ((leaves (org-publish-expand-projects (list project))))
    (org-export-async-start
        (lambda (_) nil)
      `(let ((org-publish-use-timestamps-flag ,(not force)))
         (when ',force (org-publish-remove-all-timestamps))
         (org-publish-projects ',leaves)
         (denden-sweep-projects ',leaves)))))

(defun denden-git-lastmod (file)
  "Return FILE's last commit date as \"YYYY-MM-DD\", or nil if FILE has no
git history (uncommitted, or not in a repo)."
  (let ((default-directory (file-name-directory file)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "log" "-1" "--format=%cs"
                                  "--" (file-name-nondirectory file)))
        (let ((output (string-trim (buffer-string))))
          (unless (string-empty-p output) output))))))

(defun denden--org-date-to-iso (date-string)
  "Convert org timestamp DATE-STRING, e.g. \"[2024-02-03 Sat]\", to
\"2024-02-03\", or nil if DATE-STRING is nil."
  (and date-string (format-time-string "%Y-%m-%d" (org-time-string-to-time date-string))))

(defun denden-format-iso-date (iso-date format-string)
  "ISO-DATE (\"YYYY-MM-DD\") rendered via FORMAT-STRING
(`format-time-string' syntax), or \"\" if ISO-DATE is nil. A page shows a
date in several different shapes depending on where, so every caller that
needs a non-ISO shape shares this one implementation."
  (if (and iso-date (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" iso-date))
      (format-time-string format-string
                          (encode-time 0 0 0 (string-to-number (match-string 3 iso-date))
                                       (string-to-number (match-string 2 iso-date))
                                       (string-to-number (match-string 1 iso-date))))
    ""))

(defun denden--source-section (source base-dir)
  "Return SOURCE's top-level directory relative to BASE-DIR, or \"\" if
SOURCE sits directly in BASE-DIR."
  (let ((relative (file-relative-name source base-dir)))
    (if (string-match-p "/" relative) (car (split-string relative "/")) "")))

(defun denden-file-keyword-true-p (file keyword)
  "Non-nil if FILE's #+KEYWORD: value is true (\"t\"/\"true\", case
insensitive) -- shared by every boolean front-matter convention this
content uses (#+draft:, #+toc:, ...)."
  (let ((value (cadr (assoc keyword (with-temp-buffer
                                       (insert-file-contents file)
                                       (org-mode)
                                       (org-collect-keywords (list keyword)))))))
    (and value (member (downcase value) '("t" "true")))))

(defun denden-file-is-draft-p (file)
  "Non-nil if FILE's #+draft: keyword is true."
  (denden-file-keyword-true-p file "DRAFT"))

(defun denden-draft-exclude-regexp (base-directory)
  "A regexp, in org-publish's own :exclude shape, matching every
#+draft: t file under BASE-DIRECTORY (by path relative to it), or nil if
there are none. Meant to be set as a project's :exclude so both the real
publish loop and every `org-publish-get-base-files' caller skip drafts the
same way."
  (let ((drafts (seq-filter #'denden-file-is-draft-p
                            (directory-files-recursively base-directory "\\.org\\'"))))
    (when drafts
      (mapconcat (lambda (file) (regexp-quote (file-relative-name file base-directory)))
                 drafts "\\|"))))

(defun denden-sort-pages-by-date-desc (pages)
  "PAGES sorted by :date descending; a page with no :date sorts last."
  (sort (copy-sequence pages)
        (lambda (a b) (string> (or (plist-get a :date) "") (or (plist-get b :date) "")))))

(defun denden-sort-pages-by-title (pages)
  "PAGES sorted by :title ascending, case-insensitively."
  (sort (copy-sequence pages)
        (lambda (a b) (string< (downcase (plist-get a :title)) (downcase (plist-get b :title))))))

(defun denden-page-lastmod (page)
  "PAGE's Lastmod: `denden-git-lastmod' on its :source, falling back to
:date."
  (or (denden-git-lastmod (plist-get page :source)) (plist-get page :date)))

(defun denden-html-word-count (html)
  "HTML's word count: strip tags, split on whitespace."
  (let ((text (with-temp-buffer
                (insert html)
                (goto-char (point-min))
                (while (re-search-forward "<[^>]+>" nil t) (replace-match " "))
                (buffer-string))))
    (length (split-string text nil t))))

(defun denden--file-word-count (file)
  "FILE's exported body, word count -- needed by any listing page that
shows an article's reading time."
  (denden-html-word-count
   (with-temp-buffer
     (insert-file-contents file)
     (org-mode)
     (org-export-as 'denden-html nil nil t denden-html-default-options))))

(defun denden--subtree-word-count (file export-file-name)
  "Word count of post subtree EXPORT-FILE-NAME's exported body, in FILE."
  (denden-html-word-count
   (with-temp-buffer
     (insert-file-contents file)
     (org-mode)
     (denden--goto-subtree-post export-file-name)
     (org-export-as 'denden-html t nil t denden-html-default-options))))

(defun denden--subtree-post-metadata (headline source project base-dir)
  "Metadata plist for post subtree HEADLINE in SOURCE, same shape as one
of `denden-collect-page-metadata's whole-file entries. :tags comes from
HEADLINE's own native Org tags (the \":tag1:tag2:\" on the heading line
itself, not inherited) rather than an EXPORT_TAGS property -- Org already
has a tagging mechanism per heading, no need for a second one."
  (let* ((export-file-name (org-element-property :EXPORT_FILE_NAME headline))
         (pub-dir (file-name-as-directory (org-publish-property :publishing-directory project)))
         (output (denden-output-file-for-subtree export-file-name project))
         (word-count (denden--subtree-word-count source export-file-name)))
    (list :url (denden-pretty-url (file-relative-name output pub-dir))
          :title (or (denden--subtree-property headline "TITLE") (org-element-property :raw-value headline))
          :tags (org-element-property :tags headline)
          :refs (split-string (or (denden--subtree-property headline "REFS") ""))
          :date (denden--org-date-to-iso (denden--subtree-property headline "DATE"))
          :section (denden--source-section source base-dir)
          :source source
          :aliases (split-string (or (denden--subtree-property headline "ALIASES") ""))
          :wordcount word-count
          :readingtime (max 1 (round (/ word-count 200.0))))))

(defun denden-collect-page-metadata (leaf-projects)
  "Return (:url :title :tags :refs :date :section :source :aliases
:wordcount :readingtime) plists for every page LEAF-PROJECTS (already
expanded, html-producing components only) publish -- one per source file,
or one per non-draft post subtree for a container file (see
`denden--subtree-post-headlines'). Whole-file drafts are excluded via each
project's own :exclude (see `denden-draft-exclude-regexp'); subtree drafts
(:EXPORT_DRAFT:) are excluded here. Reads
#+TITLE/#+TAGS[]/#+REFS[]/#+DATE/#+ALIASES[] keywords, or their EXPORT_*
subtree-property equivalents. :url is the pretty-URL shape
(`denden-pretty-url'): site-root-relative, no leading slash, no extension,
a trailing slash except for the site root itself (\"\"). :source is the
absolute source file path, for callers (Atom feeds, OG cards) that need to
re-derive Lastmod or re-render the body."
  (let (pages)
    (dolist (project leaf-projects)
      (when (org-publish-property :denden-output-extension project)
        (let ((base-dir (file-name-as-directory (org-publish-property :base-directory project)))
              (pub-dir (file-name-as-directory (org-publish-property :publishing-directory project))))
          (dolist (source (org-publish-get-base-files project))
            (let ((headlines (denden--subtree-post-headlines source)))
              (if headlines
                  (dolist (h headlines)
                    (unless (denden--subtree-property-true-p h "DRAFT")
                      (push (denden--subtree-post-metadata h source project base-dir) pages)))
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
                        pages))))))))
    (nreverse pages)))

(defun denden-pages-in-section (pages section)
  "Return the subset of PAGES whose :section is SECTION."
  (seq-filter (lambda (page) (equal (plist-get page :section) section)) pages))

(defun denden-pages-with-tag (pages tag)
  "Return the subset of PAGES whose :tags include TAG."
  (seq-filter (lambda (page) (member tag (plist-get page :tags))) pages))

(defun denden-group-pages-by-year (pages)
  "Group page-metadata PAGES (see `denden-collect-page-metadata') by the
year of :date, newest year first, newest page first within a year.
A page with no :date sorts last, grouped under the key \"\"."
  (let* ((sorted (denden-sort-pages-by-date-desc pages))
         (groups nil))
    (dolist (page sorted)
      (let ((year (if (plist-get page :date) (substring (plist-get page :date) 0 4) "")))
        (if (and groups (equal (caar groups) year))
            (setcdr (car groups) (cons page (cdar groups)))
          (push (cons year (list page)) groups))))
    (mapcar (lambda (group) (cons (car group) (nreverse (cdr group)))) (nreverse groups))))

(defun denden--taxonomy-counts (pages field)
  "Return (:tag :count) plists for every value across PAGES's FIELD
(:tags or :refs), sorted by count descending then name. Shared by
`denden-tag-counts' and `denden-ref-counts'."
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
count descending then tag name."
  (denden--taxonomy-counts pages :tags))

(defun denden-ref-counts (pages)
  "Return (:tag :count) plists for every value across PAGES's :refs, same
shape and ordering as `denden-tag-counts'."
  (denden--taxonomy-counts pages :refs))

(defun denden-collect-body-links (body-html)
  "Return deduped (:href :text :internal) plists for links in BODY-HTML, a
rendered post body. Only http(s):// and /-prefixed hrefs count; first
occurrence of a given href wins."
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
  "Return the subset of PAGES whose :refs include REF."
  (seq-filter (lambda (page) (member ref (plist-get page :refs))) pages))

(defun denden--item-link (item)
  "Return the link object inside ITEM's :tag secondary string, or nil."
  (car (org-element-map (org-element-property :tag item) 'link #'identity)))

(defun denden--link-text (link)
  "LINK's bracket description as raw text, or its raw-link if none."
  (let ((begin (org-element-property :contents-begin link))
        (end (org-element-property :contents-end link)))
    (if (and begin end) (string-trim (buffer-substring-no-properties begin end))
      (org-element-property :raw-link link))))

(defun denden-export-org-fragment (text)
  "Export TEXT, a raw Org-mode inline fragment, to an HTML string via the
denden-html backend, so an inline link/bold/code renders as real markup
instead of leaking as literal Org syntax when spliced with the `raw-html'
pseudo-tag. `org-export-string-as' always wraps a bare fragment in one
<p>...</p>; stripped here since callers splice the result inline, not as a
block."
  (if (string-empty-p text)
      text
    (let ((html (string-trim (org-export-string-as text 'denden-html t denden-html-default-options))))
      (if (and (string-prefix-p "<p>" html) (string-suffix-p "</p>" html))
          (substring html 3 -4)
        html))))

(defun denden-parse-topic-item (item)
  "Return (:url :name :desc :parts) for descriptive-list ITEM: URL/NAME
from the link inside its term, DESC the text before the first \" | \" in
its body (exported to HTML, `denden-export-org-fragment'), PARTS the
\" | \"-split remainder, trimmed and left as raw text. Reads the AST
directly instead of regexing rendered dt/dd HTML."
  (let* ((link (denden--item-link item))
         (begin (org-element-property :contents-begin item))
         (end (org-element-property :contents-end item))
         (raw (if (and begin end) (string-trim (buffer-substring-no-properties begin end)) ""))
         (segments (mapcar #'string-trim (split-string raw " | "))))
    (list :url (if link (org-element-property :raw-link link) "")
          :name (if link (denden--link-text link) "")
          :desc (denden-export-org-fragment (or (car segments) ""))
          :parts (cdr segments))))

(defun denden-parse-topic-list (file)
  "Parse FILE (a #+layout: topic-list content file) into groups: a list of
(:label H2-TITLE :items (denden-parse-topic-item results)), one group per
top-level headline, items limited to its descriptive-list items."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((tree (org-element-parse-buffer)))
      (org-element-map tree 'headline
        (lambda (headline)
          (list :label (org-element-property :raw-value headline)
                :items (seq-mapcat
                        (lambda (plain-list)
                          (if (eq (org-element-property :type plain-list) 'descriptive)
                              (mapcar #'denden-parse-topic-item (org-element-map plain-list 'item #'identity))
                            nil))
                        (org-element-map (org-element-contents headline) 'plain-list #'identity
                                          nil nil 'headline))))
        nil nil 'headline))))

(defun denden-topic-list-intro-html (file)
  "The exported body of FILE up to (not including) its first H2 section --
the intro prose before any topic group."
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
  "Collapse TEXT's whitespace runs to single spaces and trim."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text)))

(defun denden--page-skeleton (dom)
  "Return a normalised skeleton plist for DOM: headings, links, images,
landmarks and visible text."
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
  "Parse FILE's contents as HTML and return its dom.el tree."
  (with-temp-buffer
    (insert-file-contents file)
    (libxml-parse-html-region (point-min) (point-max))))

(defun denden-diff-pages (old-file new-file)
  "Compare OLD-FILE and NEW-FILE's normalised skeletons.
Return nil if they match, else a list of (FIELD OLD-VALUE NEW-VALUE) for
every field that differs."
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
  "Maximum acceptable byte size for one rendered HTML page.")

(defun denden--heading-level (element)
  "Return ELEMENT's heading level (1-6) if it is h1..h6, else nil."
  (and (consp element)
       (let ((name (symbol-name (dom-tag element))))
         (when (string-match "\\`h\\([1-6]\\)\\'" name)
           (string-to-number (match-string 1 name))))))

(defun denden-lint-html-string (html &optional known-output-paths)
  "Return a list of problem strings found in HTML, a full page source.
KNOWN-OUTPUT-PATHS, when given, is a list of site-root-relative paths
(e.g. \"/about/\") that internal links (href starting with \"/\") are
checked against; without it, that one check is skipped."
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
  "DATE (\"YYYY-MM-DD\") as an Atom/RFC3339 datetime, midnight UTC."
  (format "%sT00:00:00Z" date))

(defun denden--atom-entry-node (page base-url)
  "One <entry> node for PAGE. BASE-URL is the site root, for each
category's \"tags/\" scheme URI."
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
  "An Atom feed string over PAGES (each a plist: :title :permalink :date
:lastmod :tags :content-html). SELF-URL is always the feed's own canonical
(rss.xml) permalink, regardless of which of rss.xml/feed.xml/index.xml is
being built, which is exactly why the three real output files are
byte-identical."
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
  "sitemap.xml body: one <url> per page, <loc> plus <lastmod> when known.
PAGES are plists with :permalink and :lastmod."
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
  "robots.txt: allow everything, point at the real sitemap.
Plain text, not markup -- there is no tag tree here for the sexp-HTML
serializer to help with."
  (format "User-agent: *\nAllow: /\n\nSitemap: %ssitemap.xml\n"
          (file-name-as-directory base-url)))

(defun denden-alias-redirect-html (target-permalink)
  "A noindex, canonical-linked, meta-refresh redirect page to
TARGET-PERMALINK."
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
  "HTML with tags stripped and whitespace collapsed to single spaces.
Entities are not decoded."
  (string-trim
   (replace-regexp-in-string
    "[ \t\n\r]+" " "
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t) (replace-match " "))
      (buffer-string)))))

(defun denden--search-index-sentences (plain-text)
  "PLAIN-TEXT split into sentences on \".\"/\"!\"/\"?\" followed by
whitespace, each trimmed, sentences of 3 characters or fewer dropped."
  (seq-filter (lambda (s) (> (length s) 3))
              (mapcar #'string-trim
                      (split-string (replace-regexp-in-string "\\([.!?]\\)[ \t\n]+" "\\1\1" plain-text) "\1"))))

(defun denden-search-index-json (pages)
  "The search-index.json payload: one {title url text} entry per sentence
across PAGES (each a plist: :title :url :plain-text). :url is used
verbatim -- the caller (`site-build-search-index') is responsible for it
already being an absolute, slash-prefixed path; this function does not
assume any particular shape for it. No precomputed tokens array: the
client matches words against :text directly."
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
  "TEXT greedily word-wrapped to MAX-WIDTH characters per line: a word is
added to the current line unless doing so would exceed MAX-WIDTH, in which
case the current line is pushed and the word starts a new one."
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
  "LINES capped to MAX-LINES; when that cuts real content, the last kept
line gets an ellipsis appended."
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
  "Reload denden.el, site.el and theme.el fresh from disk (`load', not
`require'), so edits made in this Emacs session take effect without
restarting it."
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
  "Function of (FORCE ON-DONE) that performs an async build with
live-reload support, called by `denden-build-dev'/`denden-watch-start'.
Must be set by the site layer, e.g. to a function like `site-build-async'."
  :type 'function :group 'denden)

(defcustom denden-full-build-function nil
  "Function of one argument FORCE that performs a complete synchronous
build (org-publish plus every site-specific auxiliary output). Set by the
site layer, e.g. to a function like `site-build'."
  :type 'function :group 'denden)

(defcustom denden-content-directory nil
  "Directory `denden-list-drafts' scans, and every content file lives
under. Set by the site layer."
  :type 'directory :group 'denden)

(defcustom denden-post-directory nil
  "Directory `denden-new' creates a standalone post file under, when not
appending into `denden-container-file'. Set by the site layer, typically
a \"posts\" subdirectory of `denden-content-directory'."
  :type 'directory :group 'denden)

(defcustom denden-container-file nil
  "Path to a container file (see \"Single-big-org-file authoring\" above)
that `denden-new' offers to append a new post subtree into, instead of
creating a standalone file. nil means `denden-new' always creates a
standalone file. Set by the site layer. May be a bare file name (e.g.
\"source.org\", resolved against `denden-content-directory' -- see
`denden--container-file-path') or a full absolute path; either way it is
never resolved against Org's own `org-directory', which is what a bare
name would otherwise fall back to inside `org-capture'."
  :type '(choice (const :tag "None" nil) file) :group 'denden)

(defcustom denden-publishing-directory nil
  "Directory a build writes its output to. Set by the site layer."
  :type 'directory :group 'denden)

(defcustom denden-project-root-directory nil
  "Project root `denden-find'/`denden-site' operate on. Set by the site
layer."
  :type 'directory :group 'denden)

(defcustom denden-preview-command "python3.14 -m http.server"
  "Shell command `denden-build-serve' runs, with `denden-publishing-
directory' as its working directory, to preview a build."
  :type 'string :group 'denden)

(defcustom denden-new-post-template "#+title: %s\n#+date: %s\n#+tags[]: %s\n#+draft: %s\n\n"
  "Template `denden-new' formats with title, today's date, space-separated
tags, and \"true\"/\"false\", then inserts into a new post buffer."
  :type 'string :group 'denden)

(defconst denden-livereload-marker-file "denden-livereload-marker.txt"
  "Name of the marker file the live-reload script polls, at the output
root. Its content (a timestamp) only has to change on every rebuild.")

(defun denden--livereload-script-tag ()
  "The dev-only live-reload <script> tag text: polls
`denden-livereload-marker-file' every second and reloads the page when its
content changes from what was last seen."
  (format "<script>(function(){var last=null;function poll(){fetch(%S,{cache:\"no-store\"}).then(function(r){return r.text()}).then(function(t){if(last!==null&&t!==last){location.reload()}last=t}).catch(function(){})}setInterval(poll,1000)})();</script>"
          (concat "/" denden-livereload-marker-file)))

(defun denden-write-livereload-marker (pub-dir)
  "Write/refresh PUB-DIR's live-reload marker file to the current time."
  (with-temp-file (expand-file-name denden-livereload-marker-file pub-dir)
    (insert (format-time-string "%s%N"))))

(defun denden-inject-livereload-file (file)
  "Append the dev-only live-reload script to FILE (one .html output).
Safe to call again on the same file after a fresh rebuild rewrote it
from source -- publishing always regenerates a page's content from
scratch, so there is nothing already-injected left to duplicate."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-max))
    (insert (denden--livereload-script-tag))
    (write-region (point-min) (point-max) file nil 0)))

(defun denden-inject-livereload (pub-dir)
  "`denden-inject-livereload-file' on every .html file under PUB-DIR."
  (dolist (file (directory-files-recursively pub-dir "\\.html\\'"))
    (denden-inject-livereload-file file)))

(defun denden-build-dev (&optional force on-done)
  "Rebuild for local preview via `denden-build-function' (async), then --
once the child process actually finishes -- refresh the live-reload marker
and append the poll script to every page under `denden-publishing-
directory'. ON-DONE, when given, is called with no arguments after that."
  (interactive)
  (unless denden-build-function
    (error "denden-build-function is not set -- the site layer must set it"))
  (funcall denden-build-function
           force
           (lambda (_)
             (denden-write-livereload-marker denden-publishing-directory)
             (denden-inject-livereload denden-publishing-directory)
             (message "denden: dev rebuild complete, live-reload marker refreshed")
             (when on-done (funcall on-done)))))

(defcustom denden-build-file-function nil
  "Function of (FILE ON-DONE) that rebuilds just FILE -- one content
source file -- async, with a live build's freshly refreshed cross-page
metadata, then calls ON-DONE. Set by the site layer, e.g. to
`site-build-file-async'. `denden-watch--on-event' uses this fast path
only when every file touched during a debounce window is a content
file (see `denden--content-org-file-p'); a CSS/JS/elisp/config change
always falls back to a full, forced `denden-build-dev' rebuild instead
-- a template/asset edit doesn't touch any .org file's mtime, so ox-
publish's own timestamp cache would otherwise skip every page."
  :type 'function :group 'denden)

(defun denden--content-org-file-p (file)
  "Non-nil if FILE is a *.org file living under `denden-content-
directory' -- the only case `denden-watch--on-event' considers safe for
the single-file fast rebuild path, matching the real \"site-org\"
project's own :base-extension \"org\"."
  (and denden-content-directory
       (string-suffix-p ".org" file)
       (file-in-directory-p file denden-content-directory)))

(defun denden-build-dev-file (file &optional on-done)
  "Rebuild just FILE via `denden-build-file-function' (async), then --
once done -- refresh the live-reload marker and re-append the poll
script to every page. Re-sweeping the whole directory rather than just
FILE's own output is simplest: predicting a source file's exact output
path would have to special-case the home page, topic-list pages, and a
single-big-org-file container's many-subtrees-per-file shape, and the
sweep itself is cheap (a few dozen small files, single-digit
milliseconds) -- not worth that complexity to skip. ON-DONE, when given,
is called with no arguments after that."
  (unless denden-build-file-function
    (error "denden-build-file-function is not set -- the site layer must set it"))
  (funcall denden-build-file-function
           file
           (lambda (_)
             (denden-write-livereload-marker denden-publishing-directory)
             (denden-inject-livereload denden-publishing-directory)
             (message "denden: dev rebuild complete (%s), live-reload marker refreshed"
                       (file-relative-name file denden-repository-directory))
             (when on-done (funcall on-done)))))

(defvar denden-watch--descriptors nil
  "Active `file-notify' descriptors from `denden-watch-start'.")

(defvar denden-watch--timer nil
  "Debounce timer: a rebuild fires this long after the last change.")

(defconst denden-watch-debounce-seconds 0.6
  "Quiet period after a change before `denden-watch-start' rebuilds.")

(defun denden-watch--directories ()
  "Content/, static/, denden/, site/, theme/ themselves, plus every
directory under them. `directory-files-recursively' never includes its
own BASE argument in its result -- only descendants -- so without the
explicit `cons' below a file living directly in one of these five (e.g.
content/hello-world.org, not content/posts/hello-world.org) is watched
by nothing: the five top-level directories are absent from the list, and
`denden-watch-start' otherwise only watches `denden-repository-
directory' itself, which catches direct repo-root children, not files
two levels down."
  (seq-filter
   #'file-directory-p
   (seq-mapcat
    (lambda (dir)
      (let ((full (expand-file-name dir denden-repository-directory)))
        (if (file-directory-p full) (cons full (directory-files-recursively full "" t)) nil)))
    '("content" "static" "denden" "site" "theme"))))

(defvar denden-watch--pending-files nil
  "Files touched since the last debounced rebuild fired (event index 2 --
see `file-notify-add-watch's docstring for the (DESCRIPTOR ACTION FILE
[FILE1]) event shape).")

(defun denden-watch--on-event (event)
  "Record EVENT's file, then debounce: reload elisp and rebuild
`denden-watch-debounce-seconds' after the last file-notify event, not on
every single one. When every file touched during the debounce window is
a content .org file (`denden--content-org-file-p') and
`denden-build-file-function' is set, rebuilds only those files -- the
fast path. Otherwise (a CSS/JS/elisp/config change touched something,
or no per-file hook is set) falls back to a full, forced
`denden-build-dev' rebuild -- forced because a template/asset edit
doesn't touch any .org file's mtime, so ox-publish's own timestamp
cache would otherwise skip every page."
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
                                (message "denden: change detected, rebuilding %d file(s)..."
                                         (length files))
                                (dolist (file files) (denden-build-dev-file file)))
                            (message "denden: change detected, rebuilding...")
                            (denden-build-dev t)))))))

;;;###autoload
(defun denden-watch-start ()
  "Watch content/, static/, denden/, site/, theme/ for changes and rebuild
automatically (debounced), with live reload. Call `denden-watch-stop' to
turn this off, or use `denden-auto-rebuild-mode' instead of calling this
directly."
  (interactive)
  (denden-watch-stop)
  (dolist (dir (cons denden-repository-directory (denden-watch--directories)))
    (push (file-notify-add-watch dir '(change) #'denden-watch--on-event)
          denden-watch--descriptors))
  (message "denden: watching for changes (%d directories)" (length denden-watch--descriptors)))

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
  "Global minor mode: while enabled, content/static/denden/site/theme
changes trigger a debounced rebuild with live reload
(`denden-watch-start'); disabling it calls `denden-watch-stop'."
  :global t
  :lighter " Denden-Auto"
  (if denden-auto-rebuild-mode (denden-watch-start) (denden-watch-stop)))

;;;; Command-defining macro, for a site layer's own commands

(defmacro denden-define-command (site name arglist docstring &rest body)
  "Define an autoloaded interactive command denden-SITE-NAME.
ARGLIST/DOCSTRING/BODY are as in `defun'. Wraps BODY in a start/done
message pair using the resulting command's own name, so a site layer gets
the same M-x ergonomics as denden's own commands without repeating the
boilerplate."
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
  "Build the site, using ox-publish's own timestamp cache unless FORCE.
Calls `denden-full-build-function'."
  (interactive "P")
  (unless denden-full-build-function
    (error "denden-full-build-function is not set -- the site layer must set it"))
  (funcall denden-full-build-function force)
  (message "denden-build: done"))

;;;###autoload
(defun denden-clean-build ()
  "Clear the publish cache and rebuild everything from scratch."
  (interactive)
  (denden-build t))

(defun denden--start-preview-server ()
  "Serve `denden-publishing-directory' via `denden-preview-command',
without building first."
  (let ((default-directory (file-name-as-directory denden-publishing-directory)))
    (async-shell-command denden-preview-command)))

;;;###autoload
(defun denden-build-serve ()
  "Build the site, then serve `denden-publishing-directory' via
`denden-preview-command'."
  (interactive)
  (denden-build)
  (denden--start-preview-server))

;;;###autoload
(defun denden-clean ()
  "Delete `denden-publishing-directory' entirely, without rebuilding."
  (interactive)
  (if (and denden-publishing-directory (file-directory-p denden-publishing-directory))
      (progn (delete-directory denden-publishing-directory t)
             (message "denden-clean: removed %s" denden-publishing-directory))
    (message "denden-clean: nothing to remove")))

;;;###autoload
(defun denden-lint ()
  "Run `denden-lint-file' over every page under `denden-publishing-
directory' and show the results in *denden-lint*."
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
  "List every draft post under `denden-content-directory' in
*denden-drafts*: whole draft files, plus every :EXPORT_DRAFT: post
subtree inside a container file."
  (interactive)
  (let ((files (directory-files-recursively denden-content-directory "\\.org\\'"))
        (lines nil))
    (dolist (file files)
      (if (denden-file-is-draft-p file)
          (push (file-relative-name file denden-content-directory) lines)
        (dolist (h (denden--subtree-post-headlines file))
          (when (denden--subtree-property-true-p h "DRAFT")
            (push (format "%s :: %s" (file-relative-name file denden-content-directory)
                          (org-element-property :EXPORT_FILE_NAME h))
                  lines)))))
    (setq lines (nreverse lines))
    (with-current-buffer (get-buffer-create "*denden-drafts*")
      (erase-buffer)
      (if lines
          (dolist (line lines) (insert line "\n"))
        (insert "No drafts."))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun denden-diff (old-file new-file)
  "Compare OLD-FILE and NEW-FILE's rendered structure (`denden-diff-pages')
and show the differences in *denden-diff*. A debugging aid for comparing
two builds of the same page, not part of the real build."
  (interactive "fFirst file: \nfSecond file: ")
  (let ((diffs (denden-diff-pages old-file new-file)))
    (with-current-buffer (get-buffer-create "*denden-diff*")
      (erase-buffer)
      (if diffs
          (dolist (diff diffs) (insert (format "%S\n\n" diff)))
        (insert "No differences."))
      (display-buffer (current-buffer)))))

(defun denden--all-content-tags ()
  "Every distinct tag used across `denden-content-directory': #+TAGS[]
whole-file keywords, plus every post subtree's own native Org heading
tags (see `denden--subtree-post-metadata')."
  (delete-dups
   (seq-mapcat
    (lambda (file)
      (append
       (split-string (or (cadr (assoc "TAGS[]" (with-temp-buffer
                                                   (insert-file-contents file)
                                                   (org-mode)
                                                   (org-collect-keywords '("TAGS[]")))))
                          ""))
       (seq-mapcat (lambda (h) (org-element-property :tags h)) (denden--subtree-post-headlines file))))
    (directory-files-recursively denden-content-directory "\\.org\\'"))))

(defvar org-capture-templates)
(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function org-set-tags-command "org" (&optional arg))

(defun denden--capture-standalone-post (title tags draft)
  "Create a standalone post file for TITLE under `denden-post-directory',
via `org-capture' (so template expansion, cursor placement and
`jump-to-captured' behave like any other capture), using
`denden-new-post-template'. TAGS is a #+tags[] value -- a standalone file
has no heading of its own to carry Org tags on."
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

(defun denden--container-file-path ()
  "`denden-container-file' resolved to an absolute path: used as-is when
already absolute, else resolved against `denden-content-directory'.
Passed to `org-capture' pre-resolved so its own `(file ...)' target
handling (`org-capture-expand-file') never falls back to Org's own
`org-directory' for a bare relative file name -- `expand-file-name'
ignores its DIRECTORY argument entirely once FILENAME is already
absolute, so a pre-resolved absolute path is immune to that fallback."
  (if (file-name-absolute-p denden-container-file)
      denden-container-file
    (expand-file-name denden-container-file denden-content-directory)))

(defun denden--capture-into-container (title draft)
  "Append TITLE as a new post subtree to `denden-container-file'
(resolved via `denden--container-file-path'), via `org-capture'. Prompts
for the subtree's own :EXPORT_FILE_NAME: (the output path; defaults to
the slugified TITLE). DRAFT becomes :EXPORT_DRAFT:; :EXPORT_DATE: is
today; :CUSTOM_ID: is derived from :EXPORT_FILE_NAME:'s own final path
component, so `denden-heading-id' picks it up for a stable HTML anchor
too. Tags are NOT prompted here -- Org already has a
per-heading tagging mechanism, so once the entry exists (still in the
capture buffer, before the user finalizes it), `org-set-tags-command' is
called on it directly, with Org's own completion (every tag already used
anywhere in the buffer) rather than denden's own."
  (require 'org-capture)
  (let* ((export-name (read-string "EXPORT_FILE_NAME (output path): " (denden-slugify-path title)))
         (custom-id (denden-slugify-path (file-name-nondirectory export-name)))
         (properties (concat ":PROPERTIES:\n"
                              (format ":EXPORT_FILE_NAME: %s\n" export-name)
                              (format ":CUSTOM_ID: %s\n" custom-id)
                              (format ":EXPORT_DATE: %s\n" (format-time-string "[%Y-%m-%d %a]"))
                              (if draft ":EXPORT_DRAFT: t\n" "")
                              ":END:\n"))
         (template (format "* %s\n%s\n%%?" title properties)))
    (let ((org-capture-templates
           (list (list "d" "denden new post subtree" 'entry (list 'file (denden--container-file-path))
                       template :jump-to-captured t :empty-lines-after 1))))
      (org-capture nil "d"))
    (save-excursion (org-back-to-heading t) (call-interactively #'org-set-tags-command))))

;;;###autoload
(defun denden-new ()
  "Create a new post via `org-capture'. Prompts for a title, then whether
to append it as a new post subtree into `denden-container-file' (tags set
afterward via `org-set-tags-command', Org's own completion --
`denden--capture-into-container'), or create a standalone file under
`denden-post-directory' (tags via `completing-read-multiple' against
every tag already in use -- `denden--capture-standalone-post')."
  (interactive)
  (let ((title (read-string "Title: ")))
    (if (and denden-container-file
             (y-or-n-p (format "Append to %s? " (denden--container-file-path))))
        (denden--capture-into-container title (y-or-n-p "Draft? "))
      (denden--capture-standalone-post
       title (completing-read-multiple "Tags: " (denden--all-content-tags)) (y-or-n-p "Draft? ")))))

;;;; One-shot content-merge tool
;;
;; Converts existing whole-file posts into post subtrees of one container
;; file, for experimenting with single-big-org-file authoring against
;; real content (see docs/ox-hugo-subtree-export-research.org) without
;; committing to it: read-only on the source files, always writes a NEW
;; output file.

(defun denden--org-file-front-matter-and-body (file)
  "Return (:title :date :tags :draft :refs :aliases :body) for FILE.
:body is everything after the leading #+KEYWORD: front-matter block and
any blank lines following it."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let* ((keywords (org-collect-keywords '("TITLE" "DATE" "TAGS[]" "DRAFT" "REFS[]" "ALIASES[]")))
           (body-start (save-excursion
                         (goto-char (point-min))
                         (while (or (looking-at "^#\\+") (looking-at "^[ \t]*$"))
                           (forward-line 1))
                         (point))))
      (list :title (cadr (assoc "TITLE" keywords))
            :date (cadr (assoc "DATE" keywords))
            :tags (split-string (or (cadr (assoc "TAGS[]" keywords)) ""))
            :draft (member (downcase (or (cadr (assoc "DRAFT" keywords)) "")) '("t" "true"))
            :refs (cadr (assoc "REFS[]" keywords))
            :aliases (cadr (assoc "ALIASES[]" keywords))
            :body (buffer-substring-no-properties body-start (point-max))))))

(defun denden--demote-headings-one-level (body)
  "BODY with every Org heading demoted by one level (an extra leading
\"*\"), so it nests correctly as a post subtree's own children instead of
top-level siblings of other merged posts."
  (replace-regexp-in-string "^\\(\\*+\\)\\( \\)" "*\\1\\2" body))

;;;###autoload
(defun denden-merge-posts-into-container (posts-directory output-file)
  "One-shot, non-destructive: merge every .org file under POSTS-DIRECTORY
into OUTPUT-FILE as post subtrees, for experimenting with single-big-
org-file authoring against real content. Does not touch the source
files; OUTPUT-FILE is overwritten if it already exists. Each source file
becomes one top-level heading, its own headings (if any) demoted by one
level to nest as children. :EXPORT_FILE_NAME: is the file's own relative
path under `denden-content-directory' (so URLs match today's whole-file
build); #+TAGS[] become the heading's own native Org tags, not a
property, matching `denden-new's own container-append convention."
  (interactive
   (list (read-directory-name "Merge posts under: " denden-post-directory)
         (read-file-name "Write merged container to: " denden-content-directory)))
  (let ((files (directory-files-recursively posts-directory "\\.org\\'")))
    (with-temp-file output-file
      (insert (format "#+title: %s (merged by denden-merge-posts-into-container, %s)\n\n"
                      (file-name-nondirectory (directory-file-name posts-directory))
                      (format-time-string "%Y-%m-%d")))
      (dolist (file files)
        (let* ((data (denden--org-file-front-matter-and-body file))
               (export-name (denden-slugify-path
                             (file-name-sans-extension (file-relative-name file denden-content-directory))))
               (tags-suffix (if (plist-get data :tags)
                                 (format " :%s:" (string-join (plist-get data :tags) ":"))
                               "")))
          (insert (format "* %s%s\n" (or (plist-get data :title) (file-name-base file)) tags-suffix))
          (insert ":PROPERTIES:\n")
          (insert (format ":EXPORT_FILE_NAME: %s\n" export-name))
          (insert (format ":CUSTOM_ID: %s\n" (denden-slugify-path (file-name-nondirectory export-name))))
          (when (plist-get data :date) (insert (format ":EXPORT_DATE: %s\n" (plist-get data :date))))
          (when (plist-get data :refs) (insert (format ":EXPORT_REFS: %s\n" (plist-get data :refs))))
          (when (plist-get data :aliases) (insert (format ":EXPORT_ALIASES: %s\n" (plist-get data :aliases))))
          (when (plist-get data :draft) (insert ":EXPORT_DRAFT: t\n"))
          (insert ":END:\n\n")
          (insert (denden--demote-headings-one-level (plist-get data :body)))
          (insert "\n"))))
    (find-file output-file)
    (message "denden-merge-posts-into-container: merged %d file(s) into %s" (length files) output-file)))

(defcustom denden-preview-base-url "http://localhost:8000"
  "Base URL `denden-preview-command''s server answers on."
  :type 'string :group 'denden)

(defvar denden-preview--server-started nil
  "Non-nil once `denden-preview-current-file' has started a preview
server this Emacs session.")

(defun denden--project-for-file (file)
  "The expanded leaf project (from `org-publish-project-alist') whose
:base-directory contains FILE, or nil."
  (cl-find-if
   (lambda (project)
     (let ((base (org-publish-property :base-directory project)))
       (and base (file-in-directory-p file (expand-file-name base)))))
   (org-publish-expand-projects org-publish-project-alist)))

(defun denden--preview-url-for (file)
  "FILE's own pretty-URL path (leading slash), or nil if FILE belongs to
no known project. When the current buffer is visiting FILE and point is
inside a post subtree, resolves that subtree's own :EXPORT_FILE_NAME:
instead of FILE's own -- a container file publishes nothing itself."
  (let ((project (denden--project-for-file file)))
    (when project
      (let* ((pub-dir (file-name-as-directory (org-publish-property :publishing-directory project)))
             (export-file-name (and (equal buffer-file-name file)
                                     (org-entry-get (point) "EXPORT_FILE_NAME" t)))
             (output (if export-file-name
                         (denden-output-file-for-subtree export-file-name project)
                       (denden-output-file-for file project))))
        (concat "/" (denden-pretty-url (file-relative-name output pub-dir)))))))

;;;###autoload
(defun denden-preview-current-file ()
  "Rebuild the current buffer's file for local preview and open it in a
browser. On the first call this session: a forced `denden-build-dev' (so
live-reload is wired into every page, not just this one), then starts
the preview server. On later calls: just `denden-build-dev-file' -- the
already-open tab's own live-reload script picks up the change, so this
never opens a second tab."
  (interactive)
  (let* ((file (or buffer-file-name (error "This buffer has no file")))
         (url (denden--preview-url-for file)))
    (unless url
      (error "denden: %s is not part of any known project" file))
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

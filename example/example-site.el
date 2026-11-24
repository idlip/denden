;;; example-site.el --- A small blog built with Denden -*- lexical-binding: t; -*-

;;; Commentary:

;; A whole blog in one definition: Denden's default layout and page, a
;; small style sheet in the spirit of Bear Blog (static/style.css), and
;; one output of its own, modus.css, which takes the site's colours and
;; the colours of its code from Emacs's Modus Vivendi theme.  Copy this
;; folder to start a site of your own.  Build it with M-x denden-build,
;; or with `just build' in the denden folder.

;;; Code:

(require 'denden)

;; Modus lives with Emacs's themes; `load-theme' loads it.
(declare-function modus-themes-get-color-value "modus-themes" (color &optional overrides theme))

(defconst example-site-colors
  '(("--background-color" . bg-main)
    ("--background-dim" . bg-dim)
    ("--text-color" . fg-main)
    ("--heading-color" . fg-heading-1)
    ("--blockquote-color" . fg-dim)
    ("--border-color" . border)
    ("--link-color" . fg-link)
    ("--visited-color" . fg-link-visited)
    ("--accent-color" . keyword))
  "CSS variables of the site, and the Modus colour each takes.")

(defconst example-site-code-colors
  '(("org-default" . fg-main)
    ("org-keyword" . keyword)
    ("org-negation-char" . keyword)
    ("org-string" . string)
    ("org-doc" . docstring)
    ("org-comment" . comment)
    ("org-comment-delimiter" . comment)
    ("org-function-name" . fnname)
    ("org-variable-name" . variable)
    ("org-type" . type)
    ("org-constant" . constant)
    ("org-builtin" . builtin)
    ("org-preprocessor" . preprocessor)
    ("org-warning" . warning))
  "Code classes that Denden writes, and the Modus colour each takes.")

(defun example-site-modus-css-output (_site _pages)
  "Return modus.css, the colours of Modus Vivendi, as one (URL . TEXT).
The theme is loaded but not enabled, so building the site does not
change how Emacs looks.  _SITE and _PAGES are not used."
  (load-theme 'modus-vivendi t t)
  (let ((color (lambda (name) (modus-themes-get-color-value name nil 'modus-vivendi))))
    (list (cons "modus.css"
                (concat ":root {\n  color-scheme: dark;\n"
                        (mapconcat (lambda (entry)
                                     (format "  %s: %s;\n" (car entry) (funcall color (cdr entry))))
                                   example-site-colors "")
                        "}\n\n"
                        (mapconcat (lambda (entry)
                                     (format ".%s { color: %s; }\n" (car entry)
                                             (funcall color (cdr entry))))
                                   example-site-code-colors ""))))))

(denden-define-site example-site
  "The Denden example site, a small blog."
  :title "A Plain Blog"
  :description "A minimal blog built with Denden."
  :author "Jane Doe"
  :base-url "https://denden.idlip.in/"
  :menu '(("Home" . "/") ("Blog" . "/blog/") ("Tags" . "/tags/") ("About" . "/about/"))
  :stylesheets '("/modus.css" "/style.css")
  :outputs (list #'denden-feed-output #'denden-term-outputs #'denden-sitemap-output
                 #'denden-robots-output #'denden-redirect-outputs
                 #'example-site-modus-css-output))

(provide 'example-site)
;;; example-site.el ends here

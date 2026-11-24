;;; denden-example-test.el --- Build tests on the example site -*- lexical-binding: t; -*-

;;; Commentary:

;; Build the example site in denden/example/ into a temporary folder,
;; check the structure of every page, and compare the files with the
;; golden copy in denden/test/golden/.  Save a new golden copy, after
;; looking at the build, with `just golden'.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'dom)
(require 'denden)
(require 'example-site)

(defconst denden-example-test--golden
  (expand-file-name "golden/" (file-name-directory (or load-file-name buffer-file-name)))
  "Folder of the golden copy of the example site.")

(defun denden-example-test--structure-problems (html)
  "Return the structure problems of the page HTML, as a list of strings.
A page needs exactly one h1, alt text on every image, and headings
that never skip a level on the way down."
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (levels (mapcar (lambda (heading)
                           (string-to-number (substring (symbol-name (dom-tag heading)) 1)))
                         (dom-search dom (lambda (node)
                                           (memq (dom-tag node) '(h1 h2 h3 h4 h5 h6))))))
         (problems nil))
    (unless (= (seq-count (lambda (level) (= level 1)) levels) 1)
      (push "not exactly one h1" problems))
    (dolist (image (dom-by-tag dom 'img))
      (unless (dom-attr image 'alt)
        (push (format "image %s has no alt text" (dom-attr image 'src)) problems)))
    (cl-loop for (previous next) on levels
             when (and next (> next (1+ previous)))
             do (push (format "heading level skips from h%d to h%d" previous next) problems))
    (nreverse problems)))

(defun denden-example-test--build (directory)
  "Build the example site into DIRECTORY and return the publishing folder."
  (let ((site (copy-sequence example-site))
        (denden-lastmod-from-git nil)
        (inhibit-message t)
        (process-environment (cons (concat denden-preview-base-url-variable "=")
                                   process-environment)))
    (setf (denden-site-publishing-directory site) (expand-file-name "public" directory))
    (denden-build site)))

(defun denden-example-test--files (directory)
  "Return the files under DIRECTORY, relative to it and sorted."
  (sort (mapcar (lambda (file) (file-relative-name file directory))
                (directory-files-recursively directory ""))))

(ert-deftest denden-example-test-pages-are-well-formed ()
  "Every page of the example has one h1, image alt text and no skipped levels."
  (ert-with-temp-directory directory
                           (let ((public (denden-example-test--build directory)))
                             (dolist (file (directory-files-recursively public (rx ".html" eos)))
                               (should (equal (cons (file-relative-name file public)
                                                    (denden-example-test--structure-problems
                                                     (with-temp-buffer (insert-file-contents file) (buffer-string))))
                                              (list (file-relative-name file public)))))
                             (should (string-search
                                      "<a class=\"u-url p-name\" href=\"/blog/hello-world/\">Hello, World</a>"
                                      (with-temp-buffer
                                        (insert-file-contents (expand-file-name "blog/index.html" public))
                                        (buffer-string)))))))

(ert-deftest denden-example-test-modus-colors ()
  "The example's colours and code colours come from Modus Vivendi."
  (ert-with-temp-directory directory
                           (let ((css (with-temp-buffer
                                        (insert-file-contents
                                         (expand-file-name "modus.css" (denden-example-test--build directory)))
                                        (buffer-string))))
                             (should (string-search "--background-color: #000000;" css))
                             (should (string-search ".org-keyword { color: #b6a0ff; }" css))
                             (should-not (memq 'modus-vivendi custom-enabled-themes)))))

(ert-deftest denden-example-test-matches-golden ()
  "A build of the example is the same, file by file, as the golden copy."
  (skip-unless (file-directory-p denden-example-test--golden))
  (ert-with-temp-directory directory
                           (let ((public (denden-example-test--build directory)))
                             (should (equal (denden-example-test--files public)
                                            (denden-example-test--files denden-example-test--golden)))
                             (dolist (file (denden-example-test--files public))
                               (should (equal (cons file (with-temp-buffer
                                                           (insert-file-contents (expand-file-name file public))
                                                           (buffer-string)))
                                              (cons file (with-temp-buffer
                                                           (insert-file-contents
                                                            (expand-file-name file denden-example-test--golden))
                                                           (buffer-string)))))))))

(provide 'denden-example-test)
;;; denden-example-test.el ends here

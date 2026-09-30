;;; example-async-init.el --- dedicated init file for the example's async build -*- lexical-binding: t; -*-

;; Loaded via `-Q -l' by `org-export-async-start' (see `example-build-async'
;; in example-site.el). Unlike denden/denden-async-init.el (denden's own
;; generic minimal init), this one also pulls in example-theme.el and
;; example-site.el, the same reasoning as site/site-async-init.el.

(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path here)
  (add-to-list 'load-path (expand-file-name ".." here)))
(require 'example-site)

(provide 'example-async-init)
;;; example-async-init.el ends here

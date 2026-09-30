;;; denden-async-init.el --- dedicated init file for denden's async build child -*- lexical-binding: t; -*-

;; Loaded via `-Q -l' (see `org-export-async-init-file' in denden.el), so
;; the child build process starts fast: no user init file, just this.

;; `-Q' means no user init file either, so nothing has set a preferred
;; coding system -- writing any of the nerd-font glyphs the theme uses
;; throughout its chrome then hits `select-safe-coding-system' asking an
;; interactive question with no terminal to answer it on, which hangs
;; reading from stdin until it errors out. Force utf-8 outright rather
;; than merely preferring it (`set-language-environment'/
;; `prefer-coding-system' alone still leave room for this to fire).
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'denden)

(provide 'denden-async-init)
;;; denden-async-init.el ends here

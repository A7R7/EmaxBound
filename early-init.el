;; [[file:config.org::*Earliest birds][Earliest birds:1]]
;;; early-init.el --- earliest birds  -*- lexical-binding: t; no-byte-compile: t -*-
;; Earliest birds:1 ends here

;; [[file:config.org::*Earliest birds][Earliest birds:2]]
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; Earliest birds:2 ends here

;; [[file:config.org::*Earliest birds][Earliest birds:3]]
(setq load-prefer-newer t)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "lib/compat" dir))
  (add-to-list 'load-path (expand-file-name "lib/packed" dir))
  (add-to-list 'load-path (expand-file-name "lib/auto-compile" dir)))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(setq package-enable-at-startup nil)

(with-eval-after-load 'package
  (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t))
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
;; Earliest birds:3 ends here

;; [[file:config.org::*Early Init][Early Init:1]]
;;; -*- lexical-binding: t; no-byte-compile: t -*-
;; Early Init:1 ends here

;; [[file:config.org::*Early Init][Early Init:2]]
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; Early Init:2 ends here

;; [[file:config.org::*Early Init][Early Init:3]]
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
;; Early Init:3 ends here

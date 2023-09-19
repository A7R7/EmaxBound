;; [[file:config.org::*Early Init][Early Init:1]]
;;; -*- lexical-binding: t; no-byte-compile: t -*-
;; Early Init:1 ends here

;; [[file:config.org::*Early Init][Early Init:2]]
(setq gc-cons-threshold most-positive-fixnum)
;; copied from lazycat
(setq gc-cons-percentage 0.6)
;; Early Init:2 ends here

;; [[file:config.org::*Early Init][Early Init:3]]
(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)
;; Early Init:3 ends here

;; [[file:config.org::*Early Init][Early Init:4]]
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)
;; Early Init:4 ends here

;; [[file:config.org::*Early Init][Early Init:5]]
(setq frame-inhibit-implied-resize t)
;; Early Init:5 ends here

;; [[file:config.org::*Early Init][Early Init:6]]
(setq use-package-enable-imenu-support t)
(setq use-package-verbose t)
;; Early Init:6 ends here

;; [[file:config.org::*Early Init][Early Init:7]]
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
;; Early Init:7 ends here

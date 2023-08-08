(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-ayu-mirage))
 '(custom-safe-themes
   '("512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02" default))
 '(dashboard-image-banner-max-width 1000)
 '(display-line-numbers-width 6)
 '(doom-ayu-mirage-brighter-modeline t)
 '(doom-modeline-enable-word-count t)
 '(elfeed-feeds
   '("https://karthinks.com/index.xml" "https://karthinks.com/"
     ("http://nullprogram.com/feed/" blog emacs)
     "http://www.50ply.com/atom.xml"
     ("http://nedroid.com/feed/" webcomic)))
 '(fill-column 90)
 '(mini-frame-internal-border-color "white")
 '(mini-frame-show-parameters '((left . 0.5) (top . 0.0) (width . 0.8) (height . 1)))
 '(mixed-pitch-fixed-pitch-faces
   '(diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face line-number line-number-current-line line-number-major-tick line-number-minor-tick markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-special-keyword org-table org-verbatim))
 '(mixed-pitch-set-height t)
 '(mouse-wheel-progressive-speed nil)
 '(olivetti-body-width nil)
 '(olivetti-style 'fancy)
 '(org-indent-indentation-per-level 3)
 '(org-modern-block-fringe nil)
 '(org-modern-block-name '("⇲ " . "⇱ "))
 '(org-modern-keyword nil)
 '(org-modern-list '((43 . "⯌") (45 . "⮚") (42 . "⊛")))
 '(org-modern-todo nil)
 '(package-selected-packages '(goto-chg epkg diff-hl dash auto-compile all-the-icons))
 '(scroll-conservatively 97)
 '(scroll-preserve-screen-position 1)
 '(vertico-count 20)
 '(vertico-posframe-border-width 3)
 '(vertico-posframe-width 140)
 '(vertico-resize nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-items-face ((t nil)))
 '(diff-hl-change ((t (:background "#2c5f72" :foreground "#77a8d9"))))
 '(diff-hl-delete ((t (:background "#844953" :foreground "#f27983"))))
 '(diff-hl-insert ((t (:background "#5E734A" :foreground "#a6cc70"))))
 '(dired-broken-symlink ((t (:background "dim gray" :foreground "yellow1" :weight bold))))
 '(doom-modeline ((t (:inherit variable-pitch))))
 '(doom-modeline-compilation ((t (:height 0.9 :inherit doom-modeline-warning))))
 '(doom-modeline-emphasis ((t (:inherit (variable-pitch doom-modeline mode-line-emphasis)))))
 '(fixed-pitch ((t (:slant normal :weight medium :height 160 :width normal :foundry "ABAT" :family "Cantarell"))))
 '(fixed-pitch-serif ((t (:height 180 :family "Monospace Serif"))))
 '(font-lock-comment-face ((t (:foreground "LightSteelBlue4" :slant italic))))
 '(line-number ((t (:weight normal :slant normal :foreground "LightSteelBlue4" :inherit default))))
 '(line-number-current-line ((t (:inherit (hl-line default) :foreground "#ffcc66" :slant italic :weight normal))))
 '(link ((t (:foreground "#ffcc66" :underline t :weight medium))))
 '(lsp-bridge-alive-mode-line ((t (:inherit (variable-pitch font-lock-constant-face) :weight bold))))
 '(meow-cheatsheet-command ((t (:height 180 :inherit fixed-pitch))))
 '(olivetti-fringe ((t (:background "#171B24"))))
 '(org-block-begin-line ((t (:height 0.8 :weight light :foreground "#3f4460" :extend t :inherit variable-pitch))))
 '(org-document-info ((t (:weight bold :height 1.8 :family "Cantarell"))))
 '(org-document-info-keyword ((t (:foreground "LightSteelBlue4" :inherit org-document-info))))
 '(org-meta-line ((t (:foreground "LightSteelBlue4"))))
 '(org-modern-block-name ((t (:inherit variable-pitch))))
 '(org-special-keyword ((t (:foreground "LightSteelBlue4"))))
 '(org-tag ((t (:foreground "LightSteelBlue4" :weight normal))))
 '(org-visual-indent-blank-pipe-face ((t (:background "#1f2430" :foreground "#1f2430" :height 0.1 :width extra-expanded))))
 '(org-visual-indent-pipe-face ((t (:background "slate gray" :foreground "slate gray" :height 0.1))))
 '(scroll-bar ((t (:background "gray13" :distant-foreground "black" :foreground "dim gray" :box nil :width condensed))))
 '(tooltip ((t (:height 1.5 :foreground "#cbccc6" :background "#171b24" :inherit variable-pitch))))
 '(variable-pitch ((t (:slant normal :weight regular :height 180 :width normal :foundry "TMC " :family "Arimo Nerd Font Propo"))))
 '(vertico-posframe-border ((t (:background "gray25")))))

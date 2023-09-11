(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-matching-delay 0.05)
 '(custom-enabled-themes '(doom-ayu-mirage))
 '(display-line-numbers-width 4)
 '(elfeed-feeds
	 '("https://manateelazycat.github.io/feed.xml"
		 ("http://nullprogram.com/feed/" blog emacs)
		 "http://www.50ply.com/atom.xml"
		 ("http://nedroid.com/feed/" webcomic)))
 '(highlight-indent-guides-delay 0.01)
 '(highlight-indent-guides-method 'bitmap)
 '(highlight-indent-guides-responsive 'top)
 '(mixed-pitch-fixed-pitch-faces
	 '(diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face line-number line-number-current-line line-number-major-tick line-number-minor-tick markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-special-keyword org-table org-verbatim widget-field org-modern-label))
 '(mixed-pitch-set-height nil)
 '(org-agenda-files nil)
 '(org-pretty-entities t)
 '(org-return-follows-link t)
 '(org-tags-column 0)
 '(tab-width 2)
 '(which-key-posframe-border-width 3))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1f2430" :foreground "#cad0e5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 181 :width normal :foundry "GOOG" :family "RobotoMono Nerd Font"))))
 '(bold ((t (:foreground "#dae0f5" :weight bold))))
 '(dashboard-items-face ((t nil)))
 '(dashboard-no-items-face ((t (:inherit dashboard-items-face))))
 '(diff-hl-insert ((t (:extend nil :background "#5E734A" :foreground "#a6cc70"))))
 '(diff-hl-margin-change ((t (:extend nil :inherit diff-hl-change))))
 '(font-lock-doc-face ((t (:foreground "LightSteelBlue4" :inherit font-lock-comment-face))))
 '(help-key-binding ((t (:height 0.95 :weight bold :box (:line-width (-1 . -1) :color "LightSteelBlue4") :foreground "LightBlue" :inherit fixed-pitch))))
 '(highlight-indent-guides-character-face ((t (:foreground "#3e445f" :weight thin))))
 '(highlight-indent-guides-top-character-face ((t (:foreground "LightSteelBlue4"))))
 '(info-colors-lisp-code-block ((t (:inherit org-block))))
 '(info-glossary-word ((t (:inherit link))))
 '(info-menu-header ((t (:weight bold))))
 '(info-menu-star ((t (:foreground "white" :weight bold))))
 '(line-number ((t (:weight normal :slant normal :foreground "LightSteelBlue4" :background "#171B24" :inherit default))))
 '(link ((t (:foreground "#ffcc66" :underline t :weight normal))))
 '(lsp-bridge-alive-mode-line ((t (:inherit (font-lock-constant-face variable-pitch) :weight regular))))
 '(mode-line ((t (:box nil :foreground "#cad0e5" :background "#141820" :inherit default))))
 '(org-agenda-date-today ((t (:foreground "#63d3ff" :weight ultra-bold))))
 '(org-agenda-dimmed-todo-face ((t (:foreground "LightSteelBlue4"))))
 '(org-agenda-structure ((t (:weight ultra-bold :foreground "#cad0e5"))))
 '(org-block-begin-line ((t (:foreground "#353d5d" :extend t :inherit variable-pitch))))
 '(org-block-end-line ((t (:extend t :inherit org-block-begin-line))))
 '(org-drawer ((t (:foreground "LightSteelBlue4"))))
 '(org-headline-done ((t (:foreground "LightSteelBlue4"))))
 '(org-meta-line ((t (:foreground "LightSteelBlue4" :height 0.8))))
 '(org-modern-date-inactive ((t (:inherit (org-modern-label default) :background "gray20"))))
 '(org-modern-horizontal-rule ((t (:strike-through "LightSteelBlue4" :inherit org-hide))))
 '(org-modern-label ((t (:box (:line-width (1 . 1) :color "#1f2430" :style flat-button) :underline nil :weight regular :width condensed))))
 '(org-modern-priority ((t (:weight semibold :inverse-video t :inherit (org-priority default)))))
 '(org-modern-statistics ((t nil)))
 '(org-modern-tag ((t (:inherit (secondary-selection org-modern-label) :extend nil :foreground "white"))))
 '(org-modern-time-inactive ((t (:foreground "gray5" :distant-foreground "gray95" :background "LightSteelBlue3" :inherit org-modern-label))))
 '(org-property-value ((t (:foreground "LightSteelBlue4"))))
 '(org-scheduled-previously ((t (:foreground "SteelBlue4"))))
 '(org-scheduled-today ((t (:foreground "#ffcc66"))))
 '(org-table ((t (:foreground "#bccbff"))))
 '(org-time-grid ((t (:foreground "LightSteelBlue1"))))
 '(org-upcoming-deadline ((t (:foreground "#ffcc66"))))
 '(outline-1 ((t (:extend t :foreground "#63d3ff" :weight bold))))
 '(outline-2 ((t (:extend t :foreground "#b0baff" :weight bold))))
 '(outline-3 ((t (:extend t :foreground "#c0cff3" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#b0baff" :inherit outline-2))))
 '(shadow ((t (:foreground "LightSteelBlue4"))))
 '(treemacs-directory-face ((t (:foreground "unspecified" :inherit treemacs-file-face))))
 '(treemacs-file-face ((t (:inherit variable-pitch :foreground "unspecified"))))
 '(treemacs-git-modified-face ((t (:foreground "#dccbff" :inherit treemacs-file-face))))
 '(treemacs-window-background-face ((t nil)))
 '(variable-pitch ((t (:family "Sarasa Gothic SC" :foundry "????" :width normal :height 180 :weight regular :slant normal :inherit default))))
 '(vertico-posframe-border ((t (:background "LightSteelBlue4"))))
 '(which-key-posframe-border ((t (:background "LightSteelBlue4"))))
 '(whitespace-empty ((t (:extend t :background "#1b202b" :foreground "SlateGray4"))))
 '(whitespace-indentation ((t (:foreground "#3b405b"))))
 '(whitespace-line ((t (:background "unspecified" :foreground "unspecified" :weight regular))))
 '(whitespace-newline ((t (:foreground "#2b304b"))))
 '(whitespace-space ((t (:foreground "#2b304b"))))
 '(whitespace-tab ((t (:foreground "#2b304b" :inherit whitespace-space)))))

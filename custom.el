(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-ayu-moonlight))
 '(dashboard-path-max-length 50)
 '(display-line-numbers-width 4)
 '(doom-modeline-modal nil)
 '(elfeed-feeds
   '("https://manateelazycat.github.io/feed.xml"
     ("http://nullprogram.com/feed/" blog emacs)
     "http://www.50ply.com/atom.xml"
     ("http://nedroid.com/feed/" webcomic)))
 '(highlight-indent-guides-delay 0.01)
 '(highlight-indent-guides-method 'bitmap)
 '(highlight-indent-guides-responsive 'top)
 '(highlight-parentheses-attributes '((:box (:line-width (-1 . -1)))))
 '(highlight-parentheses-colors nil)
 '(highlight-parentheses-delay 0.03)
 '(mixed-pitch-fixed-pitch-faces
   '(diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face line-number line-number-current-line line-number-major-tick line-number-minor-tick markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-special-keyword org-table org-verbatim widget-field org-modern-label))
 '(mixed-pitch-set-height nil)
 '(org-agenda-files nil)
 '(org-confirm-babel-evaluate t)
 '(org-pretty-entities t)
 '(org-return-follows-link t)
 '(org-tags-column 0)
 '(tab-width 2)
 '(use-package-minimum-reported-time 0.001)
 '(which-key-posframe-border-width 3))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((t (:box (:line-width (-1 . -1) :color "grey75")))) t)
 '(magit-diff-added-highlight ((t (:weight bold :foreground "#77e0c6" :background "#324853" :extend t :inherit fixed-pitch))))
 '(magit-diff-context-highlight ((t (:foreground "#d0d8f0" :background "#191a2a" :extend t :inherit fixed-pitch))))
 '(magit-diff-removed-highlight ((t (:weight bold :foreground "#ff757f" :background "#4e3444" :extend t :inherit fixed-pitch))))
 '(magit-hash ((t (:foreground "#7a88cf" :inherit fixed-pitch))))
 '(org-modern-tag ((t (:foreground "white" :inherit org-modern-label))))
 '(transient-key ((t (:inherit (font-lock-builtin-face fixed-pitch))))))

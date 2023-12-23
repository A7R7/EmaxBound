(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-ayu-moonlight))
 '(dashboard-path-max-length 50)
 '(org-modern-tag nil)
 '(org-tags-column 0)
 '(telega-server-libs-prefix "~/.nix-profile" t)
 '(warning-suppress-log-types '(((org-element org-element-parser)) (comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diredfl-date-time ((t (:foreground "#82aaff"))))
 '(diredfl-deletion-file-name ((t (:extend t :background "#4d3345" :foreground "#ff757f"))))
 '(diredfl-link-priv ((t (:foreground "#f989d3" :inherit fixed-pitch))))
 '(diredfl-no-priv ((t (:inherit (shadow fixed-pitch)))))
 '(diredfl-number ((t (:foreground "#ffa95f" :inherit fixed-pitch))))
 '(diredfl-read-priv ((t (:foreground "#ffd767" :inherit fixed-pitch))))
 '(diredfl-write-priv ((t (:foreground "#ff757f" :inherit fixed-pitch))))
 '(elfeed-search-unread-title-face ((t (:foreground "#d0d8f0" :weight semi-bold))))
 '(gomoku-X ((t (:foreground "cyan" :weight bold))))
 '(magit-diff-added ((t (:foreground "#5fb39e" :background "#293545" :extend t :inherit fixed-pitch))))
 '(magit-diff-conflict-heading ((t (:inherit (magit-diff-hunk-heading fixed-pitch)))))
 '(magit-diff-context ((t (:foreground "#7c8190" :background "#212337" :extend t :inherit fixed-pitch))))
 '(magit-diff-removed ((t (:foreground "#cc5d65" :background "#382c3d" :extend t :inherit fixed-pitch))))
 '(telega-delim-face ((t (:height 0.5 :underline (:color foreground-color :style line :position 10) :inherit shadow))))
 '(telega-msg-self-title ((t (:foreground "#ffa95f" :weight medium))))
 '(telega-msg-user-title ((t (:weight medium :foreground "#ffa95f" :inherit variable-pitch))))
 '(telega-shadow ((t (:inherit font-lock-comment-face))))
 '(transient-key ((t (:inherit (font-lock-builtin-face fixed-pitch))))))

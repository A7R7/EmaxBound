;;; init.el --- user-init-file
;; -*-lexical-binding: t; -*-
;; -*-no-byte-compile: t; -*-

;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)

  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
)

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize)
  (defun borg-assimilate-loop ()
    (interactive)
    (catch 'loop-end
      (while t ; Infinite loop until explicitly terminated
        (condition-case nil
          (borg-assimilate ()) ; Call your function here
          (quit (throw 'loop-end nil)))))
  ) ; Catch 'C-g' (quit) signal and end the loop
)

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash
  :config (global-dash-fontify-mode))

(use-package eieio)

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
)

(use-package epkg
  :defer t
  :init
  (setq epkg-repository
        (expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time) before-user-init-time))))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook 'indent-spaces-mode))

(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?x?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(use-package smerge-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t)))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook 'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here

  (set-face-attribute 'default nil
    :font "JetBrainsMono Nerd Font"
    :height 150
    :weight 'medium)
  (set-face-attribute 'variable-pitch nil
    :font "Ubuntu Nerd Font"
    :height 160
    :weight 'medium)
  (set-face-attribute 'fixed-pitch nil
    :font "JetBrainsMono Nerd Font"
    :height 150
    :weight 'medium)

  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
    :slant 'italic)

(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(use-package doom-themes
  :init
  (load-theme 'doom-ayu-mirage t))

(set-frame-parameter nil 'alpha-background 95)
(add-to-list 'default-frame-alist '(alpha-background . 95))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package dashboard
:init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; (setq dashboard-banner-logo-title "Also try NeoVim!")
  ;; show Dashboard in frames created with emacsclient -c
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "~/.config/emacs/images/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content

  (setq dashboard-items '(
        (recents . 5)
        (agenda . 5 )
        (bookmarks . 3)
        (projects . 3)
        (registers . 3)
  ))

  ;; (dashboard-modify-heading-icons '((recents . "file-text") (bookmarks . "book")))
:config
  (dashboard-setup-startup-hook)
)

(use-package diminish)

  (use-package centaur-tabs
    :hook
      (emacs-startup . centaur-tabs-mode)
    :init
      (setq centaur-tabs-set-icons t
	    centaur-tabs-set-modified-marker t
	    centaur-tabs-modified-marker "M"
	    centaur-tabs-cycle-scope 'tabs
	    centaur-tabs-set-close-button nil
	    centaur-tabs-enable-ido-completion nil)
    :config
      (centaur-tabs-mode t)
      ;; (centaur-tabs-headline-match)
      (centaur-tabs-group-by-projectile-project)
  )

(use-package shrink-path :demand t)

(use-package evil
  :init
    (setq evil-want-integration t) ;; t by default
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
  :config
    (evil-mode 1)
      ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
)

(use-package evil-collection
  ;; :demand t
  :after evil
  :config
  ;(setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor
  :demand t)

(use-package emacs
  :config (setq ring-bell-function #'ignore)
)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
:after evil
:config
  ;; (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer leader-bind
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  (leader-bind
      "b"  '(:ignore t                          :wk "Buffer")
      "bb" '(switch-to-buffer                   :wk "Switch buffer")
      "bd" '(kill-this-buffer                   :wk "Delete buffer")
      "bn" '(next-buffer                        :wk "Next buffer")
      "bp" '(previous-buffer                    :wk "Prev buffer")
      "br" '(revert-buffer                      :wk "Reload buffer")
      "]"  '(next-buffer                        :wk "Next buffer")
      "["  '(previous-buffer                    :wk "Prev buffer")

      "w"  '(:ignore t                          :wk "Window")
      "wd" '(delete-window                      :wk "Delete window")
      "wv" '(split-window-vertically            :wk "Vertical split")
      "wh" '(split-window-horizontally          :wk "Horizontal split")
      "wh" '(evil-window-left                   :wk "window <")
      "wj" '(evil-window-down                   :wk "window v")
      "wk" '(evil-window-up                     :wk "window ^")
      "wl" '(evil-window-right                  :wk "window >")
  )
)

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-idle-secondary-delay 0.05
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → "
  )
)

(use-package evil-org)

(use-package org-roam
:after org
:init
  (setq org-roam-directory (file-truename "~/roam"))
  (setq org-roam-v2-ack t)
)

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :init
    ;; (setq org-superstar-headline-bullets-list '("✖" "✚" "◉" "○" "▶")
	  ;; org-superstar-special-todo-items t
	  ;; org-ellipsis " ↴ ")
  )

(use-package org-fancy-priorities)

;; (use-package org-pandoc)

(use-package dirvish
    :init (dirvish-override-dired-mode)
)

(use-package eaf
;; :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
:custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
:config
  (defalias 'browse-web #'eaf-open-browser)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
) ;; unbind, see more in the Wiki

(use-package eaf-browser)       ;;M-x eaf-file-browser-qrcode
(use-package eaf-git)       ;;M-x eaf-file-browser-qrcode
(use-package eaf-file-manager)  ;;M-x eaf-open-in-file-manager
(use-package eaf-pdf-viewer)

(setq gc-cons-threshold (* 2 1000 1000))

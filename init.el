  ;;; init.el --- user-init-file

  ;;; -*- lexical-binding: t; no-byte-compile: t -*-
  (progn ;     startup
    (defvar before-user-init-time (current-time)
      "Value of `current-time' when Emacs begins loading `user-init-file'.")
    (message "Loading Emacs...done (%fs)"
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
    ;; smooth window on startup
    (setq frame-inhibit-implied-resize t)
    ;; Defer garbage collection further back in the startup process
    (setq gc-cons-threshold most-positive-fixnum)
    ;; copied from lazycat
    (setq gc-cons-percentage 0.6)
    ;; Prevent flashing of unstyled modeline at startup
    (setq-default mode-line-format nil)
    (setq confirm-kill-emacs 'y-or-n-p)
  )

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize)
)

(setq use-package-verbose t)
  ;(eval-and-compile ; `use-package'
  ;  (require  'use-package)
  ;  (setq use-package-verbose t))

(use-package dash
  :config (global-dash-fontify-mode))

(use-package eieio)

  (use-package auto-compile
    :config
    (setq auto-compile-display-buffer             nil
  		auto-compile-mode-line-counter            t
  		auto-compile-source-recreate-deletes-dest t
  		auto-compile-toggle-deletes-nonlib-dest   t
  		auto-compile-update-autoloads             t
  		warning-suppress-log-types        '((comp))
    )
  )

(use-package epkg
  :defer t
  :bind
     ([remap describe-package] . epkg-describe-package)
  :init
  (setq epkg-repository
	(expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector 'sqlite-builtin ))

  (use-package custom
    :no-require t
    :config
    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
    (setf custom-safe-themes t) ;Treat all themes as safe
    (when (file-exists-p custom-file)
      (load custom-file)))

(use-package server
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

  (use-package org)

(progn ;     startup
  (message "Loading early birds...done (%fs)"
	   (float-time (time-subtract (current-time) before-user-init-time))))

(use-package shrink-path :demand t)

  (use-package meow
    :custom-face
    (meow-cheatsheet-command ((t (:height 180 :inherit fixed-pitch))))
    :config
    ;; Replicate the behavior of vi's
    (defun my-meow-append ()
      "Move to the end of selection, switch to INSERT state."
      (interactive)
      (if meow--temp-normal
  	(progn
  	  (message "Quit temporary normal mode")
  	  (meow--switch-state 'motion))
        (if (not (region-active-p))
  	  (when (and (not (use-region-p))
  		     (< (point) (point-max)))
  	    (forward-char 1))
  	(meow--direction-forward)
  	(meow--cancel-selection))
        (meow--switch-state 'insert)))
    (advice-add 'meow-append :override #'my-meow-append)
    
    (setq meow-replace-state-name-list
            '((normal . "🅝")
              (beacon . "🅑")
              (insert . "🅘")
              (motion . "🅜")
              (keypad . "🅚")))
    (setq meow-keypad-self-insert-undefined nil)
    (setq meow-selection-command-fallback '(
          (meow-change . meow-change-char)
          (meow-kill . meow-delete)
          (meow-cancel-selection . keyboard-quit)
          (meow-pop-selection . meow-pop-grab)
          (meow-beacon-change . meow-beacon-change-char)
          (meow-replace . meow-yank)
          (meow-reverse . negative-argument)
    ))
    (defun meow-setup ()
      (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

      (meow-motion-overwrite-define-key
       '("<escape>" . ignore))

      (meow-leader-define-key
       ;; SPC j/k will run the original command in MOTION state.
       '("i" . meow-open-above)
       '("j" . meow-insert)
       '("k" . meow-open-below)
       '("l" . meow-append) 
      
       ;; Use SPC (0-9) for digit arguments.
       '("1" . meow-digit-argument) '("2" . meow-digit-argument)
       '("3" . meow-digit-argument) '("4" . meow-digit-argument)
       '("5" . meow-digit-argument) '("6" . meow-digit-argument)
       '("7" . meow-digit-argument) '("8" . meow-digit-argument)
       '("9" . meow-digit-argument) '("0" . meow-digit-argument)
       '("/" . meow-keypad-describe-key) '("?" . meow-cheatsheet))

      (meow-normal-define-key
       '("1" . meow-expand-1) '("2" . meow-expand-2)
       '("3" . meow-expand-3) '("4" . meow-expand-4)
       '("5" . meow-expand-5) '("6" . meow-expand-6)
       '("7" . meow-expand-7) '("8" . meow-expand-8)
       '("9" . meow-expand-9) '("0" . meow-expand-0)
       '("-" . negative-argument) '(";" . meow-reverse)
       '("," . meow-inner-of-thing) '("." . meow-bounds-of-thing)
       '("/" . meow-visit)
       '("[" . meow-beginning-of-thing) '("]" . meow-end-of-thing)
       '("b" . meow-block) '("B" . meow-to-block)
       '("c" . meow-save) '("C" . meow-sync-grab)
       '("D" . meow-open-below)
       '("E" . meow-open-above)
       '("F" . meow-append)
       '("g" . meow-find) '("G" . meow-grab)
       '("h" . meow-line) '("H" . meow-goto-line)
       '("i" . meow-prev) '("I" . meow-prev-expand)
       '("j" . meow-left) '("J" . meow-left-expand)
       '("k" . meow-next) '("K" . meow-next-expand)
       '("l" . meow-right) '("L" . meow-right-expand)
       '("m" . meow-mark-word) '("M" . meow-mark-symbol)
       '("n" . meow-search)
       '("o" . meow-next-word) '("O" . meow-next-symbol)
       '("p" . meow-pop-selection)
       '("q" . meow-quit)
       '("S" . meow-insert)
       '("t" . meow-till)
       '("u" . meow-back-word) '("U" . meow-back-symbol)
       '("v" . meow-replace) '("V" . meow-yank-pop)
       '("x" . meow-kill)
       '("y" . meow-join)
       '("z" . meow-undo) '("Z" . meow-undo-in-selection)
       '("'" . repeat) '("<escape>" . meow-cancel-selection)))
    (meow-setup)
    (meow-global-mode)
    )

  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (use-package general
    :config
    ;; (general-evil-setup)
    ;; set up 'SPC' as the global leader key

    (general-evil-setup t)
    (general-create-definer config/leader
      :states '(normal insert visual emacs)
      ;:keymaps 'meow-keypad-state-keymap
      :keymaps 'override
      ;:prefix "SPC" ;; set leader
      :global-prefix "M-SPC" ;; access leader in insert mode
    )

    (config/leader
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key"))

    ;; buffers
    (config/leader :infix "b"
      ""        '(nil                            :wk "  Buffer ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key")
      "b"       '(switch-to-buffer               :wk " Switch ")
      "d"       '(kill-this-buffer               :wk "󰅖 Delete ")
      "r"       '(revert-buffer                  :wk "󰑓 Reload ")
      "["       '(previous-buffer                :wk " Prev ")
      "]"       '(next-buffer                    :wk " Next ")
      )
    ;; centaur tabs
    (config/leader
      "{"       '(centaur-tabs-backward-group    :wk " Prev Group")
      "}"       '(centaur-tabs-forward-group     :wk " Next Group")
      "["       '(centaur-tabs-backward          :wk " Prev Buffer ")
      "]"       '(centaur-tabs-forward           :wk " Next Buffer ")
      )
    ;; builtin-tabs
    (config/leader :infix "TAB"
      ""        '(nil                            :wk " 󰓩 Tab ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key")
      "TAB"     '(tab-new                        :wk "󰝜 Tab New ")
      "d"       '(tab-close                      :wk "󰭌 Tab Del ")
      "["       '(tab-previous                   :wk " Prev ")
      "]"       '(tab-next                       :wk " Next ")
      )
    ;; windows
    (config/leader :infix "w"
      ""        '(nil                            :wk " 󰓩 Tab ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key")
      "d"       '(delete-window                  :wk "󰅖 Delete  ")
      "v"       '(split-window-vertically        :wk "󰤻 Split   ")
      "s"       '(split-window-horizontally      :wk "󰤼 Split   ")
      "\\"      '(split-window-vertically        :wk "󰤻 Split   ")
      "|"       '(split-window-horizontally      :wk "󰤼 Split   ")
      "h"       '(evil-window-left               :wk " Focus H ")
      "j"       '(evil-window-down               :wk " Focus J ")
      "k"       '(evil-window-up                 :wk " Focus K ")
      "l"       '(evil-window-right              :wk " Focus L ")
      )
    ;; Borg
    (config/leader :infix "B"
      ""        '(nil                            :wk " 󰏗 Borg      ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key   ")
      "a"       '(borg-assimilate                :wk "󱧕 Assimilate ")
      "A"       '(borg-activate                  :wk " Activate   ")
      "b"       '(borg-build                     :wk "󱇝 Build      ")
      "c"       '(borg-clone                     :wk " Clone      ")
      "r"       '(borg-remove                    :wk "󱧖 Remove     ")
      )
    ;; toggle
    (config/leader :infix "t"
      ""        '(nil                            :wk " 󰭩 Toggle    ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key   ")
      )
    ;; quit
    (config/leader :infix "q"
      ""        '(nil                            :wk " 󰗼 Quit      ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key   ")
      "q"       '(save-buffers-kill-terminal     :wk "󰗼 Quit Emacs ")
      )
    ;; Git
    (config/leader :infix "g"
      ""        '(nil                            :wk " 󰊢 Git       ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key   ")
      "g"       '(magit                          :wk " Magit      ")
      )
    ;; dired
    (config/leader
      "e"       '(dirvish-side                   :wk "󰙅 Dirvish-side ")
      ;;"E"       '(dirvish                        :wk " Dirvish      ")
      ;;"qe"      '(save-buffers-kill-emacs         :wk "Quit Emacs ")
      ;;"e"       '(treemacs                        :wk "󰙅 Treemacs ")
      )
    (config/leader
      "/"       '(evilnc-comment-or-uncomment-lines :wk "󱀢 Comment ")
      )
    )

  (use-package which-key
  :after general
  :init
    (setq
      which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-location 'bottom
      which-key-side-window-slot -10
      which-key-side-window-max-height 0.25
      which-key-idle-delay 0.8
      which-key-idle-secondary-delay 0.01
      which-key-max-description-length 25
      which-key-allow-imprecise-window-fit t
      ;which-key-separator " → "
      which-key-separator " "
      Which-key-show-early-on-C-h t
      which-key-sort-order 'which-key-prefix-then-key-order
      which-key-show-transient-maps t
   )
    ;(general-define-key
    ;:keymaps 'which-key-mode-map
    ;  "DEL" '(which-key-undo :wk "undo")
    ;)
    (which-key-mode 1)
  )

  (use-package which-key-posframe
  :config
    (setq which-key-posframe-poshandler
        'posframe-poshandler-window-bottom-center
        ;'posframe-poshandler-frame-bottom-center
    )
    (which-key-posframe-mode)
  )

  (use-package key-echo
  :disabled
  :config
    (key-echo-enable)
  )

  (set-face-attribute 'default nil
    ;:font "JetBrainsMono Nerd Font"
    :font "RobotoMono Nerd Font"
    ;:font "Sarasa Term SC Nerd"
    ;:font "Sarasa Gothic SC"
    :height 180
  )
  (set-face-attribute 'variable-pitch nil
    :font "Sarasa Gothic SC"
    :height 180
  )
  (set-face-attribute 'fixed-pitch nil
    ;:font "Sarasa Fixed SC"
    :font "RobotoMono Nerd Font"
    :height 180
  )
  (set-face-attribute 'fixed-pitch-serif t
    :family "Monospace Serif"
    :height 180
  )

  (set-face-attribute 'font-lock-comment-face nil
    :foreground "LightSteelBlue4" :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

  (set-face-attribute 'link nil
    :foreground "#ffcc66" :underline t :bold nil)

 (use-package emacs
   :init
     (global-set-key (kbd "C-=")            'text-scale-increase)
     (global-set-key (kbd "C--")            'text-scale-decrease)
     (global-set-key (kbd "<C-wheel-up>")   'text-scale-increase)
     (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
 )

  (use-package fixed-pitch)

  (use-package mixed-pitch-mode
  :defer t
  :config
  :hook (Custom-mode . mixed-pitch-mode)
    (setq  mixed-pitch-set-height t)
  )

(use-package all-the-icons
  :if (display-graphic-p))

;(use-package all-the-icons-dired
;  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)

  (use-package olivetti
  :hook (org-mode . olivetti-mode)
    (Custom-mode . olivetti-mode)
    (help-mode . olivetti-mode)
    (dashboard-mode . olivetti-mode)
    (dashboard-mode . variable-pitch-mode)
    (olivetti-mode . visual-line-mode)
  :init
    (setq-default fill-column 74)
  :config
    ;If nil (the default), use the value of fill-column + 2.
    (setq olivetti-body-width nil
	         olivetti-style 'fancy)
    (set-face-attribute 'olivetti-fringe nil :background "#171B24")

    (config/leader
      "tc"  '(olivetti-mode     :wk "󰉠 Center")
    )
  )

  (use-package topspace
  :init (global-topspace-mode)
  )

  (use-package solaire-mode
    :hook (minibuffer-setup . solaire-mode)
          (help-mode . solaire-mode)
          (helpful-mode . solaire-mode)
          (org-export-stack-mode . solaire-mode)
    )

  (config/leader :infix "t"
    "SPC"  '(whitespace-mode  :wk "󰡭 Show Space")
  )

  (set-frame-parameter nil 'alpha-background 96)
  (add-to-list 'default-frame-alist '(alpha-background . 96))

  (defun config/transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque:")
    (set-frame-parameter nil 'alpha-background value))

  (use-package emacs
  :config
    (setq scroll-conservatively 97)
    (setq scroll-preserve-screen-position 1)
    (setq mouse-wheel-progressive-speed nil)
    ;; The following piece of code is stolen from
    ;; https://emacs-china.org/t/topic/25114/5
    (pixel-scroll-precision-mode 1)
    (setq pixel-scroll-precision-interpolate-page t)
    (defun +pixel-scroll-interpolate-down (&optional lines)
        (interactive)
        (if lines
            (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
        (pixel-scroll-interpolate-down)))

    (defun +pixel-scroll-interpolate-up (&optional lines)
        (interactive)
        (if lines
            (pixel-scroll-precision-interpolate (* lines  
            (pixel-line-height))))
        (pixel-scroll-interpolate-up))

    (defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
    (defalias 'scroll-down-command '+pixel-scroll-interpolate-up)
  )

  (use-package doom-modeline
  :init
    (setq
      doom-modeline-height 37
      doom-modeline-enable-word-count t)
    (doom-modeline-mode 1)
  :config
    (set-face-attribute 'doom-modeline t
      :inherit 'variable-pitch
    )
  )

(use-package diminish)

  (use-package dashboard
  :init
    (setq initial-buffer-choice 'dashboard-open
  	dashboard-image-banner-max-width 1000
  	dashboard-set-heading-icons t
  	dashboard-center-content t ;; set to 't' for centered content
  	dashboard-set-file-icons t
  	initial-buffer-choice
  	    (lambda () (get-buffer-create "*dashboard*"))
  	dashboard-startup-banner ;; use custom image as banner
  	    (concat user-emacs-directory "assets/EmacsBound.xpm")
  	dashboard-items '(
  	    (recents . 5)
  	    (agenda . 5 )
  	    (bookmarks . 3)
  	    (projects . 3)
  	    (registers . 3)
  	)
    )
  :config
    (dashboard-setup-startup-hook)
  :bind (:map dashboard-mode-map
    ("i" . 'dashboard-previous-line)
    ("k" . 'dashboard-next-line)
    ("l" . 'dashboard-return)
    ("j" . 'dashboard-remove-item-under)
    )
  )

  (use-package centaur-tabs
    :hook
      (emacs-startup . centaur-tabs-mode)
      (dired-mode . centaur-tabs-local-mode)
      (dirvish-directory-view-mode . centaur-tabs-local-mode)
      (dashboard-mode . centaur-tabs-local-mode)
    :init
      (setq centaur-tabs-set-icons t
	    centaur-tabs-set-modified-marker t
	    centaur-tabs-modified-marker "M"
	    centaur-tabs-cycle-scope 'tabs
	    centaur-tabs-set-bar 'over
	    centaur-tabs-enable-ido-completion nil
      )
      (centaur-tabs-mode t)
    :config
      (centaur-tabs-change-fonts "Sarasa Gothic SC" 160)
      ;; (centaur-tabs-headline-match)
      ;; (centaur-tabs-group-by-projectile-project)

  )

  (use-package diff-hl
  :custom-face
    (diff-hl-change ((t (:background "#2c5f72" :foreground "#77a8d9"))))
    (diff-hl-delete ((t (:background "#844953" :foreground "#f27983"))))
    (diff-hl-insert ((t (:background "#5E734A" :foreground "#a6cc70"))))
  :config
    (setq diff-hl-draw-borders nil)
    (global-diff-hl-mode)
    ;(diff-hl-margin-mode) 
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
  )

  (use-package beacon
  :config  
    (beacon-mode)
  )

  (use-package mini-frame
  :config
    (setq mini-frame-detach-on-hide nil)
    ;(setq mini-frame-standalone 't)
    ;(setq mini-frame-resize-min-height 10)
    (setq mini-frame-ignore-commands
      (append mini-frame-ignore-commands
       '(evil-window-split evil-window-vsplit evil-ex)))
  )

  (use-package holo-layer
    :disabled
    :config 
    (holo-layer-enable)
  )

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

  (use-package magit
    :defer t
    :commands (magit-add-section-hook)
    :hook (magit-mode . solaire-mode) (magit-mode . olivetti-mode)
    :config
    (magit-add-section-hook 'magit-status-sections-hook
  			  'magit-insert-modules
  			  'magit-insert-stashes
  			  'append))

  (use-package treemacs
  :disabled
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs
            (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex
            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method
            'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file
            (expand-file-name ".cache/treemacs-persist"
             user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories
             '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  )

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  )

(use-package treemacs-magit
  :after (treemacs magit)
  )

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

  (use-package dirvish
  :init
    (dirvish-override-dired-mode)
  :hook
    (dired-mode . solaire-mode)
  :custom
    (dirvish-quick-access-entries ;`setq' won't work for custom
      '(("h" "~/"                          "Home")
  	("d" "~/Downloads/"                "Downloads")
  	("m" "/mnt/"                       "Drives")
  	("t" "~/.local/share/Trash/files/" "TrashCan"))
    )
  :config
    (dirvish-define-preview exa (file)
    "Use `exa' to generate directory preview."
    :require ("exa") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
  	`(shell . ("exa" "-al" "--color=always" "--icons"
  		"--group-directories-first" ,file))))

    (add-to-list 'dirvish-preview-dispatchers 'exa)
    ;; (dirvish-peek-mode) ; Preview files in minibuffer
    ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
    (setq dirvish-path-separators (list "  " "  " "  "))
    (setq dirvish-mode-line-format
  	    '(:left (sort symlink) :right (omit yank index)))
    (setq dirvish-attributes
  	    '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
    (setq delete-by-moving-to-trash t)
    (setq dired-listing-switches
  	    "-l --almost-all --human-readable --group-directories-first --no-group")
    (nmap dirvish-mode-map
  	"?"      '(dirvish-dispatch          :wk "Dispatch")
  	"TAB"    '(dirvish-subtree-toggle    :wk "Subtre-toggle")
  	"q"      '(dirvish-quit              :wk "Quit")
  	"h"      '(dired-up-directory        :wk "Up-dir")
  	"l"      '(dired-find-file           :wk "Open/Toggle")
  	"a"      '(dirvish-quick-access      :wk "Access")
  	"f"      '(dirvish-file-info-menu    :wk "File Info Menu")
  	"y"      '(dirvish-yank-menu         :wk "Yank Menu")
  	"N"      '(dirvish-narrow            :wk "Narrow")
  	;         `dired-view-file'
  	"v"      '(dirvish-vc-menu           :wk "View-file")
  	;         `dired-sort-toggle-or-edit'
  	"s"      '(dirvish-quicksort         :wk "Quick-sort")

  	"M-f"    '(dirvish-history-go-forward  :wk "History-forward")
  	"M-b"    '(dirvish-history-go-backward :wk "History-back")
  	"M-l"    '(dirvish-ls-switches-menu    :wk "ls Switch Menu")
  	"M-m"    '(dirvish-mark-menu           :wk "Mark Menu")
  	"M-t"    '(dirvish-layout-toggle       :wk "Layout-toggle")
  	"M-s"    '(dirvish-setup-menu          :wk "Setup-Menu")
  	"M-e"    '(dirvish-emerge-menu         :wk "Emerge-Menu")
  	"M-j"    '(dirvish-fd-jump             :wk "fd-jump")
    )
  )

  (use-package diredfl
    :hook
    ((dired-mode . diredfl-mode)
     ;; highlight parent and directory preview as well
     (dirvish-directory-view-mode . diredfl-mode))
    :config
    (set-face-attribute 'diredfl-dir-name nil :bold t)
  )

  (use-package helpful
  :bind
     ([remap describe-key]      . helpful-key)
     ([remap describe-command]  . helpful-command)
     ([remap describe-variable] . helpful-variable)
     ([remap describe-function] . helpful-callable)
  )

  (use-package marginalia
  :general
    (:keymaps 'minibuffer-local-map
     "M-A" 'marginalia-cycle)
  :custom
    (marginalia-max-relative-age 0)
    (marginalia-align 'right)
  :init
    (marginalia-mode)
  )

  (use-package vertico
    :init
    ;; Different scroll margin
    (setq vertico-scroll-margin 1)
    ;; Show more candidates
    (setq vertico-count 20)
    ;; Grow and shrink the Vertico minibuffer
    (setq vertico-resize nil)
    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    (setq vertico-cycle t)
    ;; use Vertico as an in-buffer completion UI
    (setq completion-in-region-function 'consult-completion-in-region)
    (vertico-mode 1)
  )

  (use-package orderless
    :init
    (setq completion-styles '(orderless))
    (setq orderless-component-separator 
  		#'orderless-escapable-split-on-space)
    (setq orderless-matching-styles
  		'(orderless-initialism orderless-prefixes orderless-regexp))
    )

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  (use-package vertico-multiform
    :after vertico
    :config (vertico-multiform-mode)
  )

  (use-package vertico-posframe
  ;:disabled
  :after vertico-multiform
  :init
    (setq vertico-posframe-poshandler
          'posframe-poshandler-frame-top-center)
    (setq vertico-count 15
          vertico-posframe-border-width 3
          vertico-posframe-width 140
          vertico-resize nil)
    (setq vertico-posframe-parameters
         '((left-fringe . 20)
           (right-fringe . 20)))
    (setq vertico-multiform-commands '(
          (execute-extended-command ; M-x
            (vertico-posframe-poshandler .
               posframe-poshandler-frame-top-center)
            (vertico-posframe-width . 120)
          )
          (meow-visit
            (vertico-posframe-poshandler .
               posframe-poshandler-window-top-right-corner)
            (vertico-posframe-width . 50)
          )
          (meow-yank-pop; M-x
            (vertico-posframe-poshandler .
               posframe-poshandler-point-window-center)
            (vertico-posframe-width . 50)
          )
          (find-file
            (vertico-count . 25)
            (vertico-posframe-width . 70)
            (vertico-posframe-poshandler .
               posframe-poshandler-window-center)
          )
          (org-insert-link; M-x
            (vertico-posframe-poshandler .
               posframe-poshandler-point-top-left-corner)
            (vertico-posframe-width . 70)
          )
          (consult-line
            (vertico-posframe-poshandler . 
               posframe-poshandler-frame-top-center)
            (vertico-posframe-fallback-mode . vertico-buffer-mode)
            (vertico-posframe-width . 70))
          (t
            (vertico-posframe-poshandler .
               posframe-poshandler-frame-top-center)
            (vertico-posframe-width . 120)
          )
    ))

  :config
    (vertico-multiform-mode 1)
    (vertico-posframe-mode 1)
  )

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
    (use-package savehist
	:init
	(savehist-mode))

  ;; A few more useful configurations...
    (use-package emacs
	:init
	;; Add prompt indicator to `completing-read-multiple'.
	;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
	(defun crm-indicator (args)
	(cons (format "[CRM%s] %s"
			(replace-regexp-in-string
			"\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
			crm-separator)
			(car args))
		(cdr args)))
	(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

	;; Do not allow the cursor in the minibuffer prompt
	(setq minibuffer-prompt-properties
	    '(read-only t cursor-intangible t face minibuffer-prompt))
	(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

	;; Emacs 28: Hide commands in M-x which do not work in the current mode.
	;; Vertico commands are hidden in normal buffers.
	;; (setq read-extended-command-predicate
	;;       #'command-completion-default-include-p)
	;; Enable recursive minibuffers
	(setq enable-recursive-minibuffers t))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ;("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ;("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

  (use-package blink-search
  :config
    (setq blink-search-enable-posframe t)
  )

  (use-package color-rg
  :config
    (general-def isearch-mode-map
      "M-s M-s" 'isearch-toggle-color-rg
    )
  )

  (use-package lsp-bridge
  :init
    (global-lsp-bridge-mode)
  :config
    ;(set-face-attributes 'lsp-bridge-alive-mode-line nil
    ;  :inherit 'variable-pitch
    ;)
  )

  (use-package treesit
  :commands (treesit-install-language-grammar  
             config/treesit-install-all-languages)
  :init
    (setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (common-lisp . 
          ("https://github.com/theHamsta/tree-sitter-commonlisp"))
        (csharp     . 
          ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . 
          ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . 
          ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . 
          ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript .   
          ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . 
          ("https://github.com/MDeiml/tree-sitter-markdown" nil   
          "tree-sitter-markdown/src"))
        (ocaml . 
            ("https://github.com/tree-sitter/tree-sitter-ocaml" nil 
            "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . 
            ("https://github.com/tree-sitter/tree-sitter-typescript" nil 
            "typescript/src"))
        (tsx . 
            ("https://github.com/tree-sitter/tree-sitter-typescript" nil 
            "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun config/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75)))))
  ;; stolen from lazycat
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (css-mode        . css-ts-mode)
          (js-mode         . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (sh-mode         . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (rust-mode       . rust-ts-mode)
          ))

  (add-hook 'markdown-mode-hook #'(lambda () 
  				  (treesit-parser-create 'markdown)))

  (add-hook 'web-mode-hook #'(lambda ()
  			     (let ((file-name (buffer-file-name)))
  			       (when file-name
  				 (treesit-parser-create
  				  (pcase (file-name-extension file-name)
  				    ("vue" 'vue)
  				    ("html" 'html)
  				    ("php" 'php))))
  			       )))

  (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  (add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  (add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
  (add-hook 'go-mode-hook #'(lambda () (treesit-parser-create 'go)))
  (add-hook 'java-mode-hook #'(lambda () (treesit-parser-create 'java)))
  (add-hook 'java-ts-mode-hook #'(lambda () (treesit-parser-create 'java)))
  (add-hook 'php-mode-hook #'(lambda () (treesit-parser-create 'php)))
  (add-hook 'php-ts-mode-hook #'(lambda () (treesit-parser-create 'php)))

  (use-package treesit-auto
  :disabled
  :config
    (global-treesit-auto-mode))

  (use-package emacs
  :custom-face
    (line-number ((t (
      :weight normal :slant normal :foreground "LightSteelBlue4"     
      :inherit default))))
    (line-number-current-line ((t (
      :inherit (hl-line default) :slant normal :foreground "#ffcc66"))))
  :hook (prog-mode . config/toggle-line-number-absolute)
  :config
    (defun config/toggle-line-number-nil ()
      (interactive)
      (setq display-line-numbers nil)
    )
    (defun config/toggle-line-number-absolute ()
      (interactive)
      (setq display-line-numbers t)
    )
    (defun config/toggle-line-number-relative ()
      (interactive)
      (setq display-line-numbers 'relative)
    )
    (defun config/toggle-line-number-visual ()
      (interactive)
      (setq display-line-numbers 'visual)
    )
    (config/leader :infix "tl"
      ""    '(nil                                :wk "  Line Number ")
      "DEL" '(which-key-undo                     :wk "󰕍 Undo key   ")
      "n"   '(config/toggle-line-number-nil      :wk "󰅖 Nil        ")
      "a"   '(config/toggle-line-number-absolute :wk "󰱇 Absolute   ")
      "r"   '(config/toggle-line-number-relative :wk "󰰠 Relative   ")
      "v"   '(config/toggle-line-number-visual   :wk " Visual     ")
      "h"   '(hl-line-mode                       :wk "󰸱 Hl-line")
    )
  )



  (use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

  (use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
    (setq highlight-indent-guides-method 'character
          highlight-indent-guides-character 9474 
          highlight-indent-guides-auto-enabled nil
    )
    (set-face-attribute 'highlight-indent-guides-character-face nil
      :foreground "LightSteelBlue4")
    (set-face-attribute 'highlight-indent-guides-top-character-face nil
      :foreground "#ffcc66")

  )

  (use-package smartparens
  :config
    (smartparens-global-mode)
  )

  (use-package fingertip
  :config
    (dolist (hook (list
		'c-mode-common-hook 'c-mode-hook 'c++-mode-hook
		'c-ts-mode-hook 'c++-ts-mode-hook
		'cmake-ts-mode-hook
		'java-mode-hook
		'haskell-mode-hook
		'emacs-lisp-mode-hook 
             'lisp-interaction-mode-hook 'lisp-mode-hook
		'maxima-mode-hook
		'ielm-mode-hook
		'bash-ts-mode-hook 'sh-mode-hook
		'makefile-gmake-mode-hook
		'php-mode-hook
		'python-mode-hook 'python-ts-mode-hook
		'js-mode-hook
		'go-mode-hook
		'qml-mode-hook
		'jade-mode-hook
		'css-mode-hook 'css-ts-mode-hook
		'ruby-mode-hook
		'coffee-mode-hook
		'rust-mode-hook 'rust-ts-mode-hook
		'qmake-mode-hook
		'lua-mode-hook
		'swift-mode-hook
		'web-mode-hook
		'markdown-mode-hook
		'llvm-mode-hook
		'conf-conf-mode-hook 'conf-ts-mode-hook
		'nim-mode-hook
		'typescript-mode-hook 'typescript-ts-mode-hook
		'js-ts-mode-hook 'json-ts-mode-hook
		))
    (add-hook hook #'(lambda () (fingertip-mode 1))))
    (general-def
      :keymaps 'fingertip-mode-map
	"(" 'fingertip-open-round
	"[" 'fingertip-open-bracket
	"{" 'fingertip-open-curly
	")" 'fingertip-close-round
	"]" 'fingertip-close-bracket
	"}" 'fingertip-close-curly
	"=" 'fingertip-equal

	"%" 'fingertip-match-paren
	"\"" 'fingertip-double-quote
	"'" 'fingertip-single-quote

	"SPC" 'fingertip-space
	"RET" 'fingertip-newline

	"M-o" 'fingertip-backward-delete
	"C-d" 'fingertip-forward-delete
	"C-k" 'fingertip-kill

	"M-\"" 'fingertip-wrap-double-quote
	"M-'" 'fingertip-wrap-single-quote
	"M-[" 'fingertip-wrap-bracket
	"M-{" 'fingertip-wrap-curly
	"M-(" 'fingertip-wrap-round
	"M-)" 'fingertip-unwrap

	"M-p" 'fingertip-jump-right
	"M-n" 'fingertip-jump-left
	"M-:" 'fingertip-jump-out-pair-and-newline

	"C-j" 'fingertip-jump-up
    )
  )

  (use-package aggressive-indent
  :config
    (global-aggressive-indent-mode 1)
  )

(use-package yasnippet
  :init
  (yas-global-mode 1)
)

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'text-mode
    ;; expand unconditionally
    ";o-" "ō"
    ";i-" "ī"
    ";a-" "ā"
    ";u-" "ū"
    ";e-" "ē")
  (aas-set-snippets 'latex-mode
    ;; set condition!
    :cond #'texmathp ; expand only while in math
    "supp" "\\supp"
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"
    "Olon" "O(n \\log n)"
    ;; Use YAS/Tempel snippets with ease!
    "amin" '(yas "\\argmin_{$1}") ; YASnippet snippet shorthand form
    "amax" '(tempel "\\argmax_{" p "}") ; Tempel snippet shorthand form
    ;; bind to functions!
    ";ig" #'insert-register
    ";call-sin"
    (lambda (angle) ; Get as fancy as you like
      (interactive "sAngle: ")
      (insert (format "%s" (sin (string-to-number angle))))))
  ;; disable snippets by redefining them with a nil expansion
  (aas-set-snippets 'latex-mode
    "supp" nil))

  (use-package lsp-bridge
  :config
    (setq lsp-bridge-tex-lsp-server "digestif")
  )

(use-package auctex)

(use-package laas
  :hook (LaTeX-mode . laas-mode))

  (use-package org
  :custom-face
    (org-latex-and-related ((t (:foreground "LightSteelBlue4" :weight bold))))
    (org-meta-line ((t (:foreground "LightSteelBlue4"))))
    (org-special-keyword ((t (:foreground "LightSteelBlue4"))))
    (org-tag ((t (:foreground "LightSteelBlue4" :weight normal))))
  :hook (org-mode . mixed-pitch-mode)
  :config
    (set-face-attribute 'org-level-1 nil
	:family "Sarasa Gothic SC" :height 1.8 )
    (set-face-attribute 'org-level-2 nil
	:family "Sarasa Gothic SC" :height 1.6 )
    (set-face-attribute 'org-level-3 nil
	:family "Sarasa Gothic SC" :height 1.4 )
    (set-face-attribute 'org-level-4 nil
	:family "Sarasa Gothic SC" :height 1.3 )
    (set-face-attribute 'org-level-5 nil
	:family "Sarasa Gothic SC" :height 1.2 )
    (set-face-attribute 'org-level-6 nil
	:family "Sarasa Gothic SC" :height 1.1 )
    (set-face-attribute 'org-document-title nil
	:family "Sarasa Gothic SC" :height 2.5 :bold t)
    (set-face-attribute 'org-document-info nil
	:family "Sarasa Gothic SC" :height 1.8 :bold t)
    (set-face-attribute 'org-document-info-keyword nil
      :foreground "LightSteelBlue4" :inherit 'org-document-info)
    (set-face-attribute 'org-block t
      :extend t :inherit 'fixed-pitch)
  )

  (use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
     (setq org-modern-keyword
       (quote (("author" . "⛾")
	       ("title" . "❖")
	       ("subtitle" . "◈")
	       ("html" . "󰅱 ")
	       (t . t))))
     (setq org-modern-star
	  ;'("◉" "○" "◈" "◇" "✳")
	  '("⚀" "⚁" "⚂" "⚃" "⚄" "⚅")
	  ;'("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")
     )
     (setq org-modern-list ;; for '+' '-' '*' respectively
	 '((43 . "⯌") (45 . "⮚") (42 . "⊛"))
     )
     (setq org-modern-block-fringe nil)
     (setq org-modern-todo nil)
     (setq org-modern-block-name '("⇲ " . "⇱ "))
     (set-face-attribute 'org-modern-block-name nil
	:inherit 'variable-pitch)
     (setq org-modern-table nil)
  )

  (use-package org-superstar
  :defer t
  ;:hook (org-mode . org-superstar-mode)
  :init
    (setq
      ;;org-superstar-headline-bullets-list '("󰇊" "󰇋" "󰇌" "󰇍" "󰇎" "󰇏")
      org-superstar-special-todo-items t
      ;;org-ellipsis "  "
    )
  )

  (use-package org-bars
  :defer t
  :commands 'org-bars-mode
  ;:hook (org-mode . org-bars-mode)
  :custom-face
    (org-visual-indent-blank-pipe-face ((t (:background "#1f2430" :foreground "#1f2430" :height 0.1 :width extra-expanded))))
    (org-visual-indent-pipe-face ((t (:background "slate gray" :foreground "slate gray" :height 0.1))))
  :config
    (setq org-bars-color-options '(
	  :desaturate-level-faces 100
	  :darken-level-faces 10
    ))
    (setq org-bars-extra-pixels-height 25)
    (setq org-bars-stars '(:empty "" :invisible "" :visible ""))
  )

  (use-package org-visual-outline
  :hook (org-mode . org-visual-indent-mode)
  )

  (use-package valign
  :hook (org-mode . valign-mode)
  :config
    (setq valign-fancy-bar t)
  )

  (use-package hl-todo
    :init
    (hl-todo-mode)
  )

  (use-package org-fancy-priorities)

  (use-package org-appear
    :hook (org-mode . org-appear-mode)
    :init
    (setq org-appear-autoemphasis  t
          ;org-appear-autolinks t
          org-appear-autosubmarkers t
          org-appear-autoentities t
          org-appear-autokeywords t
          org-appear-inside-latex t
          org-hide-emphasis-markers t
    )
  )

  (use-package org
  :init
    (setq electric-indent-mode nil)
  :config
    (setq org-src-tab-acts-natively t)
    (setq org-src-preserve-indentation nil)
  )

  (use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  )

  (use-package org-capture
  :after org
  :bind (("C-c c" . org-capture))
  :init
    (setq org-directory "~/Org")
  :config
    (setq org-capture-templates (append org-capture-templates '(
      ("j" "Journal"
        entry (file+datetree "~/Org/journal.org")
        "* %U - %^{heading}\n  %?")
      ("i" "Inbox"
        entry (file "~/Dropbox/org/inbox.org")
        "* %U - %^{heading} %^g\n %?\n")
      ("n" "Notes"
        entry (file "~/Dropbox/org/notes/inbox.org")
        "* %^{heading} %t %^g\n  %?\n")
    )))
  )

  (use-package org-super-links
  :after org
  :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link))
  )

  (use-package org-roam
  :after org
  :init
    (setq org-roam-directory (file-truename "~/roam"))
    (setq org-roam-v2-ack t)
    (setq org-roam-capture-templates '(
            ("d" "default" plain "%?"
              :target (file+head 
                "%<%Y%m%d%H>-${slug}.org"
                "#+title: ${title}\n#+filetags: \n")
              :unnarrowed t)
            ("b" "book notes" plain "%?"
              :target (file+head 
                "book/book%<%Y%m%d%H>-${slug}.org"
                "#+title: ${title}\n#+filetags: :bookreading: \n\n")
              :unnarrowed t)
            ("c" "company" plain "%?"
              :target (file+head 
                "company/company%<%Y%m%d%H>-${slug}.org"
                "#+title: ${title}\n#filetags: :compnay: \n\n")
              :unnarrowed t)
            ("i" "industry" plain "%?"
              :target (file+head 
                "industry/industry%<%Y%m%d%H>-${slug}.org"
                "#+title:${slug}\n#+filetags: :industry: \n\n")
              :unnarrowed t)
            ("m" "marketing" plain "%?"
              :target (file+head 
                "marketing/marketing%<%Y%m%d%H%M%S>-${slug}.org"
                "#+title: ${title}\n#+filetags: :marketing: \n\n")
              :unnarrowed t)
            ("p" "project" plain "%?"
              :target (file+head 
                "project/project%<%Y%m%d%H>-${slug}.org"
                "#+title: ${title}\n#+filetags: :project: \n\n - tag ::")
              :unnarrowed t)
            ("r" "reference" plain "%?"
              :target (file+head 
                "<%Y%m%d%H>-${slug}.org"
                "#+title: {$title}\n%filetags: reference \n\n -tag ::")
              :unarrowed t)))    
  )

  (use-package org-gtd
  :defer t
  :after org
  :init
    (setq org-gtd-update-ack "3.0.0")
  )

  (use-package org
  :after emacs
  :init
    (setq org-element-cache-persistent nil)
    (setq org-element-use-cache nil)
    (setq org-latex-preview-numbered t)
    (plist-put org-latex-preview-options :zoom 1.25)
    (let ((pos (assoc 'dvisvgm org-latex-preview-process-alist)))
      (plist-put (cdr pos) :image-converter '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f")))
  :config
    (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  )

  (use-package org-fragtog
  :defer t
  :config
      ;; Vertically align LaTeX preview in org mode
      (defun my-org-latex-preview-advice (beg end &rest _args)
      (let* ((ov (car (overlays-at (/ (+ beg end) 2) t)))
	      (img (cdr (overlay-get ov 'display)))
	      (new-img (plist-put img :ascent 95)))
	  (overlay-put ov 'display (cons 'image new-img))))
      (advice-add 'org--make-preview-overlay
		  :after #'my-org-latex-preview-advice)

      ;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/
      ;; Justifying-LaTeX-preview-fragments-in-org-mode/
      ;; specify the justification you want
      (plist-put org-format-latex-options :justify 'right)

      (defun eli/org-justify-fragment-overlay (beg end image imagetype)
      (let* ((position (plist-get org-format-latex-options :justify))
	      (img (create-image image 'svg t))
	      (ov (car (overlays-at (/ (+ beg end) 2) t)))
	      (width (car (image-display-size (overlay-get ov 'display))))
	      offset)
	  (cond
	  ((and (eq 'center position)
	      (= beg (line-beginning-position)))
	  (setq offset (floor (- (/ fill-column 2)
				  (/ width 2))))
	  (if (< offset 0)
	      (setq offset 0))
	  (overlay-put ov 'before-string (make-string offset ? )))
	  ((and (eq 'right position)
	      (= beg (line-beginning-position)))
	  (setq offset (floor (- fill-column
				  width)))
	  (if (< offset 0)
	      (setq offset 0))
	  (overlay-put ov 'before-string (make-string offset ? ))))))
      (advice-add 'org--make-preview-overlay
		  :after 'eli/org-justify-fragment-overlay)

      ;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/
      ;; Better-equation-numbering-in-LaTeX-fragments-in-org-mode/
      (defun org-renumber-environment (orig-func &rest args)
      (let ((results '())
	      (counter -1)
	      (numberp))
	  (setq results (cl-loop for (begin .  env) in
	      (org-element-map (org-element-parse-buffer)
		  'latex-environment
		  (lambda (env)
		  (cons
		      (org-element-property :begin env)
		      (org-element-property :value env))))
	      collect
	      (cond
		  ((and (string-match "\\\\begin{equation}" env)
		      (not (string-match "\\\\tag{" env)))
		  (cl-incf counter)
		  (cons begin counter))
		  ((and (string-match "\\\\begin{align}" env)
		      (string-match "\\\\notag" env))
		  (cl-incf counter)
		  (cons begin counter))
		  ((string-match "\\\\begin{align}" env)
		  (prog2
		      (cl-incf counter)
		      (cons begin counter)
		  (with-temp-buffer
		      (insert env)
		      (goto-char (point-min))
		      ;; \\ is used for a new line. Each one leads
		      ;; to a number
		      (cl-incf counter (count-matches "\\\\$"))
		      ;; unless there are nonumbers.
		      (goto-char (point-min))
		      (cl-decf counter
			      (count-matches "\\nonumber")))))
		  (t
		  (cons begin nil)))))
	  (when (setq numberp (cdr (assoc (point) results)))
	  (setf (car args)
		  (concat
		  (format "\\setcounter{equation}{%s}\n" numberp)
		  (car args)))))
      (apply orig-func args))
      (advice-add 'org-create-formula-image :around #'org-renumber-environment)
  )

  (use-package grip-mode
  :after org
  )

  (use-package ox
    :config
    (setq org-export-with-toc t
  		org-export-with-tags 'not-in-toc
  		org-export-with-drawers nil
  		org-export-with-priority t
  		org-export-with-footnotes t
  		org-export-with-smart-quotes t
  		org-export-with-section-numbers t
  		org-export-with-sub-superscripts '{})
    (setq org-export-use-babel t
  		org-export-headline-levels 9
  		org-export-coding-system 'utf-8
  		org-export-with-broken-links 'mark)
    )

  (use-package ox-html
    :defer t
    :after ox
    :config
    (setq org-html-doctype "html5"
  		org-html-html5-fancy t
  		org-html-checkbox-type 'unicode
  		org-html-validation-link nil))

  (use-package htmlize
    :config
    (setq htmlize-pre-style t
  		htmlize-output-type 'inline-css))



  (use-package ox-latex
  :defer t
  :after ox
  )

  (use-package ox-reveal
  :defer t
  :after ox
  :config
    (setq org-reveal-hlevel 1
  		org-reveal-theme "moon"
  		org-reveal-mathjax t
  		org-reveal-ignore-speaker-notes t
  		org-reveal-title-slide "<h1><font size=\"8\">%t</font></h1><h2><font size=\"6\">%s</font></h2><p><font size=\"5\">%a</font><br/><font size=\"5\">%d</font></p>"
  		org-reveal-plugins '(markdown zoom notes search)
  		org-reveal-klipsify-src 'on))

  (use-package ox-gfm
  :defer t
  :after ox
  )

  (use-package ox-pandoc
  :defer t
  :after ox
  :config
    (setq org-pandoc-format-extensions 
  	      '(markdown_github+pipe_tables+raw_html)
  		org-pandoc-command "/usr/local/bin/pandoc")
    )

  (use-package ox-publish)

  (use-package ox-hugo
    :after ox
    :init (setq org-hugo-base-dir "~/Blog")
    :config
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
       See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) 
  		   (fname (org-hugo-slug title)))
  	  (mapconcat #'identity
          `(,(concat "* TODO " title) ":PROPERTIES:"
            ,(concat ":EXPORT_FILE_NAME: " fname) ":END:" "%?\n")
            ;Place the cursor here finally
          "\n"
        )
      )
    )

    (add-to-list 'org-capture-templates
  	'("h"
        "Hugo post"
        entry
  	  (file+olp "capture.org" "Notes")
  	  (function org-hugo-new-subtree-post-capture-template)))    
  )

  (use-package easy-hugo
    )

  (use-package elfeed
  :config
    (setq elfeed-feeds '(
	("http://nullprogram.com/feed/" blog emacs)
	"http://www.50ply.com/atom.xml"  ; no autotagging
	("http://nedroid.com/feed/" webcomic)
      )
    )
  )

  (use-package eaf
  :disabled
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
    (setq confirm-kill-processes nil)
  ) ;; unbind, see more in the Wiki

  ;(use-package eaf-browser)
  ;(use-package eaf-pdf-viewer)

  (add-hook 'tetris-mode-hook
    (defun config/tetris-center ()
      (config/window-center 76)
    )
  )

  (add-hook '2048-mode-hook
    (defun config/2048-center ()
      (config/window-center 35)
    )
  )

(progn ;     startup
  (message "Loading %s...done (%fs)" user-init-file
	   (float-time (time-subtract (current-time)
				      before-user-init-time)))
  (add-hook 'after-init-hook
	    (lambda ()
	      (message
	       "Loading %s...done (%fs) [after-init]" user-init-file
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

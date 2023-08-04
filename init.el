;;; init.el --- user-init-file
;;; -*- lexical-binding: t; no-byte-compile: t -*-

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
  ;; Defer garbage collection further back in the startup process
  (setq gc-cons-threshold most-positive-fixnum)
  ;; Prevent flashing of unstyled modeline at startup
  (setq-default mode-line-format nil)
)

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize)
;  (defun borg-assimilate-loop ()
;    (interactive)
;    (catch 'loop-end
;      (while t ; Infinite loop until explicitly terminated
;        (condition-case nil
;          (borg-assimilate ()) ; Call your function here
;          (quit (throw 'loop-end nil)))))
;  ) ; Catch 'C-g' (quit) signal and end the loop
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

(use-package evil
  :init
    (setq evil-want-integration t) ;; t by default
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (setq evil-want-C-u-scroll t)

  :config
    (evil-mode 1)
      ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (evil-define-key 'normal 'foo-mode "e" 'baz)
)

(use-package evil-collection
  ;; :demand t
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  ;(setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor
  :demand t)

(use-package emacs
  :config (setq ring-bell-function #'ignore)
)

(use-package undo-tree
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
:after evil
:config
  ;; (general-evil-setup)
  ;; set up 'SPC' as the global leader key

  (general-evil-setup t)
  (general-create-definer config/leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (config/leader
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key"))

  (config/leader :infix "b"
      ""        '(nil                            :wk "  Buffer ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key")
      "b"       '(switch-to-buffer               :wk " Switch ")
      "d"       '(kill-this-buffer               :wk "󰅖 Delete ")
      "r"       '(revert-buffer                  :wk "󰑓 Reload ")
      "["       '(previous-buffer                :wk " Prev ")
      "]"       '(next-buffer                    :wk " Next ")
  )
  (config/leader
      "{"       '(centaur-tabs-backward-group    :wk " Prev Group")
      "}"       '(centaur-tabs-forward-group     :wk " Next Group")
      "["       '(centaur-tabs-backward          :wk " Prev Buffer ")
      "]"       '(centaur-tabs-forward           :wk " Next Buffer ")
  )

  (config/leader :infix "TAB"
      ""        '(nil                            :wk " 󰓩 Tab ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key")
      "TAB"     '(tab-new                        :wk "󰝜 Tab New ")
      "d"       '(tab-close                      :wk "󰭌 Tab Del ")
      "["       '(tab-previous                   :wk " Prev ")
      "]"       '(tab-next                       :wk " Next ")
  )

  (config/leader :infix "w"
      ""        '(nil                            :wk " 󰓩 Tab ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key")
      "d"       '(delete-window                  :wk "󰅖 Delete  ")
      "v"       '(split-window-vertically        :wk "󰤻 Split   ")
      "s"       '(split-window-horizontally      :wk "󰤼 Split   ")
      "h"       '(evil-window-left               :wk " Focus H ")
      "j"       '(evil-window-down               :wk " Focus J ")
      "k"       '(evil-window-up                 :wk " Focus K ")
      "l"       '(evil-window-right              :wk " Focus L ")
      "\\"      '(split-window-vertically        :wk "󰤻 Split   ")
      "|"       '(split-window-horizontally      :wk "󰤼 Split   ")
  )
  (config/leader :infix "w"
      "\\"      '(split-window-vertically        :wk "󰤻 Split   ")
      "|"       '(split-window-horizontally      :wk "󰤼 Split   ")
  )

  (config/leader :infix "B"
      ""        '(nil                            :wk " 󰏗 Borg      ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key   ")
      "a"       '(borg-assimilate                :wk "󱧕 Assimilate ")
      "A"       '(borg-activate                  :wk " Activate   ")
      "b"       '(borg-build                     :wk "󱇝 Build      ")
      "c"       '(borg-clone                     :wk " Clone      ")
      "r"       '(borg-remove                    :wk "󱧖 Remove     ")
  )

  (config/leader :infix "t"
      ""        '(nil                            :wk "  Toggle    ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key   ")
  )

  (config/leader :infix "q"
      ""        '(nil                            :wk " 󰗼 Quit      ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key   ")
      "q"       '(save-buffers-kill-terminal     :wk "󰗼 Quit Emacs ")
  )

  (config/leader :infix "g"
      ""        '(nil                            :wk " 󰊢 Git       ")
      "DEL"     '(which-key-undo                 :wk "󰕍 Undo key   ")
      "g"       '(magit                          :wk " Magit      ")
  )

  (config/leader 
      "e"       '(dirvish-side                   :wk "󰙅 Dirvish-side ")
      "E"       '(dirvish                        :wk " Dirvish      ")
      ;"qe"      '(save-buffers-kill-emacs         :wk "Quit Emacs ")
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
  )
  ;(general-define-key
  ;:keymaps 'which-key-mode-map
  ;  "DEL" '(which-key-undo :wk "undo")
  ;)
  (which-key-mode 1)
)

(use-package which-key-posframe
:config
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (which-key-posframe-mode)
)

(set-face-attribute 'default nil
  ;:font "JetBrainsMono Nerd Font"
  :font "RobotoMono Nerd Font"
  ;:font "Sarasa Term SC Nerd"
  :height 155
  ;:weight 'medium
)
(set-face-attribute 'variable-pitch nil
  :font "Sarasa Gothic SC"
  :height 180
)
(set-face-attribute 'fixed-pitch nil
  :font "Sarasa Gothic SC"
  :height 180
)

(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

(use-package emacs
  :init 
    (global-set-key (kbd "C-=")            'text-scale-increase)
    (global-set-key (kbd "C--")            'text-scale-decrease)
    (global-set-key (kbd "<C-wheel-up>")   'text-scale-increase)
    (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
)

(use-package all-the-icons
  :ensure t
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
:config
    (defun config/adjust-olivetti-body-width ()
    (when (and (boundp 'mixed-pitch-mode) mixed-pitch-mode)
        (setq olivetti-body-width 54)))

    (add-hook 'olivetti-mode-hook 'config/adjust-olivetti-body-width)

    (config/leader
    "tc"  '(olivetti-mode     :wk "󰉠 Center")
    )
)

(use-package topspace
:hook (dashboard-mode . topspace-mode)
)

(set-frame-parameter nil 'alpha-background 96)
(add-to-list 'default-frame-alist '(alpha-background . 96))

(defun config/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value))

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
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))

(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)

(use-package doom-modeline
:init 
  (setq doom-modeline-height 37)
  (doom-modeline-mode 1)
:config
)

(use-package diminish)

(use-package dashboard
:init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; show Dashboard in frames created with emacsclient -c
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner (concat user-emacs-directory "assets/EmacsBound.svg"))  ;; use custom image as banner
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
    (centaur-tabs-change-fonts "Cantarell" 160)
    ;; (centaur-tabs-headline-match)
    ;; (centaur-tabs-group-by-projectile-project)

)

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
   ""        '(nil                                :wk "  Line Number ")
   "DEL"     '(which-key-undo                     :wk "󰕍 Undo key ")
   "n"       '(config/toggle-line-number-nil      :wk "󰅖 Nil        ")
   "a"       '(config/toggle-line-number-absolute :wk "󰯫 Absolute   ")
   "r"       '(config/toggle-line-number-relative :wk " Relative   ")
   "v"       '(config/toggle-line-number-visual   :wk " Visual     ")
)

(use-package vertico
  :init
  (setq completion-styles '(orderless))
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-matching-styles
      '(orderless-initialism orderless-prefixes orderless-regexp))
  ;; Different scroll margin
  ;(setq vertico-scroll-margin 1)
  ;; Show more candidates
  ;(setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;(setq vertico-resize nil)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  ;; use Vertico as an in-buffer completion UI
  (setq completion-in-region-function 'consult-completion-in-region)
  (vertico-mode 1)
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
:disabled
:after vertico-multiform
:init 
  (setq vertico-multiform-commands
       '((consult-line posframe
            (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
            ;(vertico-posframe-fallback-mode . vertico-buffer-mode)
            (vertico-posframe-width . 50))
         (execute-extended-command
            (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
            (vertico-posframe-fallback-mode . vertico-buffer-mode))
         (t posframe)
        )
  )
  ;(vertico-multiform-mode 1)
  ;(vertico-posframe-mode 1)
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

;(add-hook 'minibuffer-setup-hook 'solaire-mode)

(add-hook 'minibuffer-setup-hook
  (defun config/set-minibuffer-margin ()
    (setq olivetti-body-width 140)
    (olivetti-mode)
  )
)

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
)

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
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

(use-package yasnippet
  :init
  (yas-global-mode 1)
)

(use-package lsp-bridge
  :init
  (global-lsp-bridge-mode)
)

(use-package org
  :hook (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'org-document-title nil :family "Cantarell" :height 2.5 :bold t)
  (set-face-attribute 'org-level-1 nil :family "Cantarell" :height 1.8 :bold t)
  (set-face-attribute 'org-level-2 nil :family "Cantarell" :height 1.6 :bold t)
  (set-face-attribute 'org-level-3 nil :family "Cantarell" :height 1.4 :bold t)
  (set-face-attribute 'org-level-4 nil :family "Cantarell" :height 1.3 )
  (set-face-attribute 'org-level-5 nil :family "Cantarell" :height 1.2 )
  (set-face-attribute 'org-level-6 nil :family "Cantarell" :height 1.1 )
)

(use-package org-modern
:hook (org-mode . org-modern-mode)
:config
   (setq org-modern-keyword
     (quote (("author" . "⛾")
             ("title" . "❖")
             ("subtitle" . "§")
             (t . t))))
   (setq org-modern-star
        ;'("◉" "○" "◈" "◇" "✳")
        '("⚀" "⚁" "⚂" "⚃" "⚄" "⚅")
        ;'("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")
   )
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

(use-package hl-todo
  :init
  (hl-todo-mode)
)

(use-package org-fancy-priorities)

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autoemphasis  t)
  ;(setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-inside-latex t)
  (setq org-hide-emphasis-markers t)
)

(use-package evil-org)

(use-package org
:config
    (setq org-src-tab-acts-natively t)
    (setq org-src-preserve-indentation nil)
)

(use-package valign
:hook (org-mode . valign-mode)
)

(use-package org-roam
:after org
:init
  (setq org-roam-directory (file-truename "~/roam"))
  (setq org-roam-v2-ack t)
)

;; (use-package org-pandoc)

(use-package dirvish
:init
  (dirvish-override-dired-mode)
:custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
    '(("h" "~/"                          "Home")
      ("d" "~/Downloads/"                "Downloads")
      ("m" "/mnt/"                       "Drives")
      ("t" "~/.local/share/Trash/files/" "TrashCan"))
  )
:config
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
      "TAB"    '(dirvish-subtree-toggle    :wk "Subtre-toggle")
      "q"      '(dirvish-quit              :wk "Quit")
      "h"      '(dired-up-directory        :wk "Up-dir")
      "l"      '(dired-find-file           :wk "Open/Toggle")
      "a"      '(dirvish-quick-access      :wk "Access")
      "f"      '(dirvish-file-info-menu    :wk "File Info Menu")
      "y"      '(dirvish-yank-menu         :wk "Yank Menu")
      "N"      '(dirvish-narrow            :wk "Narrow")
      "v"      '(dirvish-vc-menu           :wk "View-file") ; remapped `dired-view-file'
      "s"      '(dirvish-quicksort         :wk "Quick-sort"); remapped `dired-sort-toggle-or-edit'

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
;      :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
;      (("C-c f" . dirvish-fd)
;       :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
;       ("a"   . dirvish-quick-access)
;       ("f"   . dirvish-file-info-menu)
;       ("y"   . dirvish-yank-menu)
;       ("N"   . dirvish-narrow)
;       ("^"   . dirvish-history-last)
;       ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;       ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;       ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;       ("TAB" . dirvish-subtree-toggle)
;       ("M-f" . dirvish-history-go-forward)
;       ("M-b" . dirvish-history-go-backward)
;       ("M-l" . dirvish-ls-switches-menu)
;       ("M-m" . dirvish-mark-menu)
;       ("M-t" . dirvish-layout-toggle)
;       ("M-s" . dirvish-setup-menu)
;       ("M-e" . dirvish-emerge-menu)
;       ("M-j" . dirvish-fd-jump))

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

(use-package shrink-path :demand t)

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

(use-package eaf
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

(use-package eaf-browser)
(use-package eaf-pdf-viewer)

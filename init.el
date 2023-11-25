;;; -*- lexical-binding: t; no-byte-compile: t -*-

(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  ;; (switch-to-buffer "*Messages*")
  (message "Loading Emacs...done (%fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
)

(progn
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  ;; (setq initial-scratch-message "")
  ;; This improves performance for some fonts
  (setq inhibit-compacting-font-cache t)
  ;; emacs.stackexchange/how-to-disable-emacs-bidi
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t
        long-line-threshold 1000
        large-hscroll-threshold 1000
        syntax-wholeline-max 1000)
  (setq global-auto-revert-mode 1)
  (setq use-dialog-box nil)
  (setq confirm-kill-emacs 'y-or-n-p)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  )

(use-package borg
:init
  (add-to-list 'load-path
    (expand-file-name "lib/borg" user-emacs-directory))
:config
  (borg-initialize)
  (switch-to-buffer "*Messages*")
)

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
  (setq epkg-database-connector 'sqlite-builtin ) ; requires emacs >=29
)

(use-package custom
:no-require t
:config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setf custom-safe-themes t) ;Treat all themes as safe
  (when (file-exists-p custom-file)
    (load custom-file))
)

(use-package server
:commands (server-running-p)
:config (or (server-running-p) (server-mode)))

(use-package gcmh
:init
  (setq gcmh-high-cons-threshold 536870912) ;; 512mb
:config
  (gcmh-mode 1)
  )

(use-package dash
  :config (global-dash-fontify-mode))

(use-package eieio)

(use-package shrink-path :demand t)

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

(use-package helpful
:bind
   ([remap describe-key]      . helpful-key)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable)
   ("C-h F" . describe-face)
   ("C-h K" . describe-keymap)
)

(use-package info+
:defer t
:config
)

(use-package info-colors
:config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node)
  (add-hook 'Info-mode-hook 'olivetti-mode)
  (add-hook 'Info-mode-hook 'mixed-pitch-mode)
)

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

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?x?:"))

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

(use-package pixel-scroll
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

(progn ;     startup
  (message "Loading core units...done (%fs)"
     (float-time (time-subtract (current-time) before-user-init-time))))

;; [[file:config.org::*General.el][General.el:1]]
;; Make ESC quit prompts
;; (global-set-key ([kbd] "<escape>") 'keyboard-escape-quit)

(use-package general
:config
  ;; [[file:config.org::*General.el][]]
  (general-def
  :keymaps '(global-map)
    "C-v"       '(clipboard-yank              :wk "paste")
    "C-SPC"     '(toggle-input-method         :wk "input method")
    "C-j"       'backward-char
    "C-;"       'forward-char
    "C-k"       'previous-line
    "C-l"       'next-line
    ;"C-/"      '(yank                        :wk "comment-dwim")
  )
  (general-def
  :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
    "M-j"       '(windmove-left               :wk " Win H ")
    "M-k"       '(windmove-up                 :wk " Win K ")
    "M-l"       '(windmove-down               :wk " Win J ")
    "M-;"       '(windmove-right              :wk " Win L ")
    "M-,"       '(sort-tab-select-prev-tab    :wk " Tab L ")
    "M-."       '(sort-tab-select-next-tab    :wk " Tab R ")
  )
  
  (general-def
  :keymaps '(vertico-map)
    "C-l"       '(vertico-next                  :wk "")
    "C-k"       '(vertico-previous              :wk "")
    "C-j"       '(vertico-directory-delete-word :wk "")
    "C-;"       '(vertico-directory-enter       :wk "")
    "C-,"       '(vertico-previous-group        :wk "")
    "C-."       '(vertico-next-group            :wk "")
    "RET"       'vertico-directory-enter
    "DEL"       'vertico-directory-delete-char
    "M-DEL"     'vertico-directory-delete-word
  )
  ;; ends here
  ;; [[file:config.org::*General.el][]]
  (general-create-definer my/leader
    :prefix-command 'my/leader-prefix-cmd
    :prefix-map 'my/leader-prefix-map
    :wk-full-keys nil
    "DEL"     '(which-key-undo                 :wk "󰕍 undo")
    )
  ;; ends here
  ;; [[file:config.org::*General.el][]]
  (my/leader
    "SPC"     '((general-simulate-key "C-<escape>") :wk "Mode leader")
    "/"       '(comment-dwim                   :wk "comment 󱀢")
    "s"       '(save-buffer                    :wk "save ")
    "e"       '(dirvish-side                   :wk "󰙅 dirvish-side ")
    "E"       '(dirvish                        :wk "󰙅 dirvish")
    "x"       '(consult-mode-command           :wk "execute")
    "z"       '(vundo                          :wk "visual undo")
  )
  ;; ends here
  ;; [[file:config.org::*General.el][]]
  (my/leader :infix "c"
    ""        '(nil                            :wk " Consult")
    "l"       '(consult-line                   :wk "line")
    "L"       '(consult-line-multi             :wk "line multi")
    "o"       '(consult-outline                :wk "outline")
    "i"       '(consult-imenu                  :wk "imenu")
    "I"       '(consult-imenu-multi            :wk "imenu multi")
    "r"       '(consult-ripgrep                :wk "ripgrep")
    "m"       '(consult-mark                   :wk "mark")
    "x"       '(consult-mode-command           :wk "execute")
    )
  ;; ends here
  ;; [[file:config.org::*General.el][]]
  (my/leader :infix "w" ;; workspaces
    ""        '(nil                            :wk " Workspace")
    "\\"      '(tab-new                        :wk "tab 󰏌")
    "|"       '(tab-close                      :wk "tab 󰅖")
    "["       '(tab-previous                   :wk "tab ")
    "]"       '(tab-next                       :wk "tab ")
  
    ","       '(sort-tab-select-next-tab       :wk "buffer ")
    "."       '(sort-tab-select-previous-tab   :wk "buffer ")
    "?"       '(sort-tab-close-current-tab     :wk "buffer 󰅖")
    "/"       '(consult-buffer                 :wk "buffer 󰏌")
    "f"       '(find-file                      :wk "file 󰏌")
  
    "j"       '(windmove-left                  :wk "window ")
    "k"       '(windmove-up                    :wk "window ")
    "l"       '(windmove-down                  :wk "window ")
    ";"       '(windmove-right                 :wk "window ")
    "'"       '(delete-window                  :wk "window 󰅖")
    "o"       '(toggle-one-window              :wk "one-window")
  
    "J"       '(split-window-right             :wk "split ")
    "K"       '(split-window-below             :wk "split ")
    "L"       '(split-window-above             :wk "split ")
    ":"       '(split-window-left              :wk "split ")
    )
  ;; ends here
  ;; [[file:config.org::*General.el][]]
  ;; Borg
  (my/leader :infix "B"
    ""        '(nil                            :wk " Borg")
    "a"       '(borg-assimilate                :wk "󱧕 assimilate ")
    "A"       '(borg-activate                  :wk " activate")
    "b"       '(borg-build                     :wk "󱇝 build")
    "c"       '(borg-clone                     :wk " clone")
    "r"       '(borg-remove                    :wk "󱧖 remove")
    )
  ;; toggle
  (my/leader :infix "t"
    ""        '(nil                            :wk " Toggle")
    )
  ;; Git
  (my/leader :infix "g"
    ""        '(nil                            :wk " Git")
    "g"       '(magit                          :wk " magit")
    )
  ;; ends here
  ;; [[file:config.org::*General.el][]]
  ;; windows, buffers and tabs(workspaces)
  ;; these are copied from emacs source code
  (defun split-window-left (&optional size window-to-split)
    (interactive `(,(when current-prefix-arg
                      (prefix-numeric-value current-prefix-arg))
                   ,(selected-window)))
    (let (new-window)
      (when (and size (< size 0) (< (- size) window-min-width))
        ;; `split-window' would not signal an error here.
        (error "Size of new window too small"))
      (setq new-window (split-window window-to-split size 'left))
      ;; Always copy quit-restore parameter in interactive use.
      (let ((quit-restore (window-parameter window-to-split 'quit-restore)))
        (when quit-restore
          (set-window-parameter new-window 'quit-restore quit-restore)))
      new-window))
  
  (defun split-window-above (&optional size window-to-split)
    (interactive `(,(when current-prefix-arg
                      (prefix-numeric-value current-prefix-arg))
                   ,(selected-window)))
    (let ((old-point (window-point))
          moved-by-window-height moved new-window bottom)
      (when (and size (< size 0) (< (- size) window-min-height))
        ;; `split-window' would not signal an error here.
        (error "Size of new window too small"))
      (setq new-window (split-window window-to-split size 'above))
      (when (and (null split-window-keep-point)
                 (or (null window-to-split)
                     (eq window-to-split (selected-window))))
        (with-current-buffer (window-buffer window-to-split)
          (save-excursion
            (goto-char (window-start))
            (setq moved (vertical-motion (window-height)))
            (set-window-start new-window (point))
            (when (> (point) (window-point new-window))
              (set-window-point new-window (point)))
            (when (= moved (window-height))
              (setq moved-by-window-height t)
              (vertical-motion -1))
            (setq bottom (point)))
          (and moved-by-window-height
               (<= bottom (point))
               (set-window-point window-to-split (1- bottom)))
          (and moved-by-window-height
               (<= (window-start new-window) old-point)
               (set-window-point new-window old-point)
               (select-window new-window))))
      ;; Always copy quit-restore parameter in interactive use.
      (let ((quit-restore (window-parameter window-to-split 'quit-restore)))
        (when quit-restore
          (set-window-parameter new-window 'quit-restore quit-restore)))
      new-window))
  ;; ends here
  
)
;; General.el:1 ends here

;; [[file:config.org::*Meow][Meow:1]]
(use-package meow
:custom-face
  (meow-cheatsheet-command ((t (:height 180 :inherit fixed-pitch))))
:config
 ;cate the behavior of vi's
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

  (defun my-meow-open-below ()
    "Open a newline below and switch to INSERT state."
    (interactive)
    (if meow--temp-normal
        (progn
          (message "Quit temporary normal mode")
          (meow--switch-state 'motion))
      (meow--switch-state 'insert)
      ;(goto-char (line-end-position))
      (move-end-of-line 1)
      (meow--execute-kbd-macro "RET")))
   (advice-add 'meow-open-below :override #'my-meow-open-below)

 (setq meow-keypad-self-insert-undefined nil)
 (setq meow-selection-command-fallback '(
    (meow-grab . meow-right-expand)
    (meow-change . meow-change-char)
    (meow-kill . meow-delete)
    (meow-cancel-selection . keyboard-quit)
    (meow-pop-selection . meow-pop-grab)
    (meow-beacon-change . meow-beacon-change-char)
    (meow-replace . meow-yank)
    (meow-reverse . negative-argument)
  ))

 (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
 ;; [[file:config.org::*Meow][]]
 (meow-normal-define-key
   '("<escape>" . meow-cancel-selection)
   '("\\" . "C-<escape>") ;; where I define all my custom major mode binding
   '("SPC" . my/leader-prefix-cmd) ;; defined latter
   '("1" . meow-expand-1) '("2" . meow-expand-2)
   '("3" . meow-expand-3) '("4" . meow-expand-4)
   '("5" . meow-expand-5) '("6" . meow-expand-6)
   '("7" . meow-expand-7) '("8" . meow-expand-8)
   '("9" . meow-expand-9) '("0" . meow-expand-0)
 
   '("q" . meow-quit) '("Q" . meow-quit)
      ;'("w" . meow-window) '("W" . meow-window)
      ;'("e" . meow-) '("E" . meow-e)
      ;'("r" . meow-) '("R" . meow-e)
   '("t" . meow-till) '("T" . meow-till-expand)
 
   '("a" . begin-of-line) '("a" . meow-beginning-of-thing)
   '("s" . meow) '("s" . meow-bounds-of-thing)
   '("d" . meow) '("d" . meow-inner-of-thing)
   '("f" . end-of-line) '("f" . meow-end-of-thing)
   '("g" . meow-right-expand) '("G" . meow-grab)
 
   '("z" . meow-undo) '("Z" . meow-undo-in-selection)
   '("x" . meow-kill) '("X" . meow-clipboard-kill)
   '("c" . meow-save)
   '("C-c" . meow-clipboard-save)
   '("v" . meow-replace) '("V" . meow-yank-pop)
   '("C-v" . meow-clipboard-yank)
   '("b". meow-block) '("B" . meow-to-block)
 
   '("y" . meow-visit)
   '("u" . meow-change) '("U" . meow-reverse)
   '("i" . meow-insert) '("I" . meow-open-above)
   '("o" . meow-append) '("O" . meow-open-below)
   '("p" . meow-pop-selection)
   '("[" . meow-beginning-of-thing) '("]" . meow-end-of-thing)
 
   '("h" . meow-join)  '("H" . meow-join)
   '("j" . meow-left)  '("J" . meow-left-expand)
   '("k" . meow-prev)  '("K" . meow-prev-expand)
   '("l" . meow-next)  '("L" . meow-next-expand)
   '(";" . meow-right) '(":" . meow-right-expand)
   '("'" . meow-line) '("\"" . meow-line)
 
   '("C-j" . "C-<escape> C-j")
   '("C-k" . "C-<escape> C-k")
   '("C-l" . "C-<escape> C-l")
   '("C-;" . "C-<escape> C-;")
 
   '("n" . meow-search)      '("N" . meow-pop-search)
   '("m" . meow-mark-word)   '("M" . meow-mark-symbol)
   '("," . meow-back-word)   '("<" . meow-back-symbol)
   '("." . meow-next-word)   '(">" . meow-next-symbol)
   '("/" . meow-reverse)
 
   '("\"" . fingertip-wrap-double-quote)
   '("(" . fingertip-wrap-round)
   '("[" . fingertip-wrap-bracket)
   '("{" . fingertip-wrap-curly)
   )
 ;; ends here
 ;; [[file:config.org::*Meow][]]
 (meow-motion-overwrite-define-key
  '("l" . meow-next)
  '("k" . meow-prev)
  '("SPC" . my/leader-prefix-cmd) ;; defined latter
  '("<escape>" . ignore)
 )
 ;; ends here
 ;; [[file:config.org::*Meow][]]
 (meow-leader-define-key
 ;; Use SPC (0-9) for digit arguments.
  '("1" . meow-digit-argument) '("2" . meow-digit-argument)
  '("3" . meow-digit-argument) '("4" . meow-digit-argument)
  '("5" . meow-digit-argument) '("6" . meow-digit-argument)
  '("7" . meow-digit-argument) '("8" . meow-digit-argument)
  '("9" . meow-digit-argument) '("0" . meow-digit-argument)
  '("/" . meow-keypad-describe-key) '("?" . meow-cheatsheet)
  ;;'("SPC" . config/leader-prefix-cmd)
 )
 
 ;; ends here
 (meow-global-mode)
)
;; Meow:1 ends here

;; [[file:config.org::*Which-key][Which-key:1]]
(use-package which-key
:init
  (setq which-key-sort-order 'which-key-key-order ;; default
        which-key-sort-uppercase-first nil
        ;; which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 10
        which-key-idle-delay 0.01 ; the first idle
        which-key-idle-secondary-delay 0.01 ; set to 0 will cause some problems
        which-key-max-description-length 25 ;
        which-key-allow-imprecise-window-fit t ; reduce delay
        which-key-separator " " ; yeah I don't like the arrow icon
        which-key-show-early-on-C-h t
        which-key-show-transient-maps t
        which-key-frame-max-height 60
        )
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right
        )
  (which-key-mode 1)
)
;; Which-key:1 ends here

;; [[file:config.org::*Which-key][Which-key:2]]
(use-package which-key-posframe
:after which-key
:config
  (setq which-key-posframe-poshandler
        'posframe-poshandler-frame-bottom-right-corner)
  (which-key-posframe-mode)
)
;; Which-key:2 ends here

;; [[file:config.org::*Rime][Rime:1]]
(use-package rime
:custom
  (rime-emacs-module-header-root "~/.nix-profile/include")
  (rime-librime-root "~/.nix-profile")
  (rime-share-data-dir "~/.config/fcitx/rime")
:config
  (setq default-input-method "rime"
        rime-show-candidate 'posframe)
  (setq rime-disable-predicates
    '(meow-normal-mode-p
      meow-motion-mode-p
      rime-predicate-after-alphabet-char-p
      rime-predicate-prog-in-code-p))
  (setq pgtk-use-im-context-on-new-connection nil)
)
;; Rime:1 ends here

;; [[file:config.org::*Fixed-pitch-mode][Fixed-pitch-mode:1]]
(use-package fixed-pitch
:config
  ;; This cause some bugs
  ;; (add-to-list 'default-frame-alist '(font . "Sarasa Gothic SC-16" ))
  (set-face-attribute 'default nil
    :font "Sarasa Gothic SC Nerd Font"
    :height 150)
  (set-face-attribute 'fixed-pitch nil
  ;:font "Sarasa Fixed SC"
    :font "RobotoMono Nerd Font Mono"
    :height 1.0)
  (set-face-attribute 'variable-pitch nil
    :font "Sarasa Gothic SC Nerd Font"
    :height 1.0)
  ; (set-fontset-font t 'symbol "Noto Sans Symbols 2")
  (set-face-attribute 'link nil
    :foreground "#ffcc66" :underline t :bold nil)
  ;; This is a temporary fix for solaire mode
  ;; (set-face-attribute 'solaire-default-face nil
  ;;   :inherit 'fixed-pitch)
  (fixed-pitch-mode)
)
;; Fixed-pitch-mode:1 ends here

;; [[file:config.org::*Zooming In/Out][Zooming In/Out:1]]
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
;; Zooming In/Out:1 ends here

;; [[file:config.org::*All-the-icons][All-the-icons:1]]
(use-package all-the-icons
  :if (display-graphic-p))

;(use-package all-the-icons-dired
;  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))
;; All-the-icons:1 ends here

;; [[file:config.org::*All-the-icons-completion][All-the-icons-completion:1]]
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))
;; All-the-icons-completion:1 ends here

;; [[file:config.org::*Nerd-icons][Nerd-icons:1]]
(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)
;; Nerd-icons:1 ends here

;; [[file:config.org::*Solaire mode][Solaire mode:1]]
(use-package solaire-mode
:defer t
:init (solaire-global-mode)
:config
  (add-hook 'org-src-mode-hook 'turn-off-solaire-mode)
)
;; Solaire mode:1 ends here

;; [[file:config.org::*Posframe][Posframe:1]]
(use-package posframe
:config
  (defun posframe-poshandler-frame-upper-center! (info)
  "Posframe's position handler.

   This poshandler function let center of posframe align to
   vertically upper 1/6, horizontally center
   of frame."
  (cons (/ (max 0 (- (plist-get info :parent-frame-width)
               (plist-get info :posframe-width))) 2)
        (/ (plist-get info :parent-frame-height) 6)))

  (defun posframe-poshandler-window-upper-center! (info)
   "Posframe's position handler.

     This poshandler function let center of posframe align to
     vertically upper 1/6, horizontally center
     of window."
  (let* ((window-left    (plist-get info :parent-window-left))
         (window-top     (plist-get info :parent-window-top))
         (window-width   (plist-get info :parent-window-width))
         (window-height  (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width)))
    (cons (max 0 (+ window-left (/ (- window-width posframe-width) 2)))
          (+ window-top (/ window-height 6))))
  )
)
;; Posframe:1 ends here

;; [[file:config.org::*Mini-frame][Mini-frame:1]]
(use-package mini-frame
:config
  (setq mini-frame-detach-on-hide nil)
  ;(setq mini-frame-standalone 't)
  ;(setq mini-frame-resize-min-height 10)
  (setq mini-frame-ignore-commands
    (append mini-frame-ignore-commands
     '(evil-window-split evil-window-vsplit evil-ex)))
)
;; Mini-frame:1 ends here

;; [[file:config.org::*Transparency][Transparency:1]]
(defun my/set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value))
;; Transparency:1 ends here

;; [[file:config.org::*Olivetti][Olivetti:1]]
(use-package olivetti
:hook (org-mode . olivetti-mode)
      (Custom-mode . olivetti-mode)
      (help-mode . olivetti-mode)
      ;(dashboard-mode . olivetti-mode)
      (dashboard-mode . variable-pitch-mode)
      (olivetti-mode . visual-line-mode)
:init
      (setq-default fill-column 100)
:config
      ;If nil (the default), use the value of fill-column + 2.
      (setq olivetti-body-width nil olivetti-style 'fancy)
      ;; (set-face-attribute 'olivetti-fringe nil :background "#171B24")
      (defun config/window-center (width)
          (interactive)
          (setq fill-column width)
          (olivetti-mode)
      )
      ;; (config/leader
      ;;  "tc"  '(olivetti-mode     :wk "󰉠 Center")
      ;; )
)
;; Olivetti:1 ends here

;; [[file:config.org::*Vertical][Vertical:1]]
(use-package topspace
:init (global-topspace-mode)
:config
;; Zen mode, the hack is from
;; https://github.com/trevorpogue/topspace/issues/17#issuecomment-1637004205
  (defun zen-mode-bodge ()
    (let ((ov (make-overlay 1 1 nil t t)))
      (overlay-put ov 'priority 9999999)
      (overlay-put ov 'before-string (propertize "    1 " 'face 'line-number))
      (overlay-put ov 'zen--remove-from-buffer-tag t))
    (let ((ov (make-overlay 1 2)))
      (overlay-put ov 'display-line-numbers-disable t)
      (overlay-put ov 'zen--remove-from-buffer-tag t)))

  (defun zen-mode-clear ()
    (remove-overlays 1 2 'zen--remove-from-buffer-tag t))

  ;; install hooks
  ;; (advice-add 'topspace--enable :after #'zen-mode-bodge)
  ;; (advice-add 'topspace--disable :after #'zen-mode-clear)
)
;; Vertical:1 ends here

;; [[file:config.org::*Vertico][Vertico:1]]
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
;; Vertico:1 ends here

;; [[file:config.org::*Orderless][Orderless:1]]
(use-package orderless
  :init
  (setq completion-styles '(orderless))
  (setq orderless-component-separator
          #'orderless-escapable-split-on-space)
  (setq orderless-matching-styles
          '(orderless-initialism orderless-prefixes orderless-regexp))
  )
;; Orderless:1 ends here

;; [[file:config.org::*Vertico-directory][Vertico-directory:1]]
(use-package vertico-directory
    :after vertico
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
)
    ;; Tidy shadowed file names
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; Vertico-directory:1 ends here

;; [[file:config.org::*Vertico-multiform][Vertico-multiform:1]]
(use-package vertico-multiform
  :after vertico
  :config (vertico-multiform-mode)
)
;; Vertico-multiform:1 ends here

;; [[file:config.org::*Vertico-posframe][Vertico-posframe:1]]
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
  (setq vertico-multiform-commands
        '(
          (execute-extended-command ; M-x
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-width . 120))

          (meow-visit
           (vertico-posframe-poshandler . posframe-poshandler-window-top-right-corner)
           (vertico-posframe-width . 50))

          (meow-yank-pop; M-x
           (vertico-posframe-poshandler . posframe-poshandler-point-window-center)
           (vertico-posframe-width . 50))

          (find-file
           (vertico-count . 25)
           (vertico-posframe-width . 80)
           (vertico-posframe-poshandler . posframe-poshandler-window-upper-center!))

          (consult-buffer
           (vertico-count . 25)
           (vertico-posframe-width . 100)
           (vertico-posframe-poshandler . posframe-poshandler-window-upper-center!))

          (switch-to-buffer
           (vertico-count . 25)
           (vertico-posframe-width . 100)
           (vertico-posframe-poshandler . posframe-poshandler-window-upper-center!))

          (org-insert-link; C-c C-l
           (vertico-posframe-poshandler . posframe-poshandler-point-top-left-corner)
           (vertico-posframe-width . 70))

          (consult-imenu
           (vertico-count . 40)
           (vertico-posframe-poshandler . posframe-poshandler-window-top-right-corner)
           (vertico-posframe-width . 80))

          (consult-outline
           (vertico-count . 30)
           (vertico-posframe-poshandler . posframe-poshandler-window-top-right-corner)
           (vertico-posframe-width . 40))

          (consult-line
           (vertico-count . 30)
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-right-corner)
           (vertico-posframe-width . 60))

          (t
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-width . 120))
      ))

:config
      (vertico-multiform-mode 1)
      (vertico-posframe-mode 1)
)
;; Vertico-posframe:1 ends here

;; [[file:config.org::*Savehist][Savehist:1]]
;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist
      :init
      (savehist-mode))
;; Savehist:1 ends here

;; [[file:config.org::*Savehist][Savehist:2]]
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
;; Savehist:2 ends here

;; [[file:config.org::*Consult][Consult:1]]
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
       ([remap list-buffers] . consult-buffer)   ;; orig. switch-to-buffer
       ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
       ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
       ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
       ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
       ;; Custom M-# bindings for fast register access
       ("M-#" . consult-register-load)
       ("M-'" . consult-register-store)  ;; orig. abbrev-prefix-mark (unrelated)
       ("C-M-#" . consult-register)
       ;; Other custom bindings
       ("M-y" . consult-yank-pop)           ;; orig. yank-pop
       ;; M-g bindings in `goto-map'
       ("M-g e" . consult-compile-error)
       ("M-g f" . consult-flymake)          ;; Alternative: consult-flycheck
       ("M-g g" . consult-goto-line)        ;; orig. goto-line
       ("M-g M-g" . consult-goto-line)      ;; orig. goto-line
       ("M-g o" . consult-outline)          ;; Alternative: consult-org-heading
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

:init

      ;; Optionally configure the register formatting.
  ;; This improves the register preview for
  ;; `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
      (setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

      ;; Optionally tweak the register preview window.
      ;; This adds thin lines, sorting and hides the mode line of the window.
      (advice-add #'register-preview :override #'consult-register-window)

      ;; Use Consult to select xref locations with preview
      (setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

:config

      ;; Optionally configure preview.
  ;; The default value is 'any, such that any key triggers the preview.
      ;; (setq consult-preview-key 'any)
      ;; (setq consult-preview-key "M-.")
      ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
      ;; :preview-key on a per-command basis using the `consult-customize' macro.
      (consult-customize
       consult-theme
     :preview-key '(:debounce 0.2 any)
       consult-ripgrep consult-git-grep consult-grep
       consult-bookmark consult-recent-file consult-xref
       consult--source-bookmark consult--source-file-register
       consult--source-recent-file consult--source-project-recent-file
       ;;  :preview-key "M-."
       :preview-key '(:debounce 0.4 any)
  )

      ;; Optionally configure the narrowing key.
      (setq consult-narrow-key "<") ;; "C-+"
  (setq consult-buffer-filter "\\*")
      ;; Optionally make narrowing help available in the minibuffer.
      ;; You may want to use `embark-prefix-help-command' or which-key instead.
      ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
)
;; Consult:1 ends here

;; [[file:config.org::*Marginalia][Marginalia:1]]
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
;; Marginalia:1 ends here

;; [[file:config.org::*Sort-tab][Sort-tab:1]]
(use-package sort-tab
:init (sort-tab-mode)
:config
  (defun my/sort-tab-filter (buffer)
    "a filter functioni for sort-tab. return t hides the buffer"
    (let ((major-mode (buffer-local-value 'major-mode buffer)))
      (cond ((eq major-mode 'dired-mode) t)
            (t nil)))
    )
  (setq sort-tab-hide-function 'my/sort-tab-filter)
)
;; Sort-tab:1 ends here

;; [[file:config.org::*Popper][Popper:1]]

;; Popper:1 ends here

;; [[file:config.org::*Toggle-one-window][Toggle-one-window:1]]
(use-package toggle-one-window)
;; Toggle-one-window:1 ends here

;; [[file:config.org::*Doom-modeline][Doom-modeline:1]]
(use-package doom-modeline
:init
      (setq
          doom-modeline-height 37
          doom-modeline-enable-word-count t)
      (doom-modeline-mode 1)
:config
      (set-face-attribute 'doom-modeline t
          :inherit 'variable-pitch)
  ;; let modeline show on the header, not bottom
  (defun move-up-modeline ()
    (interactive)
    (progn
      (setq-default header-line-format mode-line-format)
      (setq-default mode-line-format nil)
    ))
  (move-up-modeline)
)
;; Doom-modeline:1 ends here

;; [[file:config.org::*Diminish][Diminish:1]]
(use-package diminish)
;; Diminish:1 ends here

;; [[file:config.org::*Awesome-tray][Awesome-tray:1]]
(use-package awesome-tray
:defer t
:init
  ;(awesome-tray-mode)
:config
  (setq awesome-tray-hide-mode-line nil)
)
;; Awesome-tray:1 ends here

;; [[file:config.org::*Emacs Dashboard][Emacs Dashboard:1]]
(use-package dashboard
:init
  (setq initial-buffer-choice 'dashboard-open
      dashboard-image-banner-max-width 1100
      dashboard-set-heading-icons t
      dashboard-center-content t ;; set to 't' for centered content
      dashboard-set-file-icons t
      initial-buffer-choice (lambda () (get-buffer-create "*scratch*"))
      dashboard-startup-banner ;; use custom image as banner
      (concat user-emacs-directory "assets/EmaxBound.webp")
      dashboard-items
      '((recents . 5)
        (agenda . 5 )
        (bookmarks . 3)
        (projects . 3)
        (registers . 3)
        )
  )
:config
  (dashboard-setup-startup-hook)
  (setq-default header-line-format mode-line-format)
  (setq-default mode-line-format nil)
  (set-face-attribute 'dashboard-items-face nil)

:bind (:map dashboard-mode-map
  ("k" . 'dashboard-previous-line)
  ("l" . 'dashboard-next-line)
  (";" . 'dashboard-next-section)
  ("j" . 'dashboard-previous-section)
  )
)
;; Emacs Dashboard:1 ends here

;; [[file:config.org::*Visual Undo][Visual Undo:1]]
(use-package vundo
:config
  (general-def vundo-mode-map
    "j"  'vundo-backward
    ";"  'vundo-forward
    "k"  'vundo-previous
    "l"  'vundo-next
    "q"  'vundo-quit
    "s"  'vundo-save
    )
)
;; Visual Undo:1 ends here

;; [[file:config.org::*Magit][Magit:1]]
(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  :hook (magit-mode . solaire-mode) (magit-mode . olivetti-mode)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
        'magit-insert-modules
        'magit-insert-stashes
        'append
        )
  (setq magit-show-long-lines-warning nil)
)
;; Magit:1 ends here

;; [[file:config.org::*Diff][Diff:1]]
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
;; Diff:1 ends here

;; [[file:config.org::*Dirvish][Dirvish:1]]
(use-package dirvish
:after dired
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

  (general-def dirvish-mode-map
    "q"      '(dirvish-quit              :wk "quit")
    "j"      '(dired-up-directory        :wk "up-dir")
    ";"      '(dired-find-file           :wk "open/toggle")
    "a"      '(dirvish-quick-access      :wk "access")
    "c"      '(dired-do-copy             :wk "copy")
    "u"      '(dired-do-rename           :wk "rename")
  )
  ;; (nmap dirvish-mode-map
  ;;    "?"      '(dirvish-dispatch          :wk "Dispatch")
  ;;    "TAB"    '(dirvish-subtree-toggle    :wk "Subtre-toggle")
  ;;    "q"      '(dirvish-quit              :wk "Quit")
  ;;    "h"      '(dired-up-directory        :wk "Up-dir")
  ;;    "l"      '(dired-find-file           :wk "Open/Toggle")
  ;;    "a"      '(dirvish-quick-access      :wk "Access")
  ;;    "f"      '(dirvish-file-info-menu    :wk "File Info Menu")
  ;;    "y"      '(dirvish-yank-menu         :wk "Yank Menu")
  ;;    "N"      '(dirvish-narrow            :wk "Narrow")
  ;;    ;         `dired-view-file'
  ;;    "v"      '(dirvish-vc-menu           :wk "View-file")
  ;;    ;         `dired-sort-toggle-or-edit'
  ;;    "s"      '(dirvish-quicksort         :wk "Quick-sort")

  ;;    "M-f"    '(dirvish-history-go-forward  :wk "History-forward")
  ;;    "M-b"    '(dirvish-history-go-backward :wk "History-back")
  ;;    "M-l"    '(dirvish-ls-switches-menu    :wk "ls Switch Menu")
  ;;    "M-m"    '(dirvish-mark-menu           :wk "Mark Menu")
  ;;    "M-t"    '(dirvish-layout-toggle       :wk "Layout-toggle")
  ;;    "M-s"    '(dirvish-setup-menu          :wk "Setup-Menu")
  ;;    "M-e"    '(dirvish-emerge-menu         :wk "Emerge-Menu")
  ;;    "M-j"    '(dirvish-fd-jump             :wk "fd-jump")
  ;; )
  (dirvish-override-dired-mode)
)
;; Dirvish:1 ends here

;; [[file:config.org::*Diredfl][Diredfl:1]]
(use-package diredfl
:hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
:config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
)
;; Diredfl:1 ends here

;; [[file:config.org::*Embark][Embark:1]]
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; Embark:1 ends here

;; [[file:config.org::*Embark][Embark:2]]
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; Embark:2 ends here

;; [[file:config.org::*Blink search][Blink search:1]]
(use-package blink-search
:defer t
:config
  (setq blink-search-enable-posframe t)
)
;; Blink search:1 ends here

;; [[file:config.org::*Color-rg][Color-rg:1]]
(use-package color-rg
:config
  (general-def isearch-mode-map
    "M-s M-s" 'isearch-toggle-color-rg
  )
)
;; Color-rg:1 ends here

;; [[file:config.org::*Voyager][Voyager:1]]
(use-package voyager)
;; Voyager:1 ends here

;; [[file:config.org::*Holo-layer][Holo-layer:1]]
(use-package holo-layer
:defer t
:if (memq window-system '(pgtk mac ns))
:config
  (setq holo-layer-enable-cursor-animation 1
        holo-layer-enable-window-border 1
        holo-layer-sort-tab-ui 1
        ;;holo-layer-cursor-animation-type "arrow easing"
        )
  (holo-layer-enable)
)
;; Holo-layer:1 ends here

;; [[file:config.org::*Beacon][Beacon:1]]
(use-package beacon
:defer t
:config
      (beacon-mode)
)
;; Beacon:1 ends here

;; [[file:config.org::*Pulse-Cursor][Pulse-Cursor:1]]
(use-package pulsing-cursor
:config
  (setq pulse-delay 0.08
        pulse-iterations 2)
  (setq pulsing-cursor-blinks 20)
  (set-face-attribute 'pulsing-cursor-overlay-face1 nil :inherit 'cursor)
  (pulsing-cursor-mode)
)
;; Pulse-Cursor:1 ends here

;; [[file:config.org::*Goggles][Goggles:1]]
(use-package goggles
:hook ((prog-mode text-mode org-mode) . goggles-mode)
:config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing
;; Goggles:1 ends here

;; [[file:config.org::*Hl-line][Hl-line:1]]
(use-package hl-line
:init
  (global-hl-line-mode)
)
;; Hl-line:1 ends here

;; [[file:config.org::*Whitespace mode][Whitespace mode:1]]
;; (config/leader :infix "t"
;;   "SPC"  '(whitespace-mode  :wk "󰡭 Show Space")
;; )
;; Whitespace mode:1 ends here

;; [[file:config.org::*Line Number][Line Number:1]]
(use-package emacs
:custom-face
      ;; (line-number ((t (
      ;;    :weight normal :slant normal :foreground "LightSteelBlue4"
      ;;    :inherit default))))
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
      ;; (config/leader :infix "tl"
      ;;  ""    '(nil                                :wk "  Line Number ")
      ;;  "DEL" '(which-key-undo                     :wk "󰕍 Undo key   ")
      ;;  "n"   '(config/toggle-line-number-nil      :wk "󰅖 Nil        ")
      ;;  "a"   '(config/toggle-line-number-absolute :wk "󰱇 Absolute   ")
      ;;  "r"   '(config/toggle-line-number-relative :wk "󰰠 Relative   ")
      ;;  "v"   '(config/toggle-line-number-visual   :wk " Visual     ")
      ;;  "h"   '(hl-line-mode                       :wk "󰸱 Hl-line")
      ;; )
)
;; Line Number:1 ends here

;; [[file:config.org::*Rainbow-mode][Rainbow-mode:1]]

;; Rainbow-mode:1 ends here

;; [[file:config.org::*Rainbow-Delimiters][Rainbow-Delimiters:1]]
(use-package rainbow-delimiters
:hook (prog-mode . rainbow-delimiters-mode)
)
;; Rainbow-Delimiters:1 ends here

;; [[file:config.org::*Highlight-parentheses][Highlight-parentheses:1]]
(use-package highlight-parentheses
:config
  (global-highlight-parentheses-mode)
  )
;; Highlight-parentheses:1 ends here

;; [[file:config.org::*Highlight-Indent-Guides][Highlight-Indent-Guides:1]]
(use-package highlight-indent-guides
:hook (prog-mode . highlight-indent-guides-mode)
:config
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-character 9474
        highlight-indent-guides-auto-enabled nil
  )
  (set-face-attribute 'highlight-indent-guides-character-face nil
    :foreground "#3b445f")
  (set-face-attribute 'highlight-indent-guides-top-character-face nil
    :foreground "#ffcc66")

)
;; Highlight-Indent-Guides:1 ends here

;; [[file:config.org::*Smartparens][Smartparens:1]]
(use-package smartparens
:config
  (smartparens-global-mode)
)
;; Smartparens:1 ends here

;; [[file:config.org::*Electric-Indent][Electric-Indent:1]]
(use-package electric
:config
  (setq-default indent-tabs-mode nil) ;; always indent with spaces
  (setq electric-pair-mode t) ;; global-minor mode
)
;; Electric-Indent:1 ends here

;; [[file:config.org::*Aggressive-Indent][Aggressive-Indent:1]]
(use-package aggressive-indent
:disabled
:config
  (global-aggressive-indent-mode 1)
)
;; Aggressive-Indent:1 ends here

;; [[file:config.org::*Auto-save][Auto-save:1]]
(use-package auto-save
:after lsp-bridge
:config
  (auto-save-enable)
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace t)
)
;; Auto-save:1 ends here

;; [[file:config.org::*LSP-bridge][LSP-bridge:1]]
(use-package lsp-bridge
:init
  (global-lsp-bridge-mode)
:config
  ;(set-face-attributes 'lsp-bridge-alive-mode-line nil
  ;  :inherit 'variable-pitch
  ;)
)
;; LSP-bridge:1 ends here

;; [[file:config.org::*Fingertip][Fingertip:1]]
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
      ;"M-:" 'fingertip-jump-out-pair-and-newline

      "C-j" 'fingertip-jump-up
  )
)
;; Fingertip:1 ends here

;; [[file:config.org::*Treesit-auto][Treesit-auto:1]]
(use-package treesit-auto
:config

  (defun my/remap-mode (mode)
    "A hack to make org-src-get-lang-mode respect major-mode-remap-alist"
    (treesit-auto--set-major-remap)
    (alist-get mode major-mode-remap-alist mode)
    )
  (advice-add 'org-src-get-lang-mode :filter-return #'my/remap-mode)

  ;; a workaround for emacs29 do not come up with elisp-ts-mode
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  (global-treesit-auto-mode))
;; Treesit-auto:1 ends here

;; [[file:config.org::*Treesitter-context][Treesitter-context:1]]
(use-package treesitter-context
:config
  (add-hook 'python-ts-mode-hook #'treesitter-context-mode)
  )
;; Treesitter-context:1 ends here

;; [[file:config.org::*YASnippet][YASnippet:1]]
(use-package yasnippet
  :init
  (yas-global-mode 1)
)
;; YASnippet:1 ends here

;; [[file:config.org::*Tempel][Tempel:1]]
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)
;; Tempel:1 ends here

;; [[file:config.org::*Elisp][Elisp:1]]
(setq-default lexical-binding t)
;; Elisp:1 ends here

;; [[file:config.org::*AUCTeX][AUCTeX:1]]
(use-package auctex)
;; AUCTeX:1 ends here

;; [[file:config.org::*CDTeX][CDTeX:1]]
(use-package cdlatex)
;; CDTeX:1 ends here

;; [[file:config.org::*LAAS][LAAS:1]]
(use-package laas
:after ass
:hook (LaTeX-mode . laas-mode))
;; LAAS:1 ends here

;; [[file:config.org::*Nix][Nix:1]]
(use-package nix-mode
:mode "\\.nix\\'"
:config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((nix . t))
   )
)
;; Nix:1 ends here

;; [[file:config.org::*Nix][Nix:2]]
(use-package nix-ts-mode
:mode "\\.nix\\'"
)
;; Nix:2 ends here

;; [[file:config.org::*PlantUML][PlantUML:1]]
(use-package plantuml-mode
:mode "\\.plantuml\\'"
)
;; PlantUML:1 ends here

;; [[file:config.org::*Jupyter][Jupyter:1]]
(use-package jupyter
:defer t
  )
;; Jupyter:1 ends here

;; [[file:config.org::*Verilog][Verilog:1]]
(use-package verilog-mode
:mode "\\.v\\'"
)
;; Verilog:1 ends here

;; [[file:config.org::*YAML][YAML:1]]
(use-package yaml-mode
:mode "\\.yaml\\'"
)
;; YAML:1 ends here

;; [[file:config.org::*Init][Init:1]]
(use-package org
:after emacs
:init
  (setq org-element-cache-persistent nil)
  (setq org-element-use-cache nil)
  (setq org-latex-preview-numbered t)
:config
  (add-hook 'org-mode-hook #'olivetti-mode)
  ;(add-hook 'org-mode-hook #'org-visual-indent-mode)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
)
;; Init:1 ends here

;; [[file:config.org::*Fonts][Fonts:1]]
(use-package org
:custom-face
  ;; (org-latex-and-related ((t (:foreground "LightSteelBlue4" :weight bold))))
  ;; (org-meta-line ((t (:foreground "LightSteelBlue4"))))
  ;; (org-special-keyword ((t (:foreground "LightSteelBlue4"))))
  ;; (org-tag ((t (:foreground "LightSteelBlue4" :weight normal))))
;:hook (org-mode . mixed-pitch-mode)
:config
  (set-face-attribute 'org-level-1 nil :height 1.6 )
  (set-face-attribute 'org-level-2 nil :height 1.4 )
  (set-face-attribute 'org-level-3 nil :height 1.4 )
  (set-face-attribute 'org-level-4 nil :height 1.3 )
  (set-face-attribute 'org-level-5 nil :height 1.2 )
  (set-face-attribute 'org-level-6 nil :height 1.1 )
  (set-face-attribute 'org-document-title nil :height 2.5 :bold t)
  (set-face-attribute 'org-document-info nil :height 1.8 :bold t)
  (set-face-attribute 'org-document-info-keyword nil
     :inherit 'org-document-info)
  (set-face-attribute 'org-block nil
    :extend t :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil
    :height 0.9 :inherit 'default)
)
;; Fonts:1 ends here

;; [[file:config.org::*Bullets][Bullets:1]]
(use-package org-modern
;;:hook (org-mode . org-modern-mode)
:config
  (setq org-modern-keyword
     '(("author" . "◇")
      ("title" . "◈")
      ("subtitle" . "◉")
      ("html" . "󰅱 ")
      ("results" . "□ results")
      (t . t)))
  (setq org-modern-star
   '("⚀" "⚁" "⚂" "⚃" "⚄" "⚅")
  )
  (set-face-attribute 'org-modern-symbol nil :family "Noto Sans Symbols 2")
; '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")
; '("◉" "○" "◈" "◇" "✳")
  (setq org-modern-list ;; for '+' '-' '*' respectively
      '((43 . "∙") (45 . "➤") (42 . "✽"))
  )
  (setq org-modern-block-fringe nil)
  (setq org-modern-todo nil)
  (setq org-modern-block-name '("⇲ " . "⇱ "))
  (set-face-attribute 'org-modern-block-name nil
     :inherit 'variable-pitch)
  (set-face-attribute 'org-meta-line nil
     :height 0.9)
  (set-face-attribute 'org-modern-horizontal-rule nil
      :strike-through (face-foreground 'org-meta-line) :inherit 'org-hide)

  (setq org-modern-table nil)
  (global-org-modern-mode)
)
;; Bullets:1 ends here

;; [[file:config.org::*Bullets][Bullets:2]]
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
;; Bullets:2 ends here

;; [[file:config.org::*Indent lines][Indent lines:1]]
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
;; Indent lines:1 ends here

;; [[file:config.org::*Indent lines][Indent lines:2]]
(use-package org-visual-outline
;; :custom-face
  ;;  (org-visual-indent-blank-pipe-face ((t
  ;;        (:background "#1f2430" :foreground "#1f2430"
  ;;     :height 0.1 :width extra-expanded))))
  ;; (org-visual-indent-pipe-face ((t
  ;;       (:background "#454d6d" :foreground "#454d6d"
  ;;    :height 0.1 :width extra-expanded))))
:hook (org-mode . org-visual-indent-mode)
:after org
)
;; Indent lines:2 ends here

;; [[file:config.org::*valign][valign:1]]
(use-package valign
:hook (org-mode . valign-mode)
:config
  (setq valign-fancy-bar t)
)
;; valign:1 ends here

;; [[file:config.org::*Highlight TODO][Highlight TODO:1]]
(use-package hl-todo
  :init
  (hl-todo-mode)
)
;; Highlight TODO:1 ends here

;; [[file:config.org::*Org-appear][Org-appear:1]]
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autoemphasis  t
        ;; org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        ;; org-appear-inside-latex t
        org-hide-emphasis-markers t
  )

)
;; Org-appear:1 ends here

;; [[file:config.org::*Keybinds][Keybinds:1]]
(use-package org
  :config
  (general-def
    :keymaps '(org-mode-map)
    "C-<escape>" '(my/org-cmd :wk "Org")
    )
  (general-def
    :prefix-command 'my/org-cmd
    :prefix-map 'my/org-map
    "b"   '(my/org-babel-cmd                 :wk "Babel")
    "c"   '(org-ctrl-c-ctrl-c                :wk "c dwim")
    "l"   '(org-insert-link                  :wk "link")
    "'"   '(org-edit-special                 :wk "edit")
    ","   '(org-insert-structure-template    :wk "structure")
    "C-j" '(my/outline-left                  :wk "outline left")
    "C-k" '(my/outline-up                    :wk "outline up")
    "C-l" '(my/outline-down                  :wk "outline down")
    "C-;" '(my/outline-right                 :wk "outline right")
    "v"   '(org-download-clipboard           :wk "paste image")
    )
  (general-def
    :prefix-command 'my/org-babel-cmd
    :prefix-map 'my/org-babel-map
    "t" '(org-babel-tangle                   :wk "tangle")
    "d" '(org-babel-demarcate-block          :wk "demarcate")
    )
  (general-def
    :keymaps '(org-src-mode-map)
    "C-<escape>" '(my/org-src-cmd :wk "Org-src")
    )
  (general-def
    :prefix-command 'my/org-src-cmd
    :prefix-map 'my/org-src-map
    "'"   'org-edit-src-exit
    "k"   'org-edit-src-abort
    )
  )
;; Keybinds:1 ends here

;; [[file:config.org::*outline functions][outline functions:1]]
(defun my/outline-left ()
  (interactive)
  (cond ((outline-on-heading-p)
         (hide-subtree)
         (outline-up-heading 1)
         (hide-subtree)
         (outline-show-children)
         (outline-show-entry))
        (t
         (outline-back-to-heading)
         ))
  )

(defun my/outline-up ()
  (interactive)
  (cond ((outline-on-heading-p)
         (hide-subtree)
         (outline-backward-same-level 1)
         (outline-show-children)
         (outline-show-entry)
         )
        (t
         (org-previous-block 1)))
)

(defun my/outline-down ()
  (interactive)
  (cond ((outline-on-heading-p)
         (hide-subtree)
         (outline-forward-same-level 1)
         (outline-show-children)
         (outline-show-entry)
         )
        (t
         (org-next-block 1)))
  )

(defun my/outline-right ()
  (interactive)
  (outline-show-children)
  (outline-show-entry)
  (if (outline-has-subheading-p)
      (progn (outline-next-heading)
             (outline-show-children)
             (outline-show-entry))))
;; outline functions:1 ends here

;; [[file:config.org::*Src block][Src block:1]]
(use-package org
:init
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (C . t)
          (python . t)
          (jupyter . t)
          (java . t)
          ))
:config
  (setq org-src-tab-acts-natively t
        org-src-preserve-indentation nil
        org-confirm-babel-evaluate nil
        org-confirm-elisp-link-function nil
        org-link-elisp-confirm-function nil
        )
  (general-define-key :keymap 'org-src-mode-map
     "C-c C-c" 'eval-buffer)
)
;; Src block:1 ends here

;; [[file:config.org::*Src block][Src block:2]]
(use-package org-auto-tangle
:hook (org-mode . org-auto-tangle-mode)
)
;; Src block:2 ends here

;; [[file:config.org::*Src block][Src block:3]]
(use-package ob-async
:after org
:config
  ;; below languages may have independent implementation of async
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
  )
;; Src block:3 ends here

;; [[file:config.org::*LaTeX][LaTeX:1]]
(use-package org
:config
  (defconst org-match-substring-regexp
    (concat
     "\\(\\S-\\)\\([_^]\\)\\("
     "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
     "\\|"
     "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
     "\\|"
     "\\(?:.\\)"
     "\\|"
     "\\(?:\\\\[[:alnum:].,\\]*[[:alnum:]]\\)"
     "\\)")
    "The regular expression matching a sub- or superscript.")

  (defun +org-raise-scripts (limit)
    "Add raise properties to sub/superscripts."
    (when (and org-pretty-entities org-pretty-entities-include-sub-superscripts
               (re-search-forward org-match-substring-regexp limit t))
      (let* ((pos (point)) table-p comment-p
             (mpos (match-beginning 3))
             (emph-p (get-text-property mpos 'org-emphasis))
             (link-p (get-text-property mpos 'mouse-face))
             (keyw-p (eq 'org-special-keyword (get-text-property mpos 'face)))
             (tex-p (eq 'org-latex-and-related (get-text-property mpos 'face))))
        (goto-char (line-beginning-position))
        (setq table-p (looking-at-p org-table-dataline-regexp)
              comment-p (looking-at-p "^[ \t]*#[ +]"))
        (goto-char pos)
        ;; Handle a_b^c
        (when (member (char-after) '(?_ ?^)) (goto-char (1- pos)))
        (if (not (or comment-p emph-p link-p keyw-p))
            (put-text-property (match-beginning 3) (match-end 0)
                               'display
                               (if (equal (char-after (match-beginning 2)) ?^)
                                   (nth (if table-p 3 1) org-script-display)
                                 (nth (if table-p 2 0) org-script-display)))
          (put-text-property (match-beginning 2) (match-end 3) 'org-emphasis t))
        t)))

  (advice-add #'org-raise-scripts :override #'+org-raise-scripts)
)
;; LaTeX:1 ends here

;; [[file:config.org::*LaTeX][LaTeX:2]]
(use-package org
:config
  (setq org-entities-user
        '(("vdash" "\\vdash" t "⊢" "⊢" "⊢" "⊢")
          ("vDash" "\\vDash" t "⊨" "⊨" "⊨" "⊨")
          ("Vdash" "\\Vdash" t "⊩" "⊩" "⊩" "⊩")
          ("Vvdash" "\\Vvdash" t "⊪" "⊪" "⊪" "⊪")
          ("nvdash" "\\nvdash" t "⊬" "⊬" "⊬" "⊬")
          ("nvDash" "\\nvDash" t "⊭" "⊭" "⊭" "⊭")
          ("nVdash" "\\nVdash" t "⊮" "⊮" "⊮" "⊮")
          ("nVDash" "\\nVDash" t "⊯" "⊯" "⊯" "⊯")
          ("subseteq" "\\subseteq" t "⊆" "⊆" "⊆" "⊆")
          ("supseteq" "\\supseteq" t "⊇" "⊇" "⊇" "⊇")
          ("subsetneq" "\\subsetneq" t "⊊" "⊊" "⊊" "⊊")
          ("supsetneq" "\\supsetneq" t "⊋" "⊋" "⊋" "⊋")
          ("nsubseteq" "\\nsubseteq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteq" "\\nsupseteq" t "⊉" "⊉" "⊉" "⊉")
          ("nsubseteqq" "\\nsubseteqq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteqq" "\\nsupseteqq" t "⊉" "⊉" "⊉" "⊉")
          ("subsetneqq" "\\subsetneqq" t "⊊" "⊊" "⊊" "⊊")
          ("supsetneqq" "\\supsetneqq" t "⊋" "⊋" "⊋" "⊋")
          ("nsubset" "\\nsubset" t "⊄" "⊄" "⊄" "⊄")
          ("nsupset" "\\nsupset" t "⊅" "⊅" "⊅" "⊅")
          ("nsubseteq" "\\nsubseteq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteq" "\\nsupseteq" t "⊉" "⊉" "⊉" "⊉")))
  )
;; LaTeX:2 ends here

;; [[file:config.org::*LaTeX][LaTeX:3]]
(use-package org
:init
  (setq org-latex-preview-numbered t)
  (plist-put org-latex-preview-options :zoom 1.25)
  (let ((pos (assoc 'dvisvgm org-latex-preview-process-alist)))
    (plist-put (cdr pos) :image-converter '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f"))))
;; LaTeX:3 ends here

;; [[file:config.org::*Org-download][Org-download:1]]
(use-package org-download
  :after org
)
;; Org-download:1 ends here

;; [[file:config.org::*PlantUML][PlantUML:1]]
(use-package plantuml-mode
  :defer t
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :init
  ;; enable plantuml babel support
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((plantuml . t))))
  :config
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-plantuml-executable-path "plantuml")
  (setq plantuml-executable-path "plantuml")
  (setq plantuml-default-exec-mode 'executable)
  ;; set default babel header arguments
  (setq org-babel-default-header-args:plantuml
        '((:exports . "results")
          (:results . "file")
          ))
)
;; PlantUML:1 ends here

;; [[file:config.org::*Org-capture][Org-capture:1]]
(use-package org-capture
:after org
:bind (("C-c c" . org-capture))
:init
  (setq org-directory "~/Org")
:config
  ;; if file path is not absolute
  ;; it is treated as relative path to org-directory
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
     See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: "))
       (fname (org-hugo-slug title)))
    (mapconcat #'identity
        `(,(concat "* TODO " title) ":PROPERTIES:"
          ,(concat ":EXPORT_FILE_NAME: " fname) ":END:" "%?\n")
          ;Place the cursor here finally
        "\n")))
  (setq org-capture-templates (append org-capture-templates '(
    ("j" "Journal" entry
      (file+datetree "journal.org")
      "* %U - %?\n")
    ("i" "Inbox" entry
      (file "inbox.org")
      "* %U - %? %^g\n")
       '("h" "Hugo post" entry
      (file+olp "capture.org" "Notes")
      (function org-hugo-new-subtree-post-capture-template))
  )))
)
;; Org-capture:1 ends here

;; [[file:config.org::*Custom-ID][Custom-ID:1]]
(use-package org
:config
  (defun org-add-custom-id ()
    "Add CUSTOM_ID property to current heading, skip if already have"
    (interactive)
    (let ((custom-id (org-entry-get nil "CUSTOM_ID")))
      (unless custom-id
        (org-set-property "CUSTOM_ID" (org-id-new))))
    )

  (defun org-add-custom-id-to-all-headings ()
    "Add CUSTOM_ID properties to headings without a CUSTOM_ID property in current Org buffer."
    (interactive)
    (org-map-entries
     (lambda ()
       (let ((custom-id (org-entry-get nil "CUSTOM_ID")))
         (unless custom-id
           (org-set-property "CUSTOM_ID" (org-id-new)))))
     nil 'file)
    )
)
;; Custom-ID:1 ends here

;; [[file:config.org::*Org-super-links][Org-super-links:1]]
(use-package org-super-links
:after org
:config

  (defun my/org-insert-backlink ()
    "insert backlink using consult-org-heading in current org file"
    (interactive)
    (let ((target-position
           (save-excursion (consult-org-heading) (point) )))
      (org-super-links--insert-link
       (set-marker (make-marker) target-position)))
    (recenter)
    )
  (setq org-super-links-search-function 'my/org-insert-backlink)
  (setq org-super-links-backlink-prefix nil)
)
;; Org-super-links:1 ends here

;; [[file:config.org::*Org-roam][Org-roam:1]]
(use-package org-roam
:after org
:defer t
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
;; Org-roam:1 ends here

;; [[file:config.org::*TODOs][TODOs:1]]
(use-package org
:config
(setq org-todo-keywords '(
     (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CNCL(c@/!)")
     (sequence "SOMEDAY")
))
(setq org-todo-state-tags-triggers
   '(("CNCL" ("CNCL" . t))
     ("WAIT" ("WAIT" . t))
     ("HOLD" ("WAIT") ("HOLD" . t))
     (done ("WAIT") ("HOLD"))
     ("TODO" ("WAIT") ("CNCL") ("HOLD"))
     ("NEXT" ("WAIT") ("CNCL") ("HOLD"))
     ("DONE" ("WAIT") ("CNCL") ("HOLD"))))
)

;;selecting keys from the fast todo selection key menu
(setq org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-log-done 'time
)
;; TODOs:1 ends here

;; [[file:config.org::*Org-super-agenda][Org-super-agenda:1]]
(use-package org-super-agenda
:after org-agenda
:config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups '(
    (:name "Today"
     :time-grid t
     :scheduled today)
    (:name "Due today"
           :deadline today)
    (:name "Important"
     :priority "A")
    (:name "Overdue"
     :deadline past)
    (:name "Due soon"
     :deadline future)
    (:name "Waiting"
     :todo "WAIT")
    (:name "Someday"
     :todo "SOMEDAY")
  ))
)
;; Org-super-agenda:1 ends here

;; [[file:config.org::*Calendar][Calendar:1]]
(use-package calendar
:hook
  (calendar-mode . olivetti-mode)
  (calendar-mode . solaire-mode)
:config
  (setq calendar-date-style 'iso)
)
;; Calendar:1 ends here

;; [[file:config.org::*GTD][GTD:1]]
(use-package org-gtd
:after org
:init
  (setq org-gtd-update-ack "3.0.0")
:config
;; where org-gtd will put its files.
(setq org-gtd-directory "~/gtd/")
;; package: https://github.com/Malabarba/org-agenda-property
;; this is so you can see who an item was delegated to in the agenda
(setq org-agenda-property-list '("DELEGATED_TO"))
;; I think this makes the agenda easier to read
(setq org-agenda-property-position 'next-line)
;; package: https://www.nongnu.org/org-edna-el/
;; org-edna is used to make sure that when a project task gets DONE,
;; the next TODO is automatically changed to NEXT.
(setq org-edna-use-inheritance t)
(org-edna-load)
:bind
(("C-c d c" . org-gtd-capture) ;; add item to inbox
 ("C-c d a" . org-agenda-list) ;; see what's on your plate today
 ("C-c d p" . org-gtd-process-inbox) ;; process entire inbox
 ("C-c d n" . org-gtd-show-all-next) ;; see all NEXT items
 ;; see projects that don't have a NEXT item
 ("C-c d s" . org-gtd-show-stuck-projects)
 ;; the keybinding to hit when you're done editing an item in the
 ;; processing phase
 ("C-c d f" . org-gtd-clarify-finalize)))
;; GTD:1 ends here

;; [[file:config.org::*Ox][Ox:1]]
(use-package ox
:after org
:defer t
:config
  (setq
    org-export-with-toc t
          org-export-with-tags 'not-in-toc
          org-export-with-drawers nil
          org-export-with-priority t
          org-export-with-footnotes t
          org-export-with-smart-quotes t
          org-export-with-section-numbers t
          org-export-with-sub-superscripts '{}
    org-export-use-babel t
          org-export-headline-levels 9
          org-export-coding-system 'utf-8
          org-export-with-broken-links 'mark
  )
)
;; Ox:1 ends here

;; [[file:config.org::*HTML][HTML:1]]
(use-package ox-html
  :defer t
  :after ox
  :config
  (setq org-html-doctype "html5"
          org-html-html5-fancy t
          org-html-checkbox-type 'unicode
          org-html-validation-link nil))
;; HTML:1 ends here

;; [[file:config.org::*HTML][HTML:2]]
(use-package htmlize
  :defer t
  :config
  (setq htmlize-pre-style t
          htmlize-output-type 'inline-css))
;; HTML:2 ends here

;; [[file:config.org::*grip-mode][grip-mode:1]]
(use-package grip-mode
:after org
)
;; grip-mode:1 ends here

;; [[file:config.org::*PDF][PDF:1]]

;; PDF:1 ends here

;; [[file:config.org::*LaTeX][LaTeX:1]]
(use-package ox-latex
:defer t
:after ox
:config
;; 使用xelatex，配合当前org文件最开始的配置来正常输出中文
  ;; 这类笔记基本不可能是全英文，所以就安心用xelatex算了
  (setq org-latex-pdf-process '("xelatex -file-line-error -interaction nonstopmode %f"
                                "bibtex %b"
                                "xelatex -file-line-error -interaction nonstopmode %f"
                                "xelatex -file-line-error -interaction nonstopmode %f"))

  ;; 生成PDF后清理辅助文件
  ;; https://answer-id.com/53623039
  (setq org-latex-logfiles-extensions
    (quote ("lof" "lot" "tex~" "tex" "aux"
      "idx" "log" "out" "toc" "nav"
      "snm" "vrb" "dvi" "fdb_latexmk"
      "blg" "brf" "fls" "entoc" "ps"
      "spl" "bbl" "xdv")))

  ;; 图片默认宽度
  (setq org-image-actual-width '(300))

  (setq org-export-with-sub-superscripts nil)

  ;; 不要自动创建备份文件
  (setq make-backup-files nil)
  (setq org-latex-listings t)
  (add-to-list
   'org-latex-classes
   '("elegantpaper"
     "\\documentclass[lang=cn]{elegantpaper}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )
;; LaTeX:1 ends here

;; [[file:config.org::*Ipynb][Ipynb:1]]
(use-package ox-ipynb
:after ox
  )
;; Ipynb:1 ends here

;; [[file:config.org::*Slides][Slides:2]]
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
;; Slides:2 ends here

;; [[file:config.org::*Markdown][Markdown:1]]
(use-package ox-gfm
:defer t
:after ox
)
;; Markdown:1 ends here

;; [[file:config.org::*MS-Office][MS-Office:1]]
(use-package ox-pandoc
:defer t
:after ox
:config
  (setq org-pandoc-format-extensions
        '(markdown_github+pipe_tables+raw_html)
  )
)
;; MS-Office:1 ends here

;; [[file:config.org::*Static HTML][Static HTML:1]]
(use-package ox-publish
:defer t
)
;; Static HTML:1 ends here

;; [[file:config.org::*Hugo][Hugo:1]]
(use-package ox-hugo
:after ox
:commands (org-hugo-export-as-md org-hugo-export-to-md)
:init
  (setq org-hugo-base-dir "~/Blog"
        org-hugo-front-matter-format "yaml"
  )
)
;; Hugo:1 ends here

;; [[file:config.org::*Hugo][Hugo:2]]
(use-package easy-hugo
:defer t
)
;; Hugo:2 ends here

;; [[file:config.org::*Telega][Telega:1]]
(use-package telega
:defer t
)
;; Telega:1 ends here

;; [[file:config.org::*PDF-Tools][PDF-Tools:1]]
(use-package pdf-tools
;; manually update
;; after each update we have to call:
;; Install pdf-tools but don't ask or raise error (otherwise daemon mode will wait for input)
;; (pdf-tools-install t t t)
:magic ("%PDF" . pdf-view-mode)
:mode (("\\.pdf\\'" . pdf-view-mode))
:hook ((pdf-view-mode . pdf-view-init))
:bind (:map pdf-view-mode-map
       ("C-s" . isearch-forward)
       ("M-w" . pdf-view-kill-ring-save)
       ("M-p" . print-pdf))
:config
(defun pdf-view-init ()
  "Initialize pdf-tools view like enabline TOC functions or use dark theme at night."

  ;; Use dark theme when opening PDFs at night time
  (let ((hour (string-to-number (format-time-string "%H"))))
    (when (or (< hour 5) (< 20 hour))
      (pdf-view-midnight-minor-mode)))

  ;; Enable pdf-tools minor mode to have features like TOC extraction by pressing `o'.
  (pdf-tools-enable-minor-modes)

  ;; Disable while-line-or-region to free keybindings.
  (whole-line-or-region-local-mode -1))

;; Use `gtklp' or `hp-print' to print as it has better cups support
(setq print-pdf-command "hp-print")
(defun print-pdf (&optional pdf)
  "Print PDF using external program defined in `print-pdf-command'."
  (interactive "P")
  (start-process-shell-command
   print-pdf-command nil (concat print-pdf-command " " (shell-quote-argument (or pdf (buffer-file-name))))))

;; more fine-grained zooming; +/- 10% instead of default 25%
(setq pdf-view-resize-factor 1.1)
;; Always use midnight-mode and almost same color as default font.
;; Just slightly brighter background to see the page boarders
(setq pdf-view-midnight-colors '("#c6c6c6" . "#363636")))
;; PDF-Tools:1 ends here

;; [[file:config.org::*Elfeed][Elfeed:1]]
(use-package elfeed
:defer t
:config
  (setq elfeed-feeds '(
      ("http://nullprogram.com/feed/" blog emacs)
      "http://www.50ply.com/atom.xml"  ; no autotagging
      ("http://nedroid.com/feed/" webcomic)
    )
  )
)
;; Elfeed:1 ends here

;; [[file:config.org::*Eaf][Eaf:1]]
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
;; Eaf:1 ends here

;; [[file:config.org::*Tetris][Tetris:1]]
(add-hook 'tetris-mode-hook
  (defun config/tetris-center ()
    (config/window-center 76)
  )
)
;; Tetris:1 ends here

;; [[file:config.org::*2048][2048:1]]
(add-hook '2048-mode-hook
  (defun config/2048-center ()
    (config/window-center 35)
  )
)
;; 2048:1 ends here

;; [[file:config.org::*Emacs-Everywhere][Emacs-Everywhere:1]]

;; Emacs-Everywhere:1 ends here

;; [[file:config.org::*End][End:1]]
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
;; End:1 ends here

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
  (setq use-dialog-box nil)
  (setq confirm-kill-emacs 'y-or-n-p)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  )
(global-auto-revert-mode)

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

(use-package no-littering)

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

(use-package general)
;; General.el:1 ends here

;; [[file:config.org::*Meow.el][Meow.el:1]]
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
 (meow-global-mode)
)
;; Meow.el:1 ends here

;; [[file:config.org::*Mode leader][Mode leader:1]]
(defvar my/mode-leader "C-<escape>")

(defmacro my/key (&optional key)
  `(general-key
       (if ,key
           (concat ,my/mode-leader " " ,key)
         ,my/mode-leader)))

(general-create-definer my/mode-leader-def
  :prefix my/mode-leader
  :wk-full-keys nil
)
;; Mode leader:1 ends here

;; [[file:config.org::*Meow-normal][Meow-normal:1]]
(meow-normal-define-key
  '("<escape>" . meow-cancel-selection)
  '("SPC" . my/leader-prefix-cmd) ;; defined latter
  '("1" . meow-expand-1) '("2" . meow-expand-2)
  '("3" . meow-expand-3) '("4" . meow-expand-4)
  '("5" . meow-expand-5) '("6" . meow-expand-6)
  '("7" . meow-expand-7) '("8" . meow-expand-8)
  '("9" . meow-expand-9) '("0" . meow-expand-0)
  '("*" . tempel-insert) '("+" . tempel-expand)

  '("q" . meow-quit) '("Q" . meow-quit)
     ;'("w" . meow-window) '("W" . meow-window)
  '("e" . embark-dwim) '("E" . embark-act)
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
;; Meow-normal:1 ends here

;; [[file:config.org::*Meow-motion][Meow-motion:1]]
(meow-motion-overwrite-define-key
 '("j" . meow-left)
 '("k" . meow-prev)
 '("l" . meow-next)
 '(";" . meow-right)
 '("SPC" . my/leader-prefix-cmd) ;; defined latter
 '("<escape>" . ignore)
)
;; Meow-motion:1 ends here

;; [[file:config.org::*Meow-keypad][Meow-keypad:1]]
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
;; Meow-keypad:1 ends here

;; [[file:config.org::*Meow-insert][Meow-insert:1]]
(general-def
:keymaps '(global-map)
  "C-v"       '(clipboard-yank              :wk "paste")
  "C-SPC"     '(toggle-input-method         :wk "input method")
  "C-j"       (my/key "C-j")
  "C-;"       (my/key "C-;")
  "C-k"       (my/key "C-k")
  "C-l"       (my/key "C-l")

  ;"C-/"      '(yank                        :wk "comment-dwim")
)
(general-def
:keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
  "\\"         (my/key)
  "M-j"       '(windmove-left               :wk " Win H ")
  "M-k"       '(windmove-up                 :wk " Win K ")
  "M-l"       '(windmove-down               :wk " Win J ")
  "M-;"       '(windmove-right              :wk " Win L ")
  "M-,"       '(sort-tab-select-prev-tab    :wk " Tab L ")
  "M-."       '(sort-tab-select-next-tab    :wk " Tab R ")
)
;; Meow-insert:1 ends here

;; [[file:config.org::*Global leader][Global leader:1]]
(general-create-definer my/leader
:prefix-command 'my/leader-prefix-cmd
:prefix-map 'my/leader-prefix-map
:wk-full-keys nil
  "DEL"     '(which-key-undo                 :wk "undo-key")
)
;; Global leader:1 ends here

;; [[file:config.org::*Global leader][Global leader:2]]
(my/leader
  "SPC"     (my/key)
  "/"       '(comment-dwim                   :wk "󱀢 comment")
  "c"       '(nil                            :wk "consult")
  "d"       '(dirvish-side                   :wk "󰙅 dirvish-side ")
  "D"       '(dirvish                        :wk "󰙅 dirvish")
  "r"       '(nil                            :wk "run")
  "s"       '(save-buffer                    :wk " save")
  "t"       '(nil                            :wk "toggle")
  "w"       '(nil                            :wk "workspace")
  "x"       '(consult-mode-command           :wk " execute")
  "z"       '(vundo                          :wk "󰕌 visual undo")
)
;; Global leader:2 ends here

;; [[file:config.org::*Global leader][Global leader:3]]
(my/leader :infix "r"
  "b"      '(nil                             :wk "borg")
  "e"      '(elfeed                          :wk " elfeed")
  "t"      '(telega                          :wk " telega")
  "p"      '(profiler-start                  :wk " profiler")
  "P"      '(profiler-stop                   :wk "profiler stop")
)
;; Global leader:3 ends here

;; [[file:config.org::*Global leader][Global leader:4]]
;; Borg
(my/leader :infix "rb"
  "a"       '(borg-assimilate                :wk "󱧕 assimilate ")
  "b"       '(borg-build                     :wk "󱇝 build")
  "c"       '(borg-clone                     :wk " clone")
  "d"       '(borg-remove                    :wk "󱧖 delete")
  "r"       '(borg-activate                  :wk " run")
  )
;; Global leader:4 ends here

;; [[file:config.org::*Global leader][Global leader:5]]
(my/leader :infix "c"
  "l"       '(consult-line                   :wk "line")
  "L"       '(consult-line-multi             :wk "line multi")
  "o"       '(consult-outline                :wk "outline")
  "i"       '(consult-imenu                  :wk "imenu")
  "I"       '(consult-imenu-multi            :wk "imenu multi")
  "r"       '(consult-ripgrep                :wk "ripgrep")
  "m"       '(consult-mark                   :wk "mark")
  "x"       '(consult-mode-command           :wk "execute")
  )
;; Global leader:5 ends here

;; [[file:config.org::*Global leader][Global leader:6]]
(my/leader :infix "w" ;; workspaces
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
;; Global leader:6 ends here

;; [[file:config.org::*Global leader][Global leader:7]]
;; toggle
(my/leader :infix "t"
  "o"       '(olivetti-mode                  :wk "󰼭 olivetti")
  "l"       '(nil                            :wk " line Number ")
  )

(my/leader :infix "tl"
  "DEL" '(which-key-undo                     :wk "󰕍 Undo key   ")
  "n"   '(config/toggle-line-number-nil      :wk "󰅖 Nil        ")
  "a"   '(config/toggle-line-number-absolute :wk "󰱇 Absolute   ")
  "r"   '(config/toggle-line-number-relative :wk "󰰠 Relative   ")
  "v"   '(config/toggle-line-number-visual   :wk " Visual     ")
  "h"   '(hl-line-mode                       :wk "󰸱 Hl-line")
  )
;; Global leader:7 ends here

;; [[file:config.org::*Global leader][Global leader:8]]
;; Git
(my/leader :infix "g"
  ""        '(nil                            :wk "git")
  "g"       '(magit                          :wk " magit")
  )
;; Global leader:8 ends here

;; [[file:config.org::*Global leader][Global leader:9]]
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
;; Global leader:9 ends here

;; [[file:config.org::*Transient][Transient:1]]
(use-package transient-posframe
:config
  (setq transient-posframe-min-height 1)
  (setq transient-posframe-poshandler
        'posframe-poshandler-frame-bottom-right-corner)
  (transient-posframe-mode)
)
;; Transient:1 ends here

;; [[file:config.org::*Which-key.el][Which-key.el:1]]
(use-package which-key
:init
  ;; contents
  (setq which-key-sort-order 'which-key-key-order)
  (setq which-key-sort-uppercase-first nil)
  ;; delays
  (setq which-key-idle-delay 0.01) ; the first idle
  (setq which-key-idle-secondary-delay 0.01) ; set to 0 will cause some problems
  (setq which-key-show-early-on-C-h t)
  ;; arrangements
  (setq which-key-max-display-columns nil)
  (setq which-key-max-description-length 25) ;
  (setq which-key-allow-imprecise-window-fit t) ; reduce delay
  (setq which-key-show-transient-maps t)
  (setq which-key-frame-max-height 60)
  ;; window
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right)
  ;; characters
  (setq which-key-prefix-prefix "󰜄 ")
  (setq which-key-separator " ") ; yeah I don't like the arrow
  ;; (dolist (replace
  ;;          '((("SPC" . nil) . ("󱁐" . nil))
  ;;            (("TAB" . nil) . ("󰌒" . nil))
  ;;            (("RET" . nil) . ("󰌑" . nil))
  ;;            (("DEL" . nil) . ("󰭜" . nil))
  ;;            ))
  ;;   (add-to-list 'which-key-replacement-alist replace))
:config
  (set-face-attribute 'which-key-key-face nil :inherit 'fixed-pitch)
  (which-key-mode 1)
)
;; Which-key.el:1 ends here

;; [[file:config.org::*Which-key.el][Which-key.el:2]]
(use-package which-key-posframe
:after which-key
:config
  (setq which-key-posframe-poshandler
        'posframe-poshandler-frame-bottom-right-corner)
  (setq which-key-max-display-columns 1)
  (setq which-key-min-display-lines 1)
  (which-key-posframe-mode)
)
;; Which-key.el:2 ends here

;; [[file:config.org::*Rime.el][Rime.el:1]]
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
;; Rime.el:1 ends here

;; [[file:config.org::*Face-remap.el][Face-remap.el:1]]
(use-package face-remap
:config
  (set-face-attribute 'default nil
    ;; :font "Sarasa Gothic SC Nerd Font"
    :font "IBM Plex Sans"
    ;; :font "IBM Plex Serif"
    :height 150)
  (set-face-attribute 'fixed-pitch nil
    ;: :font "Sarasa Fixed SC"
    ;; :font "RobotoMono Nerd Font Mono"
    ;; :font "CommitMono Nerd Font Mono"
    :font "IBM Plex Mono Text"
    ;; :font "Monaspace Neon"
    :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
    ;; :font "Sarasa Gothic SC Nerd Font"
    :font "IBM Plex Mono Text"
    ;; :font "IBM Plex Serif"
    :height 1.0)
  (set-face-attribute 'variable-pitch nil
    ;; :font "Sarasa Gothic SC Nerd Font"
    :font "IBM Plex Serif"
    :height 1.0)

  ;; patches nerd font
  (set-fontset-font t 'han "Source Han Serif SC")
  (set-fontset-font t 'symbol "Noto Sans Symbols")
  (set-fontset-font t 'nil "Noto Emoji")
  (set-fontset-font t 'nil "Symbols Nerd Font")

  (set-face-attribute 'link nil :underline t :bold nil)

  (defun my/use-fixed-pitch ()
    (interactive)
    (face-remap-add-relative 'default 'fixed-pitch)
    (turn-off-solaire-mode)
  )
  (defun my/use-variable-pitch ()
    (interactive)
    (face-remap-add-relative 'default 'variable-pitch)
  )
  (dolist (hook '(
      dired-mode-hook
      comint-mode-hook
      magit-mode-hook
      prog-mode-hook
      profiler-report-mode-hook
      conf-mode-hook conf-ts-mode-hook
      toml-mode-hook toml-ts-mode-hook
    ))
    (add-hook hook 'my/use-fixed-pitch))
)
;; Face-remap.el:1 ends here

;; [[file:config.org::*Zooming In/Out][Zooming In/Out:1]]
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
;; Zooming In/Out:1 ends here

;; [[file:config.org::*Line Space][Line Space:1]]
(defun my/toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))
;; Line Space:1 ends here

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

;; [[file:config.org::*Transparency][Transparency:1]]
(defun my/set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value))
;; Transparency:1 ends here

;; [[file:config.org::*Olivetti][Olivetti:1]]
(use-package olivetti
:defer nil
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

;; [[file:config.org::*Visual-fill-column][Visual-fill-column:1]]
(setq-default visual-fill-column-center-text t)
;; Visual-fill-column:1 ends here

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
:config
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
)
;; Vertico:1 ends here

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

          (tempel-insert
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
          ;; doom-modeline-height 37
          doom-modeline-enable-word-count t
          doom-modeline-modal nil
          )
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
      (concat user-emacs-directory "assets/EmaxBound.xpm")
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
;; :hook (magit-mode . olivetti-mode)
:config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append
                          )
  (setq magit-show-long-lines-warning nil)
  (set-face-attribute 'magit-hash nil :inherit 'fixed-pitch)
  (set-face-attribute 'magit-diff-removed-highlight nil :inherit 'fixed-pitch)
  (set-face-attribute 'magit-diff-context-highlight nil :inherit 'fixed-pitch)
  (set-face-attribute 'magit-diff-added-highlight nil :inherit 'fixed-pitch)

  (general-def
  :keymaps '(magit-mode-map)
    "n"   'magit-gitignore
    "p"   'magit-push
    "P"   'magit-pull
    "DEL" 'magit-discard
  )
)
;; Magit:1 ends here

;; [[file:config.org::*Diff-hl][Diff-hl:1]]
(use-package diff-hl
:custom-face
  (diff-hl-change ((t (:background "#2c5f72" :foreground "#77a8d9"))))
  (diff-hl-delete ((t (:background "#844953" :foreground "#f27983"))))
  (diff-hl-insert ((t (:background "#5E734A" :foreground "#a6cc70"))))
:init
  (setq diff-hl-draw-borders nil)
:config
  ;(global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'conf-mode-hook #'diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)


  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))
)
;; Diff-hl:1 ends here

;; [[file:config.org::*Diredfl][Diredfl:1]]
(use-package diredfl
:after dired
:hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
:config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
)
;; Diredfl:1 ends here

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
    "TAB"    '(dirvish-toggle-subtree    :wk "subtree toggle")
    "j"      '(dired-up-directory        :wk "up-dir")
    ";"      '(dired-find-file           :wk "open/toggle")
    "C-b"    '(dired-up-directory        :wk "up-dir")
    "C-f"    '(dired-find-file           :wk "open/toggle")
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

;; [[file:config.org::*Embark][Embark:1]]
(use-package embark
  ;; :bind
  ;; (("C-." . embark-act)         ;; pick some comfortable binding
  ;;  ("C-;" . embark-dwim)        ;; good alternative: M-.
  ;;  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

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
:commands holo-layer-enable
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

;; [[file:config.org::*Keybindings][Keybindings:1]]
(general-def
:keymaps '(prog-mode-map)
:wk-full-keys nil
  "C-<escape>" '(my/prog-cmd :wk "Prog")
)

(general-def
:prefix-command 'my/prog-cmd
:prefix-map 'my/prog-map
:wk-full-keys nil
  "C-j" '()
  "C-k" '(backward-paragraph  :wk " para")
  "C-l" '(forward-paragraph :wk " para")
  "C-;" '()
  )
;; Keybindings:1 ends here

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
  (setq pulse-delay 0.05
        pulse-iterations 3)
  (setq pulsing-cursor-blinks 20)
  (set-face-attribute 'pulsing-cursor-overlay-face1 nil
                      :background (face-background 'cursor))
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
:config
  (defun my/read-face-advice (orig-fun &rest args)
    "Advice function to turn off hl-line-mode before calling describe-face."
    (if hl-line-mode
        (progn
          (hl-line-mode -1)
          (apply orig-fun args)
          (hl-line-mode 1)
          )
      (apply orig-fun args)))
  ;; (advice-add 'read-face-name :around #'my/read-face-advice)
  ;; (global-hl-line-mode)
)
;; Hl-line:1 ends here

;; [[file:config.org::*Whitespace mode][Whitespace mode:1]]
(use-package whitespace
:config
  (setq whitespace-line-column 1000) ;; do not want line to be highlighted
)
  ;; (config/leader :infix "t"
  ;;   "SPC"  '(whitespace-mode  :wk "󰡭 Show Space")
  ;; )
;; Whitespace mode:1 ends here

;; [[file:config.org::*Line Number][Line Number:1]]
(use-package emacs
:hook (prog-mode . config/toggle-line-number-absolute)
:init (setq display-line-numbers-width 4)
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
)
;; Line Number:1 ends here

;; [[file:config.org::*Rainbow-mode][Rainbow-mode:1]]

;; Rainbow-mode:1 ends here

;; [[file:config.org::*Simple-call-tree][Simple-call-tree:1]]
(use-package simple-call-tree :defer t)
;; Simple-call-tree:1 ends here

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

;; [[file:config.org::*Highlight-Indent-Guides][Highlight-Indent-Guides:1]]
(use-package highlight-indent-guides
:hook (prog-mode . highlight-indent-guides-mode)
:config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character 9474
        highlight-indent-guides-auto-enabled nil
        highlight-indent-guides-responsive nil
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

;; [[file:config.org::*Highlight-parentheses][Highlight-parentheses:1]]
(use-package highlight-parentheses
:init
  (show-paren-mode 0)
:config
  (setq highlight-parentheses-highlight-adjacent 1
        highlight-parentheses-attributes '((:box (:line-width (-1 . -1))))
        highlight-parentheses-colors nil
        highlight-parentheses-delay 0.03)
  (global-highlight-parentheses-mode)
  )
;; Highlight-parentheses:1 ends here

;; [[file:config.org::*Rainbow-Delimiters][Rainbow-Delimiters:1]]
(use-package rainbow-delimiters
:hook (prog-mode . rainbow-delimiters-mode)
)
;; Rainbow-Delimiters:1 ends here

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

  (my/mode-leader-def
  :keymaps '(tempel-map)
    "C-j"  'tempel-previous
    "C-;"  'tempel-next
  )
)
;; Tempel:1 ends here

;; [[file:config.org::*Tempel][Tempel:2]]
;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
:after tempel
)
;; Tempel:2 ends here

;; [[file:config.org::*LSP-bridge][LSP-bridge:1]]
(use-package lsp-bridge
:init
  (setq lsp-bridge-java-lsp-server "jdt-language-server")
:config
  (global-lsp-bridge-mode)
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
  ;; (general-def
  ;;   :keymaps 'fingertip-mode-map
  ;;     "(" 'fingertip-open-round
  ;;     "[" 'fingertip-open-bracket
  ;;     "{" 'fingertip-open-curly
  ;;     ")" 'fingertip-close-round
  ;;     "]" 'fingertip-close-bracket
  ;;     "}" 'fingertip-close-curly
  ;;     "=" 'fingertip-equal

  ;;     "%" 'fingertip-match-paren
  ;;     "\"" 'fingertip-double-quote
  ;;     "'" 'fingertip-single-quote

  ;;     "SPC" 'fingertip-space
  ;;     "RET" 'fingertip-newline

  ;;     "M-o" 'fingertip-backward-delete
  ;;     "C-d" 'fingertip-forward-delete
  ;;     "C-k" 'fingertip-kill

  ;;     "M-\"" 'fingertip-wrap-double-quote
  ;;     "M-'" 'fingertip-wrap-single-quote
  ;;     "M-[" 'fingertip-wrap-bracket
  ;;     "M-{" 'fingertip-wrap-curly
  ;;     "M-(" 'fingertip-wrap-round
  ;;     "M-)" 'fingertip-unwrap

  ;;     "M-p" 'fingertip-jump-right
  ;;     "M-n" 'fingertip-jump-left
  ;;     ;"M-:" 'fingertip-jump-out-pair-and-newline

  ;;     "C-j" 'fingertip-jump-up )
)
;; Fingertip:1 ends here

;; [[file:config.org::*Treesitter-context][Treesitter-context:1]]
(use-package treesitter-context
:config
  (add-hook 'python-ts-mode-hook #'treesitter-context-mode)
  )
;; Treesitter-context:1 ends here

;; [[file:config.org::*Lazycat's method][Lazycat's method:1]]
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
      (common-lisp . ("https://github.com/theHamsta/tree-sitter-commonlisp"))
      (csharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
      (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
      (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
      (go . ("https://github.com/tree-sitter/tree-sitter-go"))
      (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
      (html . ("https://github.com/tree-sitter/tree-sitter-html"))
      (java . ("https://github.com/tree-sitter/tree-sitter-java.git"))
      (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
      (json . ("https://github.com/tree-sitter/tree-sitter-json"))
      (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
      (make . ("https://github.com/alemuller/tree-sitter-make"))
      (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil
        "tree-sitter-markdown/src"))
      (nix . ("https://github.com/nix-community/tree-sitter-nix.git"))
      (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
      (org . ("https://github.com/milisims/tree-sitter-org"))
      (python . ("https://github.com/tree-sitter/tree-sitter-python"))
      (php . ("https://github.com/tree-sitter/tree-sitter-php"))
      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil
          "typescript/src"))
      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil
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
        (nix-mode        . nix-ts-mode)
        (python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (rust-mode       . rust-ts-mode)
        ))
  ;; (advice-add 'org-src-get-lang-mode :filter-return #'my/remap-mode)

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
;; Lazycat's method:1 ends here

;; [[file:config.org::*Auto-save][Auto-save:1]]
(use-package auto-save
:after lsp-bridge
:config
  (auto-save-enable)
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace t)
)
;; Auto-save:1 ends here

;; [[file:config.org::*Elisp][Elisp:1]]
(setq-default lexical-binding t)
;; Elisp:1 ends here

;; [[file:config.org::*Lisp][Lisp:1]]
(use-package lisp-mode
:mode "\\.yuck\\'"
)
;; Lisp:1 ends here

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

;; [[file:config.org::*Markdown][Markdown:1]]
(use-package markdown-mode
:config
  (dotimes (i 6)
    (set-face-attribute (intern (format "markdown-header-face-%d" (1+ i)))
                        nil
                        :inherit
                        (intern (format "outline-%d" (1+ i)))))
)
;; Markdown:1 ends here

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

;; [[file:config.org::*Rust][Rust:1]]
(use-package ron-mode
:mode "\\.ron\\'"
)
;; Rust:1 ends here

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
  (set-face-attribute 'outline-1 nil :height 1.8 )
  (set-face-attribute 'outline-2 nil :height 1.6 )
  (set-face-attribute 'outline-3 nil :height 1.4 )
  (set-face-attribute 'outline-4 nil :height 1.3 )
  (set-face-attribute 'outline-5 nil :height 1.2 )
  (set-face-attribute 'outline-6 nil :height 1.1 )
  (set-face-attribute 'org-document-title nil :height 2.5 :bold t)
  (set-face-attribute 'org-document-info nil :height 1.8 :bold t)
  (set-face-attribute 'org-document-info-keyword nil
     :inherit 'org-document-info)
  (set-face-attribute 'org-block nil
    :extend t :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil
    :background (face-background 'default) :height 0.9 :inherit 'default)
)
;; Fonts:1 ends here

;; [[file:config.org::*Org-modern][Org-modern:1]]
(use-package org-modern
;;:hook (org-mode . org-modern-mode)
:config
  (setq org-modern-keyword
     '(("author" . "◇")
      ("title" . "◈")
      ("subtitle" . "◉")
      ("html" . "󰅱 ")
      ("results" . "results")
      (t . t)))
  (setq org-modern-star
   ;;'("⚀" "⚁" "⚂" "⚃" "⚄" "⚅")
   '("󱅊" "󱅋" "󱅌" "󱅍" "󱅎" "󱅏")
   ;; '("󰇊" "󰇋" "󰇌" "󰇍" "󰇎" "󰇏")
  )
  ;; (set-face-attribute 'org-modern-symbol nil :family "Noto Sans Symbols 2")
; '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")
; '("◉" "○" "◈" "◇" "✳")
  (setq org-modern-list ;; for '+' '-' '*' respectively
      '((43 . "∙") (45 . "➤") (42 . "✽"))
  )
  (setq org-modern-block-fringe nil)
  (setq org-modern-todo nil)
  ;; (setq org-modern-block-name )
  (setq org-modern-block-name
        '("⇲ " . "⇱ ")
        ;; '("◻ " . "◻ ")
        ;; '("" . "")
        ;; '("󰨔" . "󰨔")
        ;; '("󰨓" . "󰨓")
        ;; '("󰝤 " . "󰝤 ") ; nf-md-square
        )
  (set-face-attribute 'org-modern-block-name nil
     :inherit 'variable-pitch)
  (set-face-attribute 'org-meta-line nil
     :height 0.9)
  (set-face-attribute 'org-modern-horizontal-rule nil
      :strike-through (face-foreground 'org-meta-line) :inherit 'org-hide)

  (setq org-modern-table nil)
  (global-org-modern-mode)
)
;; Org-modern:1 ends here

;; [[file:config.org::*Org-modern][Org-modern:2]]
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
;; Org-modern:2 ends here

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

;; [[file:config.org::*Keybindings][Keybindings:1]]
(setq org-return-follows-link t)
;; Keybindings:1 ends here

;; [[file:config.org::*Keybindings][Keybindings:2]]
(my/mode-leader-def
  :prefix-command 'my/org-cmd
  :prefix-map 'my/org-map
  :keymaps '(org-mode-map)
  ""    '(nil                              :wk "org-mode")
  "a"   '(org-attach                       :wk "attach")
  "b"   '(nil                              :wk "Babel")
  "c"   '(org-ctrl-c-ctrl-c                :wk "execute")
  "d"   '(org-deadline                     :wk "deadline")
  "e"   '(org-edit-special                 :wk "edit")
  ;; "f"   '(nil                              :wk "")
  "g"   '(org-goto                         :wk "goto")
  ;; "h"   '(                                 :wk "")
  ;; "i"   '(                                 :wk "")
  "i"   '(org-insert-structure-template    :wk "structure")
  ;; "j"   '(                                 :wk "")
  ;; "k"   '(                                 :wk "")
  "l"   '(org-insert-link                  :wk "link")
  "m"   '(org-toggle-inline-images         :wk "toggle image")
  ;; "n"   '(                                 :wk "")
  ;; "o"   '(                                 :wk "")
  "p"   '(org-export-dispatch              :wk "export")
  ;; "q"   '(                                 :wk "")
  "r"   '(org-fold-reveal                  :wk "reveal")
  "s"   '(org-schedule                     :wk "schedule")
  "t"   '(org-todo                         :wk "todo")
  ;; "u"   '(nil                              :wk "")
  "v"   '(org-download-clipboard           :wk "paste image")
  "w"   '(org-refile                       :wk "refile")
  "x"   '(nil                              :wk "latex")
  "y"   '(org-evaluate-time-range          :wk "time range")
  ;; "z"   '(nil                              :wk "")
  "C-j" '(my/outline-left                  :wk "dwim ")
  "C-k" '(my/outline-up                    :wk "dwim ")
  "C-l" '(my/outline-down                  :wk "dwim ")
  "C-;" '(my/outline-right                 :wk "dwim ")
  )
;; Keybindings:2 ends here

;; [[file:config.org::*Keybindings][Keybindings:3]]
(my/mode-leader-def
  :keymaps '(org-mode-map)
  :infix "b"
  "t" '(org-babel-tangle                   :wk "tangle")
  "d" '(org-babel-demarcate-block          :wk "demarcate")
  )
(my/mode-leader-def
  :keymaps '(org-mode-map)
  :infix "x"
  "c" '(org-latex-preview-clear-cache      :wk "clear")
  "p" '(org-latex-preview                  :wk "preview")
  "a" '(org-latex-preview-auto-mode        :wk "auto")
  "r" '(my/org-latex-preview-reload        :wk "reload")
  )
;; Keybindings:3 ends here

;; [[file:config.org::*Keybindings][Keybindings:4]]
(my/mode-leader-def
  :keymaps '(org-src-mode-map)
  :wk-full-keys nil
  "e"   'org-edit-src-exit
  "k"   'org-edit-src-abort
  )
;; Keybindings:4 ends here

;; [[file:config.org::*outline functions][outline functions:1]]
(defvar my/recenter 6)
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
  (recenter my/recenter)
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
  (recenter my/recenter)
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
  (recenter my/recenter))

(defun my/outline-right ()
  (interactive)
  (outline-show-children)
  (outline-show-entry)
  (if (outline-has-subheading-p)
      (progn (outline-next-heading)
             (outline-show-children)
             (outline-show-entry)))
  (recenter my/recenter))
;; outline functions:1 ends here

;; [[file:config.org::*Src block][Src block:1]]
(use-package org
:init
  (setq org-babel-load-languages
        '(
          (awk . t)
          (shell . t)
          (eshell . t)

          (emacs-lisp . t)
          (lisp . t)
          (haskell . t)
          (clojure . t)
          (scheme . t)
          (org . t)

          (C . t)
          (sql . t)
          (jupyter . t)
          (java . t)
          (lua . t)
          (js . t)

          (dot . t)
          (plantuml . t)

          (R . t)
          (python . t)
          (octave . t)
          (matlab . t)
          (julia . t)
          ))
:config
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation nil
        org-src-ask-before-returning-to-edit-buffer nil
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
:config
  (setq org-latex-preview-numbered t)
  (plist-put org-latex-preview-options :zoom 1.25)
  (let ((pos (assoc 'dvisvgm org-latex-preview-process-alist)))
    (plist-put (cdr pos) :image-converter '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f")))

  (defun my/org-latex-preview-reload ()
    (interactive)
    (call-interactively 'org-latex-preview-clear-cache)
    (org-latex-preview)
    )
)
;; LaTeX:3 ends here

;; [[file:config.org::*Org-download][Org-download:1]]
(use-package org-download
:defer t
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
  (setq org-directory "~/org")
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
    ("h" "Hugo post" entry
     (file+olp "capture.org" "Notes")
     (function org-hugo-new-subtree-post-capture-template))
  )))
)
;; Org-capture:1 ends here

;; [[file:config.org::*Custom-ID][Custom-ID:1]]
(use-package org
:config
  (defun my/org-add-custom-id ()
    "Add CUSTOM_ID property to current heading, skip if already have"
    (interactive)
    (let ((custom-id (org-entry-get nil "CUSTOM_ID")))
      (unless custom-id
        (org-set-property "CUSTOM_ID" (org-id-new))))
    )

  (defun my/org-add-custom-id-to-all-headings ()
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
:init
  (setq org-roam-directory (file-truename "~/roam"))
  (org-roam-db-autosync-mode)
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
     ("c" "coMpany" plain "%?"
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
:defer t
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
  :hook (telega-chat-mode .
                          (lambda ()
                            (visual-fill-column-mode 0)
                            (config/window-center 102)
                            (solaire-mode -1)
                            ))
  :init
  (setq telega-server-libs-prefix "~/.nix-profile")
  (setq telega-avatar-workaround-gaps-for '(return t)) ;; solves broken avatar
  (setq telega-emoji-use-images nil) ;; or emoji will look monochrome and broken
  (setq ;; telega-root
   ;; telega-root-default-view-function 'telega-view-folders
   telega-root-keep-cursor 'track
   telega-root-buffer-name "*Telega Root*"
   telega-root-fill-column 70 ; fill-column
   telega-symbol-folder "  ")
  (setq telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text"))
  (setq telega-brackets
        `(((chat (type private))
           ,(concat " "
             (nerd-icons-mdicon "nf-md-account"
                                :face '(:foreground "#86dffd" :height 0.7))
             " ") " ")
          ((chat (type bot))
           ,(concat " "
             (nerd-icons-mdicon "nf-md-robot"
                                :face '(:foreground "#86dffd" :height 0.7))
             " ") " ")
          ((chat (type basicgroup))
           ,(concat " "
             (nerd-icons-mdicon "nf-md-account_multiple"
                                :face '(:foreground "#70bcff" :height 0.7))
             " ") " ")
          ((chat (type supergroup))
           ,(concat " "
             (nerd-icons-mdicon "nf-md-account_multiple"
                                :face '(:foreground "#70bcff" :height 0.7))
             " ") " ")
          ((chat (type channel))
           ,(concat " "
             (nerd-icons-faicon "nf-fa-feed"
                                :face '(:foreground "#ffa95f" :height 0.7))
             " ") " ")
          ((user (return t))
           ,(concat " "
             (nerd-icons-mdicon "nf-md-account"
                                :face '(:foreground "#86dffd" :height 0.7))
             " ") " ")
          ((return t)
           ,(concat " "
             (nerd-icons-faicon "nf-fa-question_circle"
                                :face '(:foreground "#ff0000" :height 0.7))
             " ") " "))
        )
  (general-def
    :keymaps '(telega-msg-button-map)
    "SPC" nil
    "l"   nil
    "C"   'telega-msg-copy-link
    )

  )
;; Telega:1 ends here

;; [[file:config.org::*Elfeed][Elfeed:1]]
(use-package elfeed
:defer t
:hook
  (elfeed-search-mode .
    (lambda () (config/window-center 110) (solaire-mode -1)))
  (elfeed-show-mode .
    (lambda () (config/window-center 114) (solaire-mode -1)))
:config
  (defun nerd-icon-for-tags (tags)
    "Generate Nerd Font icon based on tags.
  Returns default if no match."
    (cond
     ((member "youtube" tags)
      (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
     ((member "instagram" tags)
      (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
     ((member "emacs" tags)
      (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
     ((member "github" tags)
      (nerd-icons-faicon "nf-fa-github"))
     ((member "zhihu" tags)
      (nerd-icons-mdicon "nf-md-alpha_z_box" :face '(:foreground "#1772F6")))
     ((member "bilibili" tags)
      (nerd-icons-mdicon "nf-md-alpha_b_box" :face '(:foreground "#FB7299")))
     (t
      (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))

  (defun my/elfeed-search-print-entry--better-default (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (date-width (car (cdr elfeed-search-date-format)))
           (title (concat (or (elfeed-meta entry :title)
                              (elfeed-entry-title entry) "")
                          ;; NOTE: insert " " for overlay to swallow
                          " "))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (delq 'unread (elfeed-entry-tags entry))))
           (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
           (title-width (- (frame-width)
                           ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                           date-width elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width) :left))


           ;; Title/Feed ALIGNMENT
           (align-to-feed-pixel (+ 2 date-width
                                   (max elfeed-search-title-min-width
                                        (min title-width elfeed-search-title-max-width)))))
      (insert (propertize date 'face 'elfeed-search-date-face) "  ")
      (insert (propertize title-column 'face title-faces 'kbd-help title))
      (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
      ;; (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when feed-title
        (insert " " (concat (nerd-icon-for-tags tags) " ")
                (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags (insert "(" tags-str ")"))))

  (setq  elfeed-search-print-entry-function
         #'my/elfeed-search-print-entry--better-default)
)
;; Elfeed:1 ends here

;; [[file:config.org::*Elfeed-org][Elfeed-org:1]]
(use-package elfeed-org
:after elfeed
:init
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  (elfeed-org)
)
;; Elfeed-org:1 ends here

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

;; [[file:config.org::*Nov][Nov:1]]
(use-package nov
:defer t)
;; Nov:1 ends here

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
(add-hook 'tetris-mode-hook (lambda () (config/window-center 76)))
;; Tetris:1 ends here

;; [[file:config.org::*2048][2048:1]]
(add-hook '2048-mode-hook (lambda () (config/window-center 35)))
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

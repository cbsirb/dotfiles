;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; It's init.el, that says enough.

;;; Code:

;; Easier customization
(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defvar user/gc-cons-threshold (* 24 gc-cons-threshold))
(csetq gc-cons-threshold most-positive-fixnum)

(csetq user/file-name-handler-alist file-name-handler-alist)
(csetq file-name-handler-alist '())

(add-hook 'after-init-hook
          (lambda ()
            (csetq gc-cons-threshold user/gc-cons-threshold)

            ;; Don't just blindly set it to the old value, maybe someone decided to add something to it
            (csetq file-name-handler-alist (append file-name-handler-alist user/file-name-handler-alist))

            (message "Time to load init file: %s"
                     (emacs-init-time))
            (garbage-collect)))

(defun user/minibuffer-setup-hook ()
  "Hook to run when entering the minibuffer."
  (csetq gc-cons-threshold most-positive-fixnum)
  (setq-local show-trailing-whitespace nil))

(defun user/minibuffer-exit-hook ()
  "Hook to run when exiting the minibuffer."
  (csetq gc-cons-threshold user/gc-cons-threshold))

;; Increase the memory while in the minibuffer
(add-hook 'minibuffer-setup-hook #'user/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'user/minibuffer-exit-hook)

(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(defconst user/custom-file (expand-file-name "custom.el" user-emacs-directory)
  "File used to store settings from Customization UI.")

(when (file-exists-p user/custom-file)
  (load-file user/custom-file))

(csetq load-prefer-newer t)

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init" user-emacs-directory))
  (package-initialize))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Install use-package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(csetq use-package-enable-imenu-support t)
(csetq use-package-expand-minimally t)
(csetq use-package-always-ensure t)
(csetq use-package-compute-statistics t)
(require 'use-package)

;;; Packages needed no matter what, and usually others are depended on it
;; (use-package habamax-theme
;;   :init
;;   (csetq habamax-theme-variable-heading-heights t)
;;   :config
;;   (load-theme 'habamax t))

(use-package parchment-theme
  :config
  (load-theme 'parchment t))

(use-package ignoramus
  :config
  (ignoramus-setup))

(use-package no-littering)

(use-package diminish
  :demand t
  :config
  (with-eval-after-load "eldoc"
    (diminish 'eldoc-mode)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package hydra
  :config
  (hydra-add-font-lock))

;;; Some default settings that I like
(defalias 'yes-or-no-p 'y-or-n-p)

(csetq enable-local-variables :safe)

(csetq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(csetq backup-by-copying t)
(csetq view-read-only t)
(csetq delete-old-versions t)
(csetq make-backup-files nil)
(csetq auto-save-default nil)
(csetq large-file-warning-threshold (* 50 1024 1024))
(csetq directory-free-space-args "-kh")

(csetq completion-ignore-case t)

(csetq sentence-end-double-space nil)

(csetq ad-redefinition-action 'accept)

(csetq auto-window-vscroll nil)
(csetq use-dialog-box nil)
(csetq enable-recursive-minibuffers nil)
(csetq echo-keystrokes 0.1)
(csetq delete-by-moving-to-trash t)
(csetq create-lockfiles nil)
(csetq read-buffer-completion-ignore-case t)
(csetq make-pointer-invisible nil)
(csetq text-quoting-style 'grave)
(csetq next-screen-context-lines 5)
(csetq truncate-lines t)
(csetq select-active-regions nil)
(csetq display-raw-bytes-as-hex t)
(csetq ring-bell-function 'ignore)
(csetq reb-re-syntax 'string)
(csetq save-silently t)

(csetq undo-limit (* 10 undo-limit))
(csetq undo-strong-limit (* 10 undo-strong-limit))

(csetq mark-ring-max 128)
(csetq global-mark-ring-max 256)
(csetq save-interprogram-paste-before-kill t)
(csetq kill-ring-max 128)
(csetq kill-do-not-save-duplicates t)
(csetq read-quoted-char-radix 16)
(csetq eval-expression-print-length nil)
(csetq eval-expression-print-level nil)

(csetq read-file-name-completion-ignore-case t)

(csetq column-number-indicator-zero-based nil)

(csetq disabled-command-function nil)

(csetq inhibit-startup-buffer-menu t)
(csetq initial-scratch-message "")
(csetq inhibit-splash-screen t)
(csetq inhibit-startup-echo-area-message t)
(csetq inhibit-startup-message t)
(fset 'display-startup-echo-area-message #'ignore)

;; "Smooth" mouse scrolling, one line at a time
(csetq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(csetq scroll-conservatively 10000)
(csetq scroll-preserve-screen-position t)
(csetq scroll-margin 7)
(csetq fast-but-imprecise-scrolling t)

(when (eq system-type 'gnu/linux)
  (csetq x-underline-at-descent-line t)
  (csetq x-stretch-cursor t)
  (csetq x-wait-for-event-timeout nil))

(when (eq system-type 'windows-nt)
  (csetq w32-pipe-read-delay 0))

(when (>= emacs-major-version 27)
  (csetq tooltip-resize-echo-area t))

(transient-mark-mode t)
(delete-selection-mode t)
(winner-mode t)
(minibuffer-depth-indicate-mode t)
(blink-cursor-mode -1)
(auto-save-mode -1)

(csetq frame-resize-pixelwise t)
(csetq window-combination-resize t)
(csetq frame-title-format
       '(:eval (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
                  "%b")))

;; Enable these functions
(mapc (lambda (x) (put x 'disabled nil))
      '(upcase-region
        downcase-region
        dired-find-alternate-file
        narrow-to-region))

;; Tabs, newlines and max line length
(csetq indent-tabs-mode nil)
(csetq tab-always-indent nil)
(csetq indicate-empty-lines t)
(csetq require-final-newline t)
(csetq fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Some special file names
(add-to-list 'auto-mode-alist '("\\.?bash.*" . shell-script-mode))

;; Setup the gui appearance
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(set-face-attribute 'default nil
                    :family "IBM Plex Mono"
                    :height 105
                    :weight 'normal)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 110
                    :weight 'normal)

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(size-indication-mode -1)
(line-number-mode t)
(column-number-mode t)
(csetq visible-cursor nil)

;; (csetq display-buffer-alist
;;        `((,(rx bos
;;                (or "*Compile-Log*"
;;                    "*Warnings*"
;;                    "*compilation"
;;                    "*rg*"
;;                    "*grep*"
;;                    "*ag search*"
;;                    "*Occur*"
;;                    "*xref*"
;;                    "*Flymake diagnostics"
;;                    "*Flycheck"
;;                    "*ivy-"
;;                    "*hgrep*"
;;                    ))
;;           (display-buffer-reuse-window
;;            display-buffer-in-side-window)
;;           (side            . bottom)
;;           (reusable-frames . nil)
;;           (window-height   . 0.25))

;;          ;; Let `display-buffer' reuse visible frames for all buffers. This must be
;;          ;; the last entry in `display-buffer-alist', because it overrides any later
;;          ;; entry with more specific actions.
;;          ("." nil (reusable-frames . nil))
;;          ))

;;; Global functions
(defun user/results-buffer-hook ()
  "Set various settings on results buffers (compilation, grep, etc.)."
  (setq-local scroll-margin 0)
  (setq-local show-trailing-whitespace nil))

;;; Keybindings
(when (display-graphic-p)
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-\M-m] [C-M-m]))

(bind-key "C-c t d" #'toggle-debug-on-error)
(bind-key "C-c t q" #'toggle-debug-on-quit)

(unbind-key "C-z")
(unbind-key "C-x C-z")
(unbind-key "C-x f")
(unbind-key "C-x m")
(unbind-key "M-o")
(unbind-key "C-x >")
(unbind-key "C-x <")
(unbind-key "<C-next>")
(unbind-key "<C-prior>")

;;; Packages

(use-package abbrev
  :ensure nil
  :hook (find-file . user/disable-abbrev-mode)
  :preface
  (defun user/disable-abbrev-mode ()
    (abbrev-mode -1)))

(use-package ag
  :defer t
  :init
  (csetq ag-reuse-buffers t)
  (csetq ag-reuse-window t)

  :config
  (dolist (ign-file grep-find-ignored-files)
    (add-to-list 'ag-ignore-list ign-file))

  (dolist (ign-dir grep-find-ignored-directories)
    (add-to-list 'ag-ignore-list ign-dir)))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :init
  (csetq auto-revert-verbose nil)
  (csetq auto-revert-avoid-polling t)

  :config
  (global-auto-revert-mode))

(use-package bookmark
  :ensure nil
  :defer t
  :init
  (csetq bookmark-save-flag 1))

(use-package bug-hunter :defer t)

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . user/c-mode-common-hook)
  :preface
  (defconst user/allman-style
    '((c-electric-pound-behavior     . (alignleft))
      (c-tab-always-indent           . nil)
      (c-comment-only-line-offset    . 0)
      (c-hanging-semi&comma-criteria . nil)
      (c-hanging-braces-alist        . ((class-open . (before after))
                                        (class-close . (before after))
                                        (defun-open . (before after))
                                        (defun-close . (before after))
                                        (inline-open . (before after))
                                        (inline-close . (before after))
                                        (block-open . (before after))
                                        (block-close . (before))
                                        (substatement-open . (before after))
                                        (statement-case-open . (before after))))
      (c-hanging-colons-alist        . ((case-label . (after))
                                        (label . (after))
                                        (access-label . (after))
                                        (access-key . (after))
                                        (member-init-intro . (after))))
      (c-cleanup-list                . (scope-operator
                                        list-close-comma
                                        defun-close-semi
                                        compact-empty-funcall))
      (c-offsets-alist               . ((arglist-close           .  4)
                                        (label                   . -4)
                                        (access-label            . -4)
                                        (substatement-open       .  0)
                                        (statement-case-intro    .  4)
                                        (statement-block-intro   .  4)
                                        (case-label              .  0)
                                        (block-open              .  0)
                                        (inline-open             .  0)
                                        (topmost-intro-cont      .  0)
                                        (knr-argdecl-intro       . -4)
                                        (brace-list-open         .  0)
                                        (brace-list-intro        .  4))))
    "A sane style, mostly based on Allman.")

  (defconst user/k&r-style
    '((c-basic-offset . 4)
      (c-comment-only-line-offset . 0)
      (c-offsets-alist . ((statement-block-intro . +)
                          (knr-argdecl-intro . 0)
                          (substatement-open . 0)
                          (substatement-label . 0)
                          (label . 0)
                          (statement-cont . +)))))

  (defun user/cc-goto-def ()
    "Go to the first occurence of the variable/parameter inside the function.  \
For anything else there is ctags."
    (interactive)

    (let ((cur-word (thing-at-point 'symbol))
          (bound (point))
          (found-point nil)
          (case-fold-search nil))
      (when cur-word
        (save-mark-and-excursion
          (c-beginning-of-defun)
          (if (re-search-forward (concat "\\<" cur-word "\\>") bound t)
              (setq found-point (point)))))

      (if found-point
          (progn
            (push-mark (point) t)
            (goto-char found-point)
            (backward-word)))))

  (defun user/c-mode-common-hook ()
    "Hook for C/C++ mode."
    (c-toggle-electric-state t)
    (c-toggle-syntactic-indentation t)

    (when lsp-mode
      (lsp-ui-doc-enable nil)
      (setq-local flymake-diagnostic-functions (remove #'flymake-cc flymake-diagnostic-functions))))

  :config
  (c-add-style "allman" user/allman-style)
  (c-add-style "sane-k&r" user/k&r-style)

  (csetq c-default-style '((java-mode . "java")
                           (awk-mode . "awk")
                           (other . "sane-k&r"))))

(use-package ccls
  :after cc-mode
  :bind (:map c-mode-base-map
              ("M-o" . #'user/ccls-show/body))
  :preface
  (defun user/ccls-callee-hierarchy ()
    (interactive)
    (ccls-call-hierarchy t))

  (defhydra user/ccls-show (:exit t)
    ("i" ccls-inheritance-hierarchy "base inheritance")
    ("I" ccls-inheritance-hierarchy "derived inheritance")
    ("c" ccls-call-hierarchy "callers")
    ("C" user/ccls-callee-hierarchy "callee")
    ("m" ccls-member-hierarchy "members")
    ("." lsp-ui-peek-find-references "references")
    ("s" lsp-ui-peek-find-workspace-symbol "symbol"))

  :init
  (csetq ccls-initialization-options '(:diagnostics (:onOpen 0 :opSave 0 :onChange -1 :spellChecking :json-false))))

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'")

(use-package cmake-font-lock
  :after cmake-mode
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package comment-dwim-2
  :after selected
  :bind ("M-;" . #'comment-dwim-2))

(use-package company
  :diminish
  :bind (("C-j" . #'company-complete)
         :map company-active-map
         ("ESC" . #'company-abort)
         ("C-l" . #'company-show-location)
         ("C-n" . #'company-select-next)
         ("C-p" . #'company-select-previous)
         ("C-w" . nil))
  :hook (after-init . global-company-mode)
  :init
  (csetq company-dabbrev-code-ignore-case t)
  (csetq company-dabbrev-downcase nil)
  (csetq company-dabbrev-ignore-case t)
  (csetq company-idle-delay 0)
  (csetq company-minimum-prefix-length 4)
  (csetq company-require-match nil)
  (csetq company-selection-wrap-around t)
  (csetq company-tooltip-align-annotations t)
  (csetq company-tooltip-flip-when-above t)
  (csetq company-transformers '(company-sort-by-occurrence))

  ;; (use-package user-completion
  ;;   :load-path "lisp"
  ;;   :bind (("C-c /" . #'user/complete-line)))
  )

(use-package compile
  :ensure nil
  :diminish compilation-in-progress
  :bind (([remap comment-region] . #'user/compile-without-ask)
         ("C-c c" . #'compile))
  :preface
  (defun user/compile-without-ask ()
    (interactive)
    (let ((compilation-read-command nil))
      (if projectile-mode
          (projectile-compile-project nil)
        (compile compile-command))))

  (defun user/switch-to-compilation-window (buffer _msg)
    (select-window (get-buffer-window buffer)))

  (defvar user/compile-process nil
    "The current compilation process or nil if none.")

  (defun user/bury-buffer (window buffer)
    "Bury the BUFFER and switch to the previous buffer in WINDOW, ignoring
errors.

If the previous buffer cannot be found for the WINDOW, then it will simply
delete the WINDOW."
    (ignore-errors
      (unless (switch-to-prev-buffer window)
        (delete-window window))
      (bury-buffer buffer)))

  (defun user/compile-start (proc)
    (when (string-equal (buffer-name (current-buffer)) "*compilation*")
      (setq user/compile-process proc)))

  (defun user/compile-done (buffer _msg)
    (when (string-equal "*compilation*" (buffer-name buffer))
      (let* ((exit-status (process-exit-status user/compile-process))
             (has-errors  (/= 0 exit-status))
             (window (get-buffer-window buffer)))

        (when (and window (not has-errors))
          (run-at-time "1 sec" nil #'user/bury-buffer window buffer)))

      (setq user/compile-process nil)))

  (require 'ansi-color)

  (defun user/colorize-compilation-buffer ()
    "Colorize a compilation mode buffer.
Taken from http://stackoverflow.com/a/3072831/355252."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  :hook (;; (compilation-start . user/compile-start)
         ;; (compilation-filter . user/colorize-compilation-buffer)
         (compilation-mode . user/results-buffer-hook))

  :init
  (csetq compilation-always-kill t)
  (csetq compilation-ask-about-save nil)
  (csetq compilation-auto-jump-to-first-error nil)
  (csetq compilation-context-lines nil)
  (csetq compilation-disable-input t)
  (csetq compilation-scroll-output 'first-error)

  ;; :config
  (add-hook 'compilation-finish-functions #'user/switch-to-compilation-window))

(use-package counsel
  :after ivy
  :diminish
  :init
  (csetq counsel-describe-function-preselect 'ivy-function-called-at-point)
  (csetq counsel-grep-post-action-hook '(recenter))
  :config
  (counsel-mode t))

(use-package counsel-etags
  :after counsel)

(use-package comint
  :ensure nil
  :bind (:map comint-mode-map
              ("<down>" . #'comint-next-input)
              ("<up>"   . #'comint-previous-input)
              ("C-n"    . #'comint-next-input)
              ("C-p"    . #'comint-previous-input)
              ("C-r"    . #'comint-history-isearch-backward))
  :init
  (csetq comint-process-echoes t)
  (csetq comint-prompt-read-only t)
  (csetq comint-history-isearch t)
  (csetq comint-ignore-dups t)

  :config
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(use-package cus-edit
  :ensure nil
  :init
  (csetq custom-buffer-done-kill t)
  (csetq custom-buffer-verbose-help nil)
  (csetq custom-file user/custom-file)
  (csetq custom-unlispify-tag-names nil)
  (csetq custom-unlispify-menu-entries nil))

(use-package cython-mode :defer t)

(use-package dabbrev
  :ensure nil
  :init
  (csetq dabbrev-case-replace nil)
  (csetq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*$]"))

(use-package daemons
  :commands (daemons daemons-start daemons-stop daemons-status)
  :init
  (csetq daemons-always-sudo t))

(use-package diff-mode
  :ensure nil
  :hook ((diff-mode . diff-delete-empty-files)
         (diff-mode . diff-make-unified)
         (diff-mode . smerge-mode))
  :init
  (csetq diff-switches '("-u" "-p" "-w")))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("SPC" . #'dired-mark)
              ("<C-return>" . #'user/open-in-external-app)
              ("<tab>" . #'user/dired-next-window))
  :preface
  (defun user/dired-next-window ()
    (interactive)
    (let ((next (car (cl-remove-if-not #'(lambda (wind)
                                           (with-current-buffer (window-buffer wind)
                                             (eq major-mode 'dired-mode)))
                                       (cdr (window-list))))))
      (when next
        (select-window next))))

  (defun user/open-in-external-app ()
    "Open the file(s) at point with an external application."
    (interactive)
    (let* ((file-list (dired-get-marked-files)))
      (mapc
       (lambda (file-path)
         (let ((process-connection-type nil))
           (start-process "" nil "xdg-open" file-path)))
       file-list)))

  :init
  (csetq dired-auto-revert-buffer t)
  (csetq dired-dwim-target t)
  (csetq dired-hide-details-hide-information-lines nil)
  (csetq dired-hide-details-hide-symlink-targets nil)
  (csetq dired-listing-switches "-lFaGh1v --group-directories-first")
  (csetq dired-ls-F-marks-symlinks t)
  (csetq dired-recursive-copies 'always))

(use-package dired-du
  :after dired
  :config
  (csetq dired-du-size-format t)
  (csetq dired-du-update-headers t))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . #'dired-narrow)))

(use-package dired-toggle
  :bind ("<f5>" . #'dired-toggle)
  :preface
  (defun user/dired-toggle-mode-hook ()
    (dired-hide-details-mode t)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))

  :hook (dired-toggle-mode . user/dired-toggle-mode-hook)
  :init
  (csetq dired-toggle-window-size 40))

(use-package dired-x
  :ensure nil
  :after dired
  :init
  (csetq dired-omit-verbose nil))

(use-package disk-usage
  :commands (disk-usage))

(use-package docker
  :commands (docker))

(use-package dumb-jump
  :hook ((dumb-jump-after-jump . recenter-top-bottom)))

(use-package eacl
  :bind (("C-x C-l" . #'eacl-complete-line)))

(use-package easy-kill
  :bind (([remap kill-ring-save] . #'easy-kill)
         ([remap mark-sexp] . #'easy-mark)))

(use-package ediff
  :ensure nil
  :defer t
  :preface
  (defun user/command-line-diff (_switch)
    "Add the -diff <file1> <file2> argument to Emacs.
_SWITCH should be 'diff'."
    (if (> 2 (length command-line-args-left))
        (error "The -diff requires 2 files")
      (let ((file1 (pop command-line-args-left))
            (file2 (pop command-line-args-left)))
        (ediff file1 file2))))

  (add-to-list 'command-switch-alist '("diff"  . user/command-line-diff))

  :init
  (csetq ediff-diff-options "-w")
  (csetq ediff-highlight-all-diffs nil)
  (csetq ediff-show-clashes-only t)
  (csetq ediff-split-window-function #'split-window-horizontally)
  (csetq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package eldoc-box
  :disabled
  :diminish eldoc-box-hover-mode
  :hook ((prog-mode . eldoc-box-hover-at-point-mode)
         (prog-mode . eldoc-box-hover-mode)))

(use-package eshell
  :ensure nil
  :defer t
  :hook ((eshell-mode . user/eshell-mode-hook)
         (eshell-first-time-mode . user/eshell-first-time-mode-hook))
  :preface
  (defun user/eshell-mode-hook ()
    (company-mode -1)
    (setq-local scroll-margin 0))

  (defun user/eshell-first-time-mode-hook ()
    (add-to-list 'eshell-modules-list 'eshell-rebind)
    (add-to-list 'eshell-modules-list 'eshell-smart)
    (add-to-list 'eshell-modules-list 'eshell-xtra))

  :init
  (csetq eshell-hist-ignoredups t)
  (csetq eshell-history-size 50000)
  (csetq eshell-ls-dired-initial-args (quote ("-h")))
  (csetq eshell-ls-exclude-regexp "~\\'")
  (csetq eshell-ls-initial-args "-hA")
  (csetq eshell-stringify-t nil))

(use-package esh-module
  :ensure nil
  :after eshell
  :config)

(use-package expand-region
  :bind (("M-2" . #'er/expand-region)
         ("M-1" . #'er/contract-region)
         ("M-@" . #'er/contract-region))
  :preface
  :init
  (csetq expand-region-fast-keys-enabled nil)
  (csetq expand-region-autocopy-register "e"))

(use-package eyebrowse
  :bind ("C-c e" . #'user/eyebrowse-hydra/body)
  :preface
  (defhydra user/eyebrowse-hydra (:color pink)
    "
^
^Eyebrowse^         ^Do^                ^Switch^
^─────────^─────────^──^────────────────^──────^────────────
_q_ quit            _c_ create          _p_ previous
^^                  _k_ kill            _n_ next
^^                  _r_ rename          _e_ last
^^                  ^^                  _s_ switch
^^                  ^^                  ^^
"
    ("q" nil)
    ("p" eyebrowse-prev-window-config :color red)
    ("n" eyebrowse-next-window-config :color red)
    ("c" eyebrowse-create-window-config)
    ("e" eyebrowse-last-window-config)
    ("k" eyebrowse-close-window-config :color red)
    ("r" eyebrowse-rename-window-config)
    ("s" eyebrowse-switch-to-window-config))
  :init
  (csetq eyebrowse-new-workspace t)
  (csetq eyebrowse-switch-back-and-forth t)
  (csetq eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode t))

(use-package flymake
  :ensure nil
  :preface
  (defun flymake-display-at-point ()
    "Display the flymake diagnostic text for the thing at point."
    (interactive)
    (when (and flymake-mode
               (get-char-property (point) 'flymake-diagnostic))
      (let ((text (flymake--diag-text (get-char-property (point) 'flymake-diagnostic))))
        (when text (message "%s" text)))))

  :bind (("C-c f n" . #'flymake-goto-next-error)
         ("C-c f p" . #'flymake-goto-prev-error)
         ("C-c f s" . #'flymake-start)
         ("C-c f f" . #'flymake-display-at-point))
  :init
  (csetq flymake-no-changes-timeout nil)
  (csetq flymake-start-syntax-check-on-newline nil)
  ;; :config
  ;; (advice-add #'flymake-make-diagnostic :around #'user/flymake-make-diagnostic)
  )

(use-package flymake-diagnostic-at-point
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :init
  (csetq flymake-diagnostic-at-point-display-diagnostic-function
         #'flymake-diagnostic-at-point-display-popup))

(use-package flyspell
  :ensure nil
  :diminish
  :init
  (csetq flyspell-issue-message-flag nil))

;; TBD
;; (use-package frog-menu)

(use-package grep
  :ensure nil
  :defer t
  :hook (grep-mode . user/results-buffer-hook))

(use-package gnus
  :ensure nil
  :preface
  (defhydra user/hydra-gnus-group (:color blue)
    "
[_A_] Remote groups (A A) [_g_] Refresh
[_L_] Local groups        [_\\^_] List servers
[_c_] Mark all read       [_m_] Compose new mail
[_G_] Search mails (G G) [_#_] Mark mail
"
    ("A" gnus-group-list-active)
    ("L" gnus-group-list-all-groups)
    ("c" gnus-topic-catchup-articles)
    ("G" dianyou-group-make-nnir-group)
    ("g" gnus-group-get-new-news)
    ("^" gnus-group-enter-server-mode)
    ("m" gnus-group-new-mail)
    ("#" gnus-topic-mark-topic)
    ("q" nil))

  (defhydra user/hydra-gnus-summary (:color blue)
    "
[_s_] Show thread   [_F_] Forward (C-c C-f)
[_h_] Hide thread   [_e_] Resend (S D e)
[_n_] Refresh (/ N) [_r_] Reply
[_!_] Mail -> disk  [_R_] Reply with original
[_d_] Disk -> mail  [_w_] Reply all (S w)
[_c_] Read all      [_W_] Reply all with original (S W)
[_#_] Mark
"
    ("s" gnus-summary-show-thread)
    ("h" gnus-summary-hide-thread)
    ("n" gnus-summary-insert-new-articles)
    ("F" gnus-summary-mail-forward)
    ("!" gnus-summary-tick-article-forward)
    ("d" gnus-summary-put-mark-as-read-next)
    ("c" gnus-summary-catchup-and-exit)
    ("e" gnus-summary-resend-message-edit)
    ("R" gnus-summary-reply-with-original)
    ("r" gnus-summary-reply)
    ("W" gnus-summary-wide-reply-with-original)
    ("w" gnus-summary-wide-reply)
    ("#" gnus-topic-mark-topic)
    ("q" nil))

  (defhydra user/hydra-gnus-article (:color blue)
    "
[_o_] Save attachment        [_F_] Forward
[_v_] Play video/audio       [_r_] Reply
[_d_] CLI to dowloand stream [_R_] Reply with original
[_b_] Open external browser  [_w_] Reply all (S w)
[_f_] Click link/button      [_W_] Reply all with original (S W)
[_g_] Focus link/button
"
    ("F" gnus-summary-mail-forward)
    ("r" gnus-article-reply)
    ("R" gnus-article-reply-with-original)
    ("w" gnus-article-wide-reply)
    ("W" gnus-article-wide-reply-with-original)
    ("o" gnus-mime-save-part)
    ("v" w3mext-open-with-mplayer)
    ("d" w3mext-download-rss-stream)
    ("b" w3mext-open-link-or-image-or-url)
    ("f" w3m-lnum-follow)
    ("g" w3m-lnum-goto)
    ("q" nil))

  :bind (;; ("C-x m" . #'gnus)
         :map gnus-group-mode-map
         ("y" . #'user/hydra-gnus-group/body)
         ("o" . #'gnus-group-list-all-groups)
         :map gnus-summary-mode-map
         ("y" . #'user/hydra-gnus-summary/body)
         (">" . #'gnus-summary-show-thread)
         ("<" . #'gnus-summary-hide-thread)
         :map gnus-article-mode-map
         ("y" . #'user/hydra-gnus-article/body))

  :init
  (csetq gnus-init-file (expand-file-name "gnus.el" user-emacs-directory)))

(use-package gud
  :ensure nil
  :defer t
  :hook (gud-mode . user/gud-mode-hook)
  :preface
  (defun user/gud-mode-hook ()
    "Hook to run when GUD mode is activated."
    (company-mode -1))
  :init
  (csetq gdb-many-windows t))

(use-package hexl
  :if (< emacs-major-version 27)
  :ensure nil
  :init
  (csetq hexl-bits 8))

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . #'ibuffer))
  :init
  (csetq ibuffer-saved-filter-groups
         '(("default"

            ("Interactive" (or (mode . lisp-interaction-mode)
                               (name . "\*Messages\*")
                               (name . "\*compilation\*")
                               (name . "\*Customize\*")
                               (name . "\*ag search\*")
                               (name . "\*grep\*")))

            ("Dired" (mode . dired-mode))

            ;; Need to be before "Programming" otherwise
            ;; `emacs-lisp-mode' will match.
            ("Emacs config" (filename . ".emacs.d"))

            ("Org-Mode" (mode . org-mode))

            ("Programming" (or (mode . c-mode)
                               (mode . c++-mode)
                               (mode . makefile-mode)
                               (mode . cmake-mode)
                               (mode . ruby-mode)
                               (mode . perl-mode)
                               (mode . python-mode)
                               (mode . js-mode)
                               (mode . js2-mode)
                               (mode . css-mode)
                               (mode . web-mode)
                               (mode . emacs-lisp-mode)))

            ("Magit" (name . "\*magit"))

            ("Help" (or (name . "\*Help\*")
                        (name . "\*Apropos\*")
                        (name . "\*info\*"))))))

  (defalias 'list-buffers 'ibuffer)

  (csetq ibuffer-default-shrink-to-minimum-size t)
  (csetq ibuffer-show-empty-filter-groups nil)

  :config
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)      ;auto update
               (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package imenu
  :ensure nil
  :bind ("M-i" . #'imenu)
  :hook (imenu-after-jump . recenter-top-bottom)
  :init
  (csetq imenu-auto-rescan t)
  (csetq imenu-auto-rescan-maxout (* 1024 1024)))

(use-package imenu-anywhere
  :bind (("M-I" . #'imenu-anywhere)))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ("C-o" . #'isearch-occur)
         ("<tab>" . #'isearch-repeat-forward)
         ("<backtab>" . #'isearch-repeat-backward))
  :init
  (csetq isearch-lazy-count t)
  (csetq isearch-allow-scroll 'unlimited)
  (csetq isearch-yank-on-move 'shift))

(use-package ivy
  :demand t
  :diminish
  :bind (("C-c C-r" . #'ivy-resume)
         ("C-c v s" . #'ivy-push-view)
         ("C-c v p" . #'ivy-pop-view)
         :map ivy-mode-map
         ([escape] . #'user/minibuffer-keyboard-quit))
  :init
  (csetq ivy-count-format "(%d/%d) ")
  (csetq ivy-height 9)
  ;; (csetq ivy-on-del-error-function nil)
  ;; (csetq ivy-use-selectable-prompt t)
  (csetq ivy-use-virtual-buffers t)
  (csetq ivy-virtual-abbreviate 'full)
  (csetq ivy-wrap t)

  :config
  (ivy-mode t))

(use-package ivy-posframe
  :diminish
  :disabled
  :commands (ivy-posframe-mode)
  :preface
  (defun user/enable-posframe-maybe (&rest _frame)
    (when (and (display-graphic-p)
               (not ivy-posframe-mode))
      (ivy-posframe-mode)))
  :init
  (csetq ivy-height 15)
  (csetq ivy-display-function #'ivy-posframe-display-at-point)
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))

  (add-hook 'after-make-frame-functions #'user/enable-posframe-maybe)
  (add-hook 'after-init-hook #'user/enable-posframe-maybe)

  :config
  (ivy-posframe-mode t))

(use-package ivy-rich
  :after ivy
  :init
  (csetq ivy-rich-switch-buffer-align-virtual-buffer t)
  (csetq ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode t))

(use-package iy-go-to-char
  :bind (("M-m" . #'iy-go-to-char)
         ("M-M" . #'iy-go-to-char-backward)
         ("C-c ;" . #'iy-go-to-or-up-to-continue)
         ("C-c ," . #'iy-go-to-or-up-to-continue-backward)))

(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :init
  (csetq js2-skip-preprocessor-directives t))

(use-package json-mode
  :bind (:map json-mode-map
              ("M-q" . #'json-reformat-region))
  :init
  (csetq json-reformat:indent-width 4)
  (csetq json-reformat:pretty-string? t))

(use-package key-chord
  :init
  (csetq key-chord-two-keys-delay 0.1)
  (csetq key-chord-one-key-delay 0.2)

  :config
  (key-chord-mode t))

(use-package key-seq
  :after key-chord
  :config
  (key-seq-define-global "jv" eyebrowse-mode-map)
  (key-seq-define-global "jc" #'compile)
  ;; (key-seq-define-global "jj" #'helm-mini)
  ;; (key-seq-define-global "jf" #'helm-find-files))
  (key-seq-define-global "jj" #'ivy-switch-buffer)
  (key-seq-define-global "jf" #'counsel-find-file)
  (key-seq-define-global "jx" #'counsel-M-x))

(use-package lsp-mode
  :commands lsp
  :hook ((c-mode-common . lsp)
         (python-mode . #'lsp))
  :init
  ;; performance reasons
  (csetq lsp-enable-on-type-formatting nil)
  (csetq lsp-enable-indentation nil)
  (csetq lsp-before-save-edits nil)

  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  (csetq lsp-auto-guess-root t)

  :config
  (require 'lsp-clients)

  ;; Until I have a method of selecting the prefered lsp-client (per project or globally)
  (remhash 'clangd lsp-clients)

  (use-package company-lsp
    :commands company-lsp
    :init
    (csetq company-lsp-async t)
    (csetq company-lsp-cache-candidates nil))

  (use-package lsp-ui
    :commands lsp-ui-mode
    :init
    (csetq lsp-ui-doc-enable t)
    (csetq lsp-ui-doc-include-signature t)

    (csetq lsp-ui-peek-always-show t)

    (csetq lsp-ui-sideline-enable nil)
    (csetq lsp-ui-sideline-show-hover nil)
    (csetq lsp-ui-sideline-show-symbol nil)
    (csetq lsp-ui-sideline-ignore-duplicate t)

    (csetq lsp-eldoc-hook (delete #'lsp-document-highlight lsp-eldoc-hook))))

(use-package magit
  :bind (("C-x g" . #'magit-status))
  :hook (git-commit-mode . git-commit-turn-on-flyspell)
  :init
  (csetq magit-diff-arguments
         '("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat" "--diff-algorithm=histogram"))
  (csetq magit-diff-refine-hunk t)
  (csetq magit-display-buffer-function
         #'magit-display-buffer-fullframe-status-v1)
  (csetq magit-process-popup-time 20)
  (csetq magit-refs-show-commit-count 'all))

(use-package magit-gitflow
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package midnight
  :ensure nil
  :init
  ;; 5 minutes for special buffers
  (csetq clean-buffer-list-delay-special (* 5 60))
  (csetq midnight-period (* 15 60))

  :config
  (midnight-delay-set 'midnight-delay 1)

  (add-to-list 'clean-buffer-list-kill-buffer-names
               '("*buffer-selection*"
                 "*Finder*"
                 "*Occur*"
                 "*rg*"
                 "*ag search*"
                 "*compilation*"
                 "*Help*"
                 "*Ido Completions*"
                 "*Finder Category*"
                 "*Finder-package*"
                 "*RE-Builder*"
                 "*vc-change-log*"))

  (add-to-list 'clean-buffer-list-kill-regexps
               '("\\`\\*Customize .*\\*\\'"
                 "\\`\\*magit: .*\\*\\'"
                 "\\`\\*magit-.*\\*\\'"
                 "\\`\\*Outline .*\\*\\'"
                 "\\`\\*.* annots\\*\\'"
                 "\\`\\*Contents*\\*\\'"
                 "\\`\\*ivy-occur.*\\*\\'"
                 "\\`\\*Ido.*\\*\\'"
                 "\\`\\*\\(Wo\\)?Man .*\\*\\'"))

  (midnight-mode t))

(use-package minions
  :config
  (minions-mode t))

(use-package misc
  :ensure nil
  :bind (("M-z" . #'zap-up-to-char)
         ("<C-right>" . #'forward-to-word)))

(use-package mouse-copy
  :ensure nil)

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :bind (("C-x m" . mu4e))
  :init
  (csetq message-kill-buffer-on-exit t)

  (csetq mu4e-maildir (expand-file-name "~/.mail"))
  (csetq mu4e-headers-auto-update t)
  (csetq mu4e-view-show-images t)
  (csetq mu4e-view-show-addresses t)
  (csetq mu4e-update-interval 120)
  (csetq mu4e-confirm-quit nil)
  (csetq mu4e-attachment-dir (expand-file-name "~/Downloads"))
  (csetq mu4e-change-filenames-when-moving t)
  (csetq mu4e-context-policy 'pick-first)
  (csetq mu4e-display-update-status-in-modeline t)
  (csetq mu4e-use-fancy-chars t)
  (csetq mu4e-save-multiple-attachments-without-asking t)
  (csetq mu4e-compose-dont-reply-to-self t)
  (csetq mu4e-headers-include-related t)
  (csetq mu4e-completing-read-function 'completing-read)

  (csetq mu4e-headers-unread-mark '("u" . "✉"))
  ;; TODO mu4e-view-attachment-assoc

  :config
  (load-file (expand-file-name "mu4e.el" user-emacs-directory)))

(use-package multi-term
  :if (eq system-type 'gnu/linux)
  :bind (("C-z" . #'multi-term-next)
         ("C-c z c" . #'multi-term)
         ("C-c z d" . #'multi-term-dedicated-toggle)
         ("C-c z n" . #'multi-term-next)
         ("C-c z p" . #'multi-term-prev))
  :hook (term-mode . user/term-mode-hook)
  :preface
  (defun user/term-mode-hook ()
    (company-mode -1)
    (setq-local scroll-margin 0))

  :init
  ;; (csetq multi-term-program "screen")
  ;; (csetq multi-term-program-switches "-DR")
  (csetq multi-term-dedicated-select-after-open-p t)
  (csetq multi-term-scroll-show-maximum-output t))

(use-package multiple-cursors
  :bind (("C->" . #'mc/unmark-next-like-this)
         ("C-<" . #'mc/unmark-previous-like-this)
         ("C-." . #'mc/mark-next-like-this)
         ("C-," . #'mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . #'mc/add-cursor-on-click)
         ("C-c m ^"     . #'mc/edit-beginnings-of-lines)
         ("C-c m `"     . #'mc/edit-beginnings-of-lines)
         ("C-c m $"     . #'mc/edit-ends-of-lines)
         ("C-c m '"     . #'mc/edit-ends-of-lines)
         ("C-c m R"     . #'mc/reverse-regions)
         ("C-c m S"     . #'mc/sort-regions)
         ("C-c m W"     . #'mc/mark-all-words-like-this)
         ("C-c m Y"     . #'mc/mark-all-symbols-like-this)
         ("C-c m a"     . #'mc/mark-all-like-this-dwim)
         ("C-c m c"     . #'mc/mark-all-dwim)
         ("C-c m l"     . #'mc/insert-letters)
         ("C-c m n"     . #'mc/insert-numbers)
         ("C-c m r"     . #'mc/mark-all-in-region)
         ("C-c m s"     . #'set-rectangular-region-anchor)
         ("C-c m %"     . #'mc/mark-all-in-region-regexp)
         ("C-c m t"     . #'mc/mark-sgml-tag-pair)
         ("C-c m w"     . #'mc/mark-next-like-this-word)
         ("C-c m x"     . #'mc/mark-more-like-this-extended)
         ("C-c m y"     . #'mc/mark-next-like-this-symbol)
         ("C-c m C-SPC" . #'mc/mark-pop)

         ("C-c m ("     . #'mc/mark-all-symbols-like-this-in-defun)
         ("C-c m C-("   . #'mc/mark-all-words-like-this-in-defun)
         ("C-c m M-("   . #'mc/mark-all-like-this-in-defun)
         ("C-c m d"     . #'mc/mark-all-symbols-like-this-in-defun)
         ("C-c m C-d"   . #'mc/mark-all-words-like-this-in-defun)
         ("C-c m M-d"   . #'mc/mark-all-like-this-in-defun)

         ("C-c m ["     . #'mc/vertical-align-with-space)
         ("C-c m {"     . #'mc/vertical-align))
  :preface
  (defun mc-prompt-once-advice (fn &rest args)
    (setq mc--this-command (lambda () (interactive) (apply fn args)))
    (apply fn args))

  (defun mc-prompt-once (&rest fns)
    (dolist (fn fns)
      (advice-add fn :around #'mc-prompt-once-advice)))

  :config
  (mc-prompt-once #'zap-up-to-char #'sp-rewrap-sexp))

(use-package nasm-mode
  :mode "\\.asm\\'")

(use-package objed
  :disabled
  :config
  (add-to-list 'objed-keeper-commands 'undo-tree-undo)
  (add-to-list 'objed-keeper-commands 'undo-tree-redo)

  (define-key objed-op-map "x" 'counsel-M-x)

  (objed-mode t))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (csetq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package prog-mode
  :ensure nil
  :preface
  (defun user/prog-mode-hook ()
    "Settings for all programming modes."
    (setq-local show-trailing-whitespace t))

  :hook (prog-mode . user/prog-mode-hook))

(use-package projectile
  :commands projectile-mode
  :diminish
  :bind (:map projectile-mode-map
              ("C-c p" . #'projectile-command-map))
  :hook (after-init . projectile-mode)
  :preface
  (defun user/projectile-invalidate-cache (&rest _args)
    (projectile-invalidate-cache nil))

  :init
  (csetq projectile-completion-system 'ivy)
  (csetq projectile-enable-caching t)
  (csetq projectile-indexing-method 'alien)
  (csetq projectile-sort-order 'recentf)
  (csetq projectile-use-git-grep t)

  :config
  (add-to-list 'projectile-globally-ignored-directories ".vscode")
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package python
  :ensure nil
  :defer t
  :init
  (when (executable-find "ipython")
    (csetq python-shell-interpreter "ipython")
    (csetq python-shell-interpreter-args
           "--colors=Linux --profile=default --simple-prompt")
    (csetq python-shell-prompt-output-regexp "Out \\[[0-9]+\\]: ")
    (csetq python-shell-prompt-input-regexp "In \\[[0-9]+\\]: ")
    (csetq python-shell-completion-setup-code
           "from IPython.core.completerlib import module_completion")
    (csetq python-shell-completion-string-code
           "';'.join(get_ipython().Completer.all_completions('''%s'''), module_completion('''%s'''))\n")))

(use-package pyvenv
  :hook (python-mode . user/auto-virtualenv)
  :preface
  (defun user/auto-virtualenv ()
    (pyvenv-mode t)

    ;; A dolist would be appropriate, but I only use venv as virtualenv name
    ;; This also works with lsp-mode since it will use the python inside
    (let ((root (locate-dominating-file default-directory "venv")))
      (if (and root (file-exists-p root))
          (pyvenv-activate (expand-file-name "venv" root))))))

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package realgud
  :commands (realgud:bashdb realgud:gdb realgud:gub realgud:ipdb
                            realgud:jdb realgud:kshdb realgud:nodejs realgud:pdb
                            realgud:perldb realgud:zshdb))

(use-package recentf
  :ensure nil
  :init
  (csetq recentf-auto-cleanup 'never)
  (csetq recentf-exclude (list
                          "/usr/share/emacs/.*\\'"
                          "/elpa/.*\\'"          ; Package files
                          "PKGBUILD"             ; ArchLinux aur
                          "crontab.*"
                          "/tmp/.*\\'"
                          #'ignoramus-boring-p))
  (csetq recentf-max-saved-items 500)
  (csetq recentf-max-menu-items 100)

  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (recentf-mode t)
  (run-at-time (* 5 60) (* 5 60) #'recentf-save-list))

(use-package rg
  :demand t
  :init
  (csetq rg-ignore-case 'smart)
  (csetq rg-hide-command nil)
  :config
  (rg-enable-default-bindings))

(use-package rmsbolt
  :defer t)

(use-package savehist
  :ensure nil
  :init
  (csetq savehist-additional-variables
         '(search-ring
           regexp-search-ring
           compile-command))
  (csetq savehist-autosave-interval 60)
  (csetq history-length 1000)

  :config
  (savehist-mode t))

(use-package saveplace
  :ensure nil
  :if (>= emacs-major-version 25)
  :config
  (save-place-mode t))

(use-package selected
  :disabled
  :demand t
  :diminish selected-minor-mode
  :config
  (selected-global-mode t))

(use-package sh-script
  :ensure nil
  :init
  (csetq sh-basic-offset 2))

(use-package simple
  :ensure nil
  :bind (("M-u" . #'upcase-dwim)
         ("M-l" . #'downcase-dwim)
         ("M-c" . #'capitalize-dwim)
         ("M-g" . #'goto-line)
         ("C-8" . #'repeat-complex-command)
         ([remap just-one-space] . #'cycle-spacing)
         ([remap newline] . #'newline-and-indent)))

(use-package smartparens
  :demand t
  :diminish
  :bind (("C-M-k" . #'sp-kill-sexp)

         ("C-M-n" . #'sp-next-sexp)
         ("C-M-p" . #'sp-previous-sexp)

         ("C-M-f" . #'sp-forward-sexp)
         ("C-M-b" . #'sp-backward-sexp)

         ("C-M-u" . #'sp-backward-up-sexp)
         ("C-M-d" . #'sp-down-sexp)

         ("C-(" . #'sp-wrap-round)
         ("C-{" . #'sp-wrap-curly)

         ("C-M-<right>"   . #'sp-forward-slurp-sexp)
         ("C-M-<left>"    . #'sp-forward-barf-sexp)
         )

  :preface
  (defun user/open-block-c-mode (_id action _context)
    (case action
      ((insert) (let (should-indent)
                  (save-excursion
                    (goto-char (line-beginning-position))
                    ;; if|when|for (expr) or just whitespace
                    (setq should-indent (looking-at-p "\\s-+\\(\\(if\\|when\\|for\\).*\\)?{}$")))
                  (when should-indent
                    (indent-according-to-mode)
                    (newline)
                    (newline)
                    (indent-according-to-mode)
                    (forward-line -1)
                    (indent-according-to-mode))))

      ((wrap) (progn
                (let* ((c (char-equal (char-before) ?{))
                       (rb (if c (region-beginning) (1+ (region-beginning))))
                       (re (if c (region-end) (1- (region-end))))
                       (buf (buffer-substring rb re))
                       (ret-line))
                  (delete-region rb re)
                  (when (not c)
                    (backward-char))
                  (newline)
                  (newline)
                  (forward-line -1)
                  (setq ret-line (line-number-at-pos))
                  (insert buf)
                  (sp-forward-sexp)
                  (sp-backward-sexp)
                  (sp-mark-sexp)
                  (indent-region (region-beginning) (region-end))
                  (goto-char (point-min))
                  (forward-line (1- ret-line)))))))

  :hook ((prog-mode . smartparens-mode)
         (minibuffer-setup . turn-on-smartparens-strict-mode))

  :init
  (csetq sp-highlight-pair-overlay nil)
  (csetq sp-highlight-wrap-overlay nil)
  (csetq sp-highlight-wrap-tag-overlay nil)
  (csetq sp-show-pair-from-inside t)
  (csetq sp-cancel-autoskip-on-backward-movement nil)
  (csetq sp-max-pair-length 4)
  (csetq sp-max-prefix-length 50)
  (csetq sp-escape-quotes-after-insert nil)

  :config
  (require 'smartparens-config)

  (add-to-list 'sp--special-self-insert-commands #'c-electric-paren)
  (add-to-list 'sp--special-self-insert-commands #'c-electric-brace)

  (sp-local-pair 'c-mode "{" nil :post-handlers '(:add user/open-block-c-mode))
  (sp-local-pair 'c++-mode "{" nil :post-handlers '(:add user/open-block-c-mode))

  (sp-local-pair 'c-mode "'" nil :actions nil)
  (sp-local-pair 'c++-mode "'" nil :actions nil)

  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

  (show-smartparens-global-mode t))

(use-package smerge-mode
  :ensure nil
  :bind (("C-c <C-m>" . #'user/hydra-smerge/body))
  :preface
  (defhydra user/hydra-smerge
    (:color pink
     :hint nil
     :pre (smerge-mode 1)
     ;; Disable `smerge-mode' when quitting hydra if
     ;; no merge conflicts remain.
     :post (smerge-auto-leave))

    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue)))

(use-package smex
  :config
  (smex-initialize))

(use-package symbol-overlay
  :diminish
  :hook ((find-file . symbol-overlay-mode))
  :bind (("M-*" . #'symbol-overlay-put)
         ("M-n" . #'symbol-overlay-jump-next)
         ("M-p" . #'symbol-overlay-jump-prev)
         ("M-8" . #'symbol-overlay-toggle-in-scope))
  :init
  (csetq symbol-overlay-displayed-window t))

(use-package tramp
  :defer t
  :ensure nil
  :init
  (csetq tramp-default-method "ssh"))

(use-package undo-tree
  :diminish
  :init
  (csetq undo-tree-auto-save-history nil)
  (csetq undo-tree-visualizer-diff t)
  (csetq undo-tree-visualizer-timestamps t)

  :config
  (global-undo-tree-mode t)

  ;; Keep region when undoing in region
  (defadvice undo-tree-keep-region (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

(use-package uniquify
  :ensure nil
  :init
  (csetq uniquify-buffer-name-style 'post-forward)
  (csetq uniquify-separator ":")
  (csetq uniquify-ignore-buffers-re "^\\*")
  (csetq uniquify-after-kill-buffer-p t))

(use-package user-advices :load-path "lisp")

(use-package user-utils
  :load-path "lisp"
  :bind (("<C-return>" . #'user/open-line-above)
         ("C-a" . #'user/smarter-move-beginning-of-line)
         ("C-w" . #'user/smarter-kill-word-or-region)
         ([remap backward-kill-word] . #'user/smarter-backward-kill-word)
         ("M-]" . #'user/next-error)
         ("M-[" . #'user/prev-error)
         ;; ([remap forward-paragraph] . #'user/forward-paragraph)
         ;; ([remap backward-paragraph] . #'user/backward-paragraph)
         ("M-j" . #'user/join-line)
         ("C-`" . #'user/open-terminal)
         ([remap scroll-up-command] . #'user/scroll-half-page-up)
         ([remap scroll-down-command] . #'user/scroll-half-page-down)
         ("C-'" . #'push-mark-no-activate)
         ("M-'" . #'jump-to-mark)
         ;; :map isearch-mode-map
         ;; ("<backspace>" . #'user/isearch-delete)
         :map minibuffer-local-map
         ("<escape>" . #'user/minibuffer-keyboard-quit)
         :map minibuffer-local-ns-map
         ("<escape>" . #'user/minibuffer-keyboard-quit)
         :map minibuffer-local-completion-map
         ("<escape>" . #'user/minibuffer-keyboard-quit)
         :map minibuffer-local-must-match-map
         ("<escape>" . #'user/minibuffer-keyboard-quit)
         :map minibuffer-local-isearch-map
         ("<escape>" . #'user/minibuffer-keyboard-quit)))

(use-package user-window
  :load-path "lisp"
  :bind (("C-c w" . #'user/windows-hydra/body)
         ([remap exchange-point-and-mark] . #'exchange-point-and-mark-no-activate))
  :preface
  (defhydra user/windows-hydra (:color pink)
    "
^
^Movement^           ^Window^            ^Zoom^
^────────^───────────^──────^───────────^────^──────────────
_q_ quit             _b_ balance        _-_ out
_3_ split right      _i_ heighten       _+_ in
_2_ split below      _j_ narrow         _=_ reset
_1_ delete others    _k_ lower          ^^
_d_ dedicated        _l_ widen          ^^
_m_ minibuffer       _f_ fullscreen     ^^
_o_ other            ^^                 ^^
^^                   ^^                 ^^
"
    ("q" nil)
    ("b" balance-windows :color blue)
    ("d" user/toggle-current-window-dedication :color blue)
    ("f" toggle-frame-fullscreen :color blue)
    ("i" enlarge-window)
    ("j" shrink-window-horizontally)
    ("k" shrink-window)
    ("l" enlarge-window-horizontally)
    ("o" other-window)
    ("m" user/switch-to-minibuffer :color blue)
    ("1" delete-other-windows :color blue)
    ("2" split-window-right :color blue)
    ("3" split-window-below :color blue)
    ("-" text-scale-decrease)
    ("+" text-scale-increase)
    ("=" (text-scale-increase 0))))

(use-package vc
  :ensure nil
  :init
  (csetq vc-follow-symlinks t))

(use-package vc-git
  :ensure nil
  :init
  (csetq vc-git-diff-switches '("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat" "--diff-algorithm=histogram"))
  (csetq vc-git-print-log-follow t))

(use-package visual-fill-column
  :commands (visual-fill-column-mode global-visual-fill-column-mode))

(use-package volatile-highlights
  :after undo-tree
  :diminish
  :init
  (csetq Vhl/highlight-zero-width-ranges t)

  :config
  (volatile-highlights-mode t)

  (vhl/define-extension 'vhl-undo-tree #'undo-tree-move #'undo-tree-undo #'undo-tree-redo #'undo)
  (vhl/install-extension 'vhl-undo-tree))

(use-package wdired
  :after dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c M-w" . #'wdired-change-to-wdired-mode))
  :init
  (csetq wdired-create-parent-directories t)
  (csetq wdired-allow-to-change-permissions t))

(use-package web-mode
  :defer t
  :hook (web-mode . user/web-mode-hook)
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.jsx$\\'" . web-mode))
  :preface
  (defun user/web-mode-hook ()
    "Hook to run when `web-mode' is active."
    (smartparens-mode -1))

  :init
  (csetq web-mode-code-indent-offset 4)
  (csetq web-mode-markup-indent-offset 2)
  (csetq web-mode-css-indent-offset 2)
  (csetq web-mode-sql-indent-offset 4)
  (csetq web-mode-attr-indent-offset 2)
  (csetq web-mode-attr-value-indent-offset 2)

  (csetq web-mode-enable-current-column-highlight t)
  (csetq web-mode-enable-current-element-highlight t)
  (csetq web-mode-enable-element-content-fontification t)
  (csetq web-mode-enable-element-tag-fontification t)
  (csetq web-mode-enable-html-entities-fontification t)
  (csetq web-mode-enable-inlays t)
  (csetq web-mode-enable-sql-detection t)
  (csetq web-mode-enable-block-face t)
  (csetq web-mode-enable-part-face t)

  (csetq web-mode-engines-alist
         '(("django" . "\\.html\\'"))))

(use-package wgrep
  :init
  (csetq wgrep-auto-save-buffer t))

(use-package wgrep-ag
  :after wgrep)

(use-package which-func
  :ensure nil
  :hook ((prog-mode . which-function-mode)
         (lsp-mode . which-function-mode)))

(use-package which-key
  :diminish
  :init
  (csetq which-key-side-window-location 'right)
  (csetq which-key-idle-delay 1)
  (csetq which-key-sort-order 'which-key-prefix-then-key-order)
  ;; (csetq which-key-show-transient-maps t)

  (which-key-mode t))

(use-package whitespace
  :ensure nil
  :diminish
  :preface
  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  :hook (prog-mode . whitespace-mode)

  :init
  (csetq whitespace-display-mappings
         '((tab-mark ?\t [187 32 32 32 32 32 32 32])))
  (csetq whitespace-style '(face tab-mark trailing)))

(use-package xref
  :ensure nil
  :init
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (csetq yas-verbosity 1)
  (csetq yas-triggers-in-field t)
  (csetq yas-wrap-around-region t)
  :config
  (yas-global-mode t)
  (yas-reload-all))

(provide 'init)

;;; init.el ends here

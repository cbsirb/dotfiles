;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; It's init.el, that says enough.

;;; Code:

;; Easier customization
(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(csetq gc-cons-threshold (* 10 gc-cons-threshold))

;; Apparently, when using default values, the input lag and hangs disappears
;; (csetq gc-cons-threshold (* 384 1024 1024))

;; (defconst user-original-gc-cons (* 7 gc-cons-threshold)
;;   "My default value of `gc-cons-threshold'.")

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (csetq gc-cons-threshold user-original-gc-cons)))

;; (defvar user-pre-minibuffer-gc-cons gc-cons-threshold)

;; (defun user-minibuffer-setup-hook ()
;;   "Hook to run when entering the minibuffer."
;;   (csetq user-pre-minibuffer-gc-cons gc-cons-threshold)
;;   (csetq gc-cons-threshold most-positive-fixnum)
;;   (setq-local show-trailing-whitespace nil))

;; (defun user-minibuffer-exit-hook ()
;;   "Hook to run when exiting the minibuffer."
;;   (csetq gc-cons-threshold user-pre-minibuffer-gc-cons))

;; ;; Increase the memory while in the minibuffer
;; (add-hook 'minibuffer-setup-hook #'user-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'user-minibuffer-exit-hook)

(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(defconst user-custom-file (expand-file-name "custom.el" user-emacs-directory)
  "File used to store settings from Customization UI.")

(when (file-exists-p user-custom-file)
  (load-file user-custom-file))

(csetq load-prefer-newer t)

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init" user-emacs-directory))
  (package-initialize))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(add-hook 'after-init-hook
          (lambda ()
            (message "Time to load init file: %s"
                     (emacs-init-time))
            (garbage-collect)))

;; Install use-package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(csetq use-package-enable-imenu-support t)
(csetq use-package-expand-minimally t)
(csetq use-package-always-ensure t)
(require 'use-package)

;;; Packages needed no matter what, and usually others are depended on it
(use-package habamax-theme
  :config
  (load-theme 'habamax t))

(use-package ignoramus
  :config
  (ignoramus-setup))

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
                    :family "Iosevka SS09"
                    :height 105
                    :weight 'regular)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 110
                    :weight 'regular)

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(size-indication-mode -1)
(line-number-mode t)
(column-number-mode t)
(csetq visible-cursor nil)

(csetq display-buffer-alist
       `((,(rx bos
               (or "*Compile-Log*"
                   "*Warnings*"
                   "*compilation"
                   "*rg*"
                   "*grep*"
                   "*ag search*"
                   "*Occur*"
                   "*xref*"
                   "*Flymake diagnostics"
                   "*ivy-"
                   "*hgrep*"
                   ))
          (display-buffer-reuse-window
           display-buffer-in-side-window)
          (side            . bottom)
          (reusable-frames . nil)
          (window-height   . 0.25))

         ;; Let `display-buffer' reuse visible frames for all buffers. This must be
         ;; the last entry in `display-buffer-alist', because it overrides any later
         ;; entry with more specific actions.
         ("." nil (reusable-frames . nil))
         ))

;;; Global functions
(defun user-results-buffer-hook ()
  "Set various settings on results buffers (compilation, grep, etc.)."
  (setq-local scroll-margin 0)
  (setq-local show-trailing-whitespace nil))

;;; Keybindings
(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-\M-m] [C-M-m])

(bind-key "M-u" #'upcase-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-c" #'capitalize-dwim)

(bind-key "C-c t d" #'toggle-debug-on-error)
(bind-key "C-c t q" #'toggle-debug-on-quit)
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key "M-g" #'goto-line)
(bind-key "C-8" #'repeat-complex-command)

(bind-key "C-o" #'isearch-occur isearch-mode-map)
(bind-key "<tab>" #'isearch-repeat-forward isearch-mode-map)
(bind-key "<backtab>" #'isearch-repeat-backward isearch-mode-map)

(unbind-key "C-z")
(unbind-key "C-x C-z")
(unbind-key "C-x f")
(unbind-key "C-x m")
(unbind-key "<insert>")
(unbind-key "M-o")
(unbind-key "C-x >")
(unbind-key "C-x <")
(unbind-key "<C-next>")
(unbind-key "<C-prior>")

;;; Packages

(use-package abbrev
  :ensure nil
  :hook (find-file . user-disable-abbrev-mode)
  :preface
  (defun user-disable-abbrev-mode ()
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

(use-package anzu
  :demand t
  :bind (([remap query-replace] . #'anzu-query-replace)
         ([remap query-replace-regexp] . #'anzu-query-replace-regexp)
         ("C-c r" . #'anzu-query-replace-at-cursor-thing)
         :map isearch-mode-map
         ([remap isearch-query-replace] . #'anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . #'anzu-isearch-query-replace-regexp))
  :diminish
  :init
  (csetq anzu-replace-to-string-separator " => ")

  :config
  (global-anzu-mode t))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :init
  (csetq auto-revert-verbose nil)
  (csetq global-auto-revert-non-file-buffers t)

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
  :hook (c-mode-common . user-c-mode-common-hook)
  :preface
  (defconst user-allman-style
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
                                        ;; (brace-list-open)
                                        ;; (brace-list-close)
                                        ;; (brace-list-intro)
                                        ;; (brace-list-entry)
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

  (defun user-cc-goto-def ()
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

  (defun user-c-mode-common-hook ()
    "Hook for C/C++ mode."
    (c-toggle-electric-state t)
    (c-toggle-syntactic-indentation t))

  :config
  (c-add-style "allman" user-allman-style)

  (csetq c-default-style '((java-mode . "java")
                           (awk-mode . "awk")
                           (other . "linux"))))

(use-package cmake-mode
  :defer t
  :mode "CMakeLists\\.txt\\'")

(use-package comment-dwim-2
  :after selected
  :bind (("M-;" . #'comment-dwim-2)
         :map selected-keymap
         (";" . #'comment-dwim-2)))

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

  (use-package user-completion
    :load-path "lisp"
    :bind (("C-c /" . #'user-complete-line)))
  :config
  (csetq company-backends (append '(company-capf) (delete 'company-capf company-backends))))

(use-package compile
  :ensure nil
  :diminish compilation-in-progress
  :bind (("C-c c" . #'compile))
  :preface
  (defvar user-compile-process nil
    "The current compilation process or nil if none.")

  (defun user-bury-buffer (window buffer)
    "Bury the BUFFER and switch to the previous buffer in WINDOW, ignoring
errors.

If the previous buffer cannot be found for the WINDOW, then it will simply
delete the WINDOW."
    (ignore-errors
      (unless (switch-to-prev-buffer window)
        (delete-window window))
      (bury-buffer buffer)))

  (defun user-compile-start (proc)
    (when (string-equal (buffer-name (current-buffer)) "*compilation*")
      (setq user-compile-process proc)))

  (defun user-compile-done (buffer _msg)
    (when (string-equal "*compilation*" (buffer-name buffer))
      (let* ((exit-status (process-exit-status user-compile-process))
             (has-errors  (/= 0 exit-status))
             (window (get-buffer-window buffer)))

        (when (and window (not has-errors))
          (run-at-time "1 sec" nil #'user-bury-buffer window buffer)))

      (setq user-compile-process nil)))

  (require 'ansi-color)

  (defun user-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer.
Taken from http://stackoverflow.com/a/3072831/355252."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  :hook ((compilation-start . user-compile-start)
         (compilation-filter . user-colorize-compilation-buffer)
         (compilation-mode . user-results-buffer-hook))

  :init
  (csetq compilation-always-kill t)
  (csetq compilation-ask-about-save nil)
  (csetq compilation-auto-jump-to-first-error nil)
  (csetq compilation-context-lines 3)
  (csetq compilation-disable-input t)
  (csetq compilation-scroll-output 'first-error)

  :config
  (add-hook 'compilation-finish-functions #'user-compile-done))

(use-package counsel
  :after ivy
  :diminish
  :init
  (csetq counsel-describe-function-preselect 'ivy-function-called-at-point)
  (csetq counsel-grep-post-action-hook '(recenter))
  :config
  (counsel-mode t))

(use-package cquery
  :after cc-mode
  ;; :commands (lsp-cquery-enable)
  :bind (:map c-mode-base-map
              ("M-o" . #'user-cquery-show/body))
  :preface
  (defhydra user-cquery-show (:exit t)
    ("b" (cquery-xref-find-custom "$cquery/base") "base")
    ("c" (cquery-xref-find-custom "$cquery/callers") "callers")
    ("d" (cquery-xref-find-custom "$cquery/derived") "derived")
    ("v" (cquery-xref-find-custom "$cquery/vars") "vars"))
  :init
  (csetq cquery-extra-init-params '(:diagnostics (:frequencyMs -1)))
  (csetq cquery-project-roots '("compile_commands.json")))

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
  (csetq custom-file user-custom-file)
  (csetq custom-unlispify-tag-names nil)
  (csetq custom-unlispify-menu-entries nil))

(use-package cython-mode :defer t)

(use-package dabbrev
  :ensure nil
  :init
  (csetq dabbrev-case-replace nil)
  (csetq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*$]"))

(use-package diff-mode
  :ensure nil
  :hook ((diff-mode . diff-delete-empty-files)
         (diff-mode . diff-make-unified)
         (diff-mode . smerge-mode))
  :init
  (csetq diff-switches '("-u" "-p" "-w")))

(use-package diff-hl
  :disabled t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (find-file . diff-hl-mode)))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("SPC" . #'dired-mark)
              ("C-c C-w" . #'wdired-change-to-wdired-mode)
              ("<C-return>" . #'user-open-in-external-app)
              ("<tab>" . #'dired-next-window))
  :preface
  (defun user-open-in-external-app ()
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
  (defun user-dired-toggle-mode-hook ()
    (dired-hide-details-mode t)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))

  :hook (dired-toggle-mode . user-dired-toggle-mode-hook)
  :init
  (csetq dired-toggle-window-size 40))

(use-package dired-x
  :ensure nil
  :after dired
  :init
  (csetq dired-omit-verbose nil))

(use-package ediff
  :ensure nil
  :defer t
  :preface
  (defun user-command-line-diff (_switch)
    "Add the -diff <file1> <file2> argument to Emacs.
_SWITCH should be 'diff'."
    (if (> 2 (length command-line-args-left))
        (error "The -diff requires 2 files")
      (let ((file1 (pop command-line-args-left))
            (file2 (pop command-line-args-left)))
        (ediff file1 file2))))

  (add-to-list 'command-switch-alist '("diff"  . user-command-line-diff))

  :init
  (csetq ediff-diff-options "-w")
  (csetq ediff-highlight-all-diffs nil)
  (csetq ediff-show-clashes-only t)
  (csetq ediff-split-window-function #'split-window-horizontally)
  (csetq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package editorconfig
  :disabled t
  :diminish
  :hook (prog-mode . editorconfig-mode))

(use-package eglot
  :hook ((c-mode-common . eglot-ensure)
         (python-mode . eglot-ensure))
  :init
  (csetq eglot-events-buffer-size 0)
  :config

  (cl-defmethod eglot-initialization-options ((server eglot-cquery))
    "Passes through required cquery initialization options"
    (let* ((root (car (project-roots (eglot--project server))))
           (cache (expand-file-name ".cquery_cached_index/" root)))
      (list :cacheDirectory (file-name-as-directory cache)
            :progressReportFrequencyMs -1
            :diagnostics (list :onType :json-false)
            :completion (list :detailedLabel t)
            )))

  (csetq eglot-ignored-server-capabilites
         '(:documentHighlightProvider
           :foldingRangeProvider
           :documentOnTypeFormattingProvider
           :codeLensProvider
           :documentLinkProvider
           :colorProvider))

  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . (eglot-cquery "cquery"))))

(use-package eshell
  :ensure nil
  :defer t
  :hook ((eshell-mode . user-eshell-mode-hook)
         (eshell-first-time-mode . user-eshell-first-time-mode-hook))
  :preface
  (defun user-eshell-mode-hook ()
    (company-mode -1)
    (setq-local scroll-margin 0))

  (defun user-eshell-first-time-mode-hook ()
    (add-to-list 'eshell-modules-list 'eshell-rebind)
    (add-to-list 'eshell-modules-list 'eshell-smart)
    (add-to-list 'eshell-modules-list 'eshell-xtra))

  :init
  (csetq eshell-hist-ignoredups t)
  (csetq eshell-history-size 50000)
  (csetq eshell-ls-dired-initial-args (quote ("-h")))
  (csetq eshell-ls-exclude-regexp "~\\'")
  (csetq eshell-ls-initial-args "-h")
  ;; (csetq eshell-prompt-function (lambda ()
  ;;                                 (concat
  ;;                                  (abbreviate-file-name
  ;;                                   (eshell/pwd))
  ;;                                  (if (= (user-uid) 0) " # " " $ "))))
  ;; (csetq eshell-rebind-keys-alist
  ;;   (quote
  ;;    (([(control 97)]
  ;;      . eshell-bol)
  ;;     ([home]
  ;;      . eshell-bol)
  ;;     ([(control 100)]
  ;;      . eshell-delchar-or-maybe-eof)
  ;;     ([backspace]
  ;;      . eshell-delete-backward-char)
  ;;     ([delete]
  ;;      . eshell-delete-backward-char))))
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
  :bind ("C-c e" . #'user-eyebrowse-hydra/body)
  :preface
  (defhydra user-eyebrowse-hydra (:color blue)
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
  :bind (("C-c f n" . #'flymake-goto-next-error)
         ("C-c f p" . #'flymake-goto-prev-error))
  :init
  (csetq flymake-no-changes-timeout nil)
  (csetq flymake-start-syntax-check-on-newline nil))

(use-package grep
  :ensure nil
  :defer t
  :hook (grep-mode . user-results-buffer-hook))

(use-package gud
  :ensure nil
  :defer t
  :hook (gud-mode . user-gud-mode-hook)
  :preface
  (defun user-gud-mode-hook ()
    "Hook to run when GUD mode is activated."
    (company-mode -1))
  :init
  (csetq gdb-many-windows t))

;; (use-package helm
;;   :diminish
;;   :demand
;;   :bind (([remap execute-extended-command] . #'helm-M-x)
;;          ([remap find-file] . #'helm-find-files)
;;          ([remap imenu] . #'helm-imenu)
;;          ([remap switch-to-buffer] . #'helm-mini)
;;          ([remap yank-pop] . #'helm-show-kill-ring)
;;          ("C-c h" . #'helm-command-prefix)
;;          ("C-h SPC" . #'helm-all-mark-rings)
;;          :map minibuffer-local-map
;;          ("C-c C-l" . #'helm-minibuffer-history)
;;          :map helm-map
;;          ("<tab>" . #'helm-execute-persistent-action)
;;          ("C-i" . #'helm-execute-persistent-action)
;;          ("C-z" . #'helm-select-action))
;;   :hook (helm-minibuffer-set-up . helm-hide-minibuffer-maybe)
;;   :init
;;   (require 'helm-config)

;;   ;; Display in own frame
;;   ;; (csetq helm-display-function 'helm-display-buffer-in-own-frame)
;;   ;; (csetq helm-display-buffer-reuse-frame t)
;;   ;; (csetq helm-use-undecorated-frame-option t)

;;   (setq helm-autoresize-max-height 20)
;;   (setq helm-autoresize-min-height 20)

;;   (csetq helm-echo-input-in-header-line t)
;;   (csetq helm-ff-file-name-history-use-recentf t)
;;   (csetq helm-net-prefer-curl t)
;;   (csetq helm-split-window-inside-p t)

;;   :config
;;   (helm-mode t)
;;   (helm-autoresize-mode t))

;; (use-package helm-projectile
;;   :after helm
;;   :config
;;   (helm-projectile-on))

;; (use-package helm-descbinds
;;   :after helm
;;   :config
;;   (helm-descbinds-mode t))

(use-package hexl
  :if (< emacs-major-version 27)
  :ensure nil
  :init
  (csetq hexl-bits 8))

(use-package highlight-indent-guides
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (csetq highlight-indent-guides-method 'character)
  (csetq highlight-indent-guides-character ?\|))

(use-package highlight-symbol
  :diminish
  :bind (:map highlight-symbol-nav-mode-map
              ("M-N" . highlight-symbol-next-in-defun)
              ("M-P" . highlight-symbol-prev-in-defun))
  :hook ((find-file . highlight-symbol-mode)
         (find-file . highlight-symbol-nav-mode))
  :init
  (csetq highlight-symbol-idle-delay 0.4)
  (csetq highlight-symbol-highlight-single-occurrence nil))

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
  :bind ("M-i" . imenu)
  :hook (imenu-after-jump . recenter-top-bottom)
  :init
  (csetq imenu-auto-rescan t)
  (csetq imenu-auto-rescan-maxout (* 1024 1024)))

(use-package imenu-anywhere
  :bind (("M-I" . imenu-anywhere)))

(use-package isearch
  :ensure nil
  :init
  (csetq isearch-allow-scroll t))

(use-package ivy
  :demand t
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-c v s" . ivy-push-view)
         ("C-c v p" . ivy-pop-view)
         :map ivy-mode-map
         ([escape] . user-minibuffer-keyboard-quit))
  :init
  (csetq ivy-count-format "(%d/%d) ")
  (csetq ivy-display-style 'fancy)
  (csetq ivy-dynamic-exhibit-delay-ms 150)
  (csetq ivy-height 9)
  (csetq ivy-on-del-error-function nil)
  (csetq ivy-use-selectable-prompt t)
  (csetq ivy-use-virtual-buffers t)
  (csetq ivy-virtual-abbreviate 'full)
  (csetq ivy-wrap t)

  :config
  (ivy-mode t))

(use-package ivy-rich
  :after ivy
  :init
  (csetq ivy-rich-switch-buffer-align-virtual-buffer t)
  (csetq ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode t))

(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :init
  (csetq js2-skip-preprocessor-directives t))

(use-package json-mode
  :bind (:map json-mode-map
              ("M-q" . json-reformat-region))
  :init
  (csetq json-reformat:indent-width 4)
  (csetq json-reformat:pretty-string? t))

(use-package jump-char
  :bind (("M-m" . #'jump-char-forward)
         ("M-M" . #'jump-char-backward))
  :init
  (unless (boundp 'lazy-highlight-face)
    (defvar lazy-highlight-face 'lazy-highlight
      "Was removed from emacs 26, so redefine it until jump-char fixes it.")))

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
  (key-seq-define-global "jf" #'counsel-find-file))

(use-package magit
  :bind (("C-x g" . magit-status))
  :hook (git-commit-mode . git-commit-turn-on-flyspell)
  :init
  (csetq magit-diff-arguments
         '("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat" "--diff-algorithm=histogram"))
  (csetq magit-diff-refine-hunk t)
  (csetq magit-display-buffer-function
         #'magit-display-buffer-fullframe-status-v1)
  (csetq magit-process-popup-time 15)
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
                 "\\`\\*\\(Wo\\)?Man .*\\*\\'"))

  (midnight-mode t))

(use-package misc
  :ensure nil
  :bind (("M-z" . #'zap-up-to-char)
         ("<C-right>" . #'forward-to-word)))

(use-package mouse-copy
  :ensure nil)

(use-package multi-term
  :if (eq system-type 'gnu/linux)
  :bind (("C-; C-;" . multi-term-next)
         ("C-; c" . multi-term)
         ("C-; d" . multi-term-dedicated-toggle)
         ("C-; n" . multi-term-next)
         ("C-; p" . multi-term-prev))
  :hook (term-mode . user-term-mode-hook)
  :preface
  (defun user-term-mode-hook ()
    (company-mode -1)
    (setq-local scroll-margin 0))

  :init
  ;; (csetq multi-term-program "screen")
  ;; (csetq multi-term-program-switches "-DR")
  (csetq multi-term-dedicated-select-after-open-p t)
  (csetq multi-term-scroll-show-maximum-output t))

(use-package multiple-cursors
  :bind (("C-," . mc/unmark-next-like-this)
         ("C-<" . mc/unmark-previous-like-this)
         ("C-." . mc/mark-next-like-this)
         ("C->" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c m ^"     . mc/edit-beginnings-of-lines)
         ("C-c m `"     . mc/edit-beginnings-of-lines)
         ("C-c m $"     . mc/edit-ends-of-lines)
         ("C-c m '"     . mc/edit-ends-of-lines)
         ("C-c m R"     . mc/reverse-regions)
         ("C-c m S"     . mc/sort-regions)
         ("C-c m W"     . mc/mark-all-words-like-this)
         ("C-c m Y"     . mc/mark-all-symbols-like-this)
         ("C-c m a"     . mc/mark-all-like-this-dwim)
         ("C-c m c"     . mc/mark-all-dwim)
         ("C-c m l"     . mc/insert-letters)
         ("C-c m n"     . mc/insert-numbers)
         ("C-c m r"     . mc/mark-all-in-region)
         ("C-c m s"     . set-rectangular-region-anchor)
         ("C-c m %"     . mc/mark-all-in-region-regexp)
         ("C-c m t"     . mc/mark-sgml-tag-pair)
         ("C-c m w"     . mc/mark-next-like-this-word)
         ("C-c m x"     . mc/mark-more-like-this-extended)
         ("C-c m y"     . mc/mark-next-like-this-symbol)
         ("C-c m C-SPC" . mc/mark-pop)
         ("C-c m ("     . mc/mark-all-symbols-like-this-in-defun)
         ("C-c m C-("   . mc/mark-all-words-like-this-in-defun)
         ("C-c m M-("   . mc/mark-all-like-this-in-defun)
         ("C-c m ["     . mc/vertical-align-with-space)
         ("C-c m {"     . mc/vertical-align)
         :map selected-keymap
         ("c"   . mc/edit-lines)
         ("."   . mc/mark-next-like-this)
         ("<"   . mc/unmark-next-like-this)
         ("C->" . mc/skip-to-next-like-this)
         (","   . mc/mark-previous-like-this)
         (">"   . mc/unmark-previous-like-this)
         ("C-<" . mc/skip-to-previous-like-this)
         ("y"   . mc/mark-next-symbol-like-this)
         ("Y"   . mc/mark-previous-symbol-like-this)
         ("w"   . mc/mark-next-word-like-this)
         ("W"   . mc/mark-previous-word-like-this))
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
  :bind (("C-\\" . objed-activate)
         :map objed-map
         ("-" . objed-forward-symbol)
         ("RET" . objed-quit))
  :config
  (add-to-list 'objed-keeper-commands 'undo-tree-undo)
  (add-to-list 'objed-keeper-commands 'undo-tree-redo))

(use-package origami
  :hook (prog-mode . origami-mode)
  :bind (("C-x n n" . origami-forward-toggle-node)
         ("C-x n t" . origami-recursively-toggle-node)
         ("C-x n a" . origami-toggle-all-nodes)
         ("C-x n o" . origami-open-node)
         ("C-x n c" . origami-close-node)
         ("C-x n u" . origami-undo)
         ("C-x n _" . origami-redo)
         ("C-x n r" . origami-reset)))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (csetq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package prog-mode
  :ensure nil
  :preface
  (defun user-prog-mode-hook ()
    "Settings for all programming modes."
    (setq-local show-trailing-whitespace t))

  :hook (prog-mode . user-prog-mode-hook))

(use-package projectile
  :diminish
  :preface
  (defun user-projectile-invalidate-cache (&rest _args)
    (projectile-invalidate-cache nil))
  :init
  (csetq projectile-completion-system 'ivy)
  (csetq projectile-enable-caching t)
  (csetq projectile-indexing-method 'alien)
  (csetq projectile-sort-order 'recentf)
  (csetq projectile-use-git-grep t)
  (csetq projectile-keymap-prefix (kbd "C-c p"))

  :config
  (projectile-mode t)

  (add-to-list 'projectile-globally-ignored-directories ".vscode")
  (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")

  (eval-after-load 'magit-branch
    '(progn
       (advice-add 'magit-checkout
                   :after #'user-projectile-invalidate-cache)
       (advice-add 'magit-branch-and-checkout
                   :after #'user-projectile-invalidate-cache))))

(use-package python
  :ensure nil
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
  :hook (python-mode . user-auto-virtualenv)
  :preface
  (defun user-auto-virtualenv ()
    (pyvenv-mode t)

    ;; A dolist would be appropriate, but I only use venv as virtualenv name
    ;; This also works with lsp-mode since it will use the python inside
    (let ((root (locate-dominating-file default-directory "venv")))
      (if (and root (file-exists-p root))
          (pyvenv-activate (expand-file-name "venv" root))))))

(use-package realgud
  :commands (realgud:bashdb realgud:gdb realgud:gub realgud:ipdb
                            realgud:jdb realgud:kshdb realgud:nodejs realgud:pdb
                            realgud:perldb realgud:zshdb))

(use-package recentf
  :ensure nil
  :after ignoramus
  :init
  (csetq recentf-auto-cleanup 'never)
  (csetq recentf-exclude (list
                          "/\\.git/.*\\'"        ; Git contents
                          "/elpa/.*\\'"          ; Package files
                          "PKGBUILD"             ; ArchLinux aur
                          "COMMIT_MSG"
                          "COMMIT_EDITMSG"
                          "crontab.*"
                          #'ignoramus-boring-p))
  (csetq recentf-max-saved-items 500)
  (csetq recentf-max-menu-items 20)

  :config
  (use-package no-littering
    :config
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))

  (recentf-mode t)
  (run-at-time (* 5 60) (* 5 60) #'recentf-save-list))

(use-package rg
  :demand t
  :hook (rg-mode . wgrep-ag-setup)
  :init
  (csetq rg-ignore-case 'smart)
  (csetq rg-hide-command nil)
  :config
  (rg-enable-default-bindings))

(use-package rmsbolt)

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
  :demand t
  :diminish selected-minor-mode
  :config
  (selected-global-mode t))

(use-package shackle
  :disabled
  :init
  (csetq shackle-select-reused-windows t)
  (csetq hackle-inhibit-window-quit-on-same-windows t)
  (csetq shackle-default-alignment 'right)
  (csetq shackle-rules '((help-mode :align t :select t)
                         ((compilation-mode rg-mode grep-mode occur-mode xref--xref-buffer-mode)
                          :noselect t :align below :size 0.25)
                         ("\\`\\*magit.*?\\*\\'" :regexp t :ignore t)
                         ))
  (csetq shackle-default-rule '(:select t))
  :config
  (shackle-mode t))

(use-package sh-script
  :ensure nil
  :init
  (csetq sh-basic-offset 2))

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
  (defun user-open-block-c-mode (_id action _context)
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

  :config
  (require 'smartparens-config)

  (sp-local-pair 'c-mode "{" nil :post-handlers '(:add user-open-block-c-mode))
  (sp-local-pair 'c++-mode "{" nil :post-handlers '(:add user-open-block-c-mode))

  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

  (show-smartparens-global-mode t))

(use-package smex
  :defer t
  :config
  (smex-initialize))

(use-package tramp
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
  :bind (("<C-return>" . #'user-open-line-above)
         ("C-a" . #'user-smarter-move-beginning-of-line)
         ("C-w" . #'user-smarter-kill-word-or-region)
         ([remap kill-ring-save] . #'user-smarter-copy-line-or-region)
         ([remap backward-kill-word] . #'user-smarter-backward-kill-word)
         ("M-]" . #'user-next-error)
         ("M-[" . #'user-prev-error)
         ([remap forward-paragraph] . #'user-forward-paragraph)
         ([remap backward-paragraph] . #'user-backward-paragraph)
         ("M-j" . #'user-join-line)
         ("C-`" . #'user-open-terminal)
         ([remap scroll-up-command] . #'user-scroll-half-page-up)
         ([remap scroll-down-command] . #'user-scroll-half-page-down)
         ("C-'" . #'push-mark-no-activate)
         ("M-'" . #'jump-to-mark)
         ([remap exchange-point-and-mark] . #'exchange-point-and-mark-no-activate)
         :map isearch-mode-map
         ("<backspace>" . #'user-isearch-delete)
         :map minibuffer-local-map
         ("<escape>" . #'user-minibuffer-keyboard-quit)
         :map minibuffer-local-ns-map
         ("<escape>" . #'user-minibuffer-keyboard-quit)
         :map minibuffer-local-completion-map
         ("<escape>" . #'user-minibuffer-keyboard-quit)
         :map minibuffer-local-must-match-map
         ("<escape>" . #'user-minibuffer-keyboard-quit)
         :map minibuffer-local-isearch-map
         ("<escape>" . #'user-minibuffer-keyboard-quit)))

(use-package user-window
  :load-path "lisp"
  :bind ("C-c w" . #'user-windows-hydra/body)
  :preface
  (defhydra user-windows-hydra (:color pink)
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
    ("d" user-toggle-current-window-dedication :color blue)
    ("f" toggle-frame-fullscreen :color blue)
    ("i" enlarge-window)
    ("j" shrink-window-horizontally)
    ("k" shrink-window)
    ("l" enlarge-window-horizontally)
    ("o" other-window)
    ("m" user-switch-to-minibuffer :color blue)
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

(use-package vlf
  :defer t
  :defines vlf-forbidden-modes-list
  :config
  (require 'vlf-setup)
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))

(use-package volatile-highlights
  :after undo-tree
  :diminish
  :init
  (csetq Vhl/highlight-zero-width-ranges t)

  :config
  (volatile-highlights-mode t)

  (vhl/define-extension 'vhl-undo-tree #'undo-tree-move #'undo-tree-undo #'undo-tree-redo #'undo)
  (vhl/install-extension 'vhl-undo-tree))

(use-package web-mode
  :defer t
  :hook (web-mode . user-web-mode-hook)
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
  (defun user-web-mode-hook ()
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
  :config
  (which-function-mode t))

(use-package which-key
  :diminish
  :init
  (csetq which-key-side-window-location 'right)
  (csetq which-key-idle-delay 1)
  (csetq which-key-sort-order 'which-key-prefix-then-key-order)

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

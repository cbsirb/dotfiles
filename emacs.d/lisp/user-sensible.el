;;; user-sensible.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Contains sensible defaults (for me, anyhow)

;;; Code:

(csetq enable-local-variables :safe)

;; Add the --diff argument
(use-package ediff
  :defer t
  :init
  (defun user-command-line-diff (_switch)
    "Add the -diff <file1> <file2> argument to Emacs.
_SWITCH should be 'diff'."
    (if (> 2 (length command-line-args-left))
        (error "The -diff requires 2 files")
      (let ((file1 (pop command-line-args-left))
            (file2 (pop command-line-args-left)))
        (ediff file1 file2))))

  (add-to-list 'command-switch-alist '("diff"  . user-command-line-diff))

  :config
  (csetq ediff-split-window-function 'split-window-horizontally)
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain))

(defalias 'yes-or-no-p 'y-or-n-p)

(csetq use-dialog-box nil)
(csetq view-read-only t)
(csetq ad-redefinition-action 'accept)

(use-package imenu
  :defer t
  :config
  (csetq imenu-auto-rescan t))

(csetq auto-window-vscroll nil)

(csetq enable-recursive-minibuffers t)

;; Use UTF-8 as default
(csetq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; More sane defaults
(csetq backup-by-copying t)
(csetq delete-old-versions t)
(csetq echo-keystrokes 0.1)
(csetq sentence-end-double-space nil)
(csetq delete-by-moving-to-trash t)
(csetq mark-ring-max 128)
(csetq global-mark-ring-max 256)
(csetq save-interprogram-paste-before-kill t)
(csetq kill-ring-max 128)
(csetq kill-do-not-save-duplicates t)
(csetq create-lockfiles nil)
(csetq read-file-name-completion-ignore-case t)
(csetq read-buffer-completion-ignore-case t)
(csetq make-pointer-invisible nil)
(csetq text-quoting-style 'grave)
(csetq next-screen-context-lines 5)
(csetq read-quoted-char-radix 16)
(csetq truncate-lines t)
(csetq select-active-regions nil)
(csetq display-raw-bytes-as-hex t)
(csetq column-number-indicator-zero-based nil)

;; very elisp-specific
(csetq eval-expression-print-length nil)
(csetq eval-expression-print-level nil)

(when (eq system-type 'gnu/linux)
  (csetq x-underline-at-descent-line t)
  (add-hook 'after-make-frame-functions (lambda (_frame)
                                            (setq-default x-selection-timeout 100))))

(remove-hook 'post-self-insert-hook 'blink-paren-post-self-insert-function)

(csetq disabled-command-function nil)

;; No need for these
(csetq initial-scratch-message "")
(csetq inhibit-splash-screen t)
(csetq inhibit-startup-echo-area-message t)
(csetq inhibit-startup-message t)
(csetq ring-bell-function 'ignore)
(fset 'display-startup-echo-area-message #'ignore)

;; When loading more than 2 files at startup hide the buffer list
(csetq inhibit-startup-buffer-menu t)

(csetq make-backup-files nil)

;; Some emacs modes that should be default
(global-auto-revert-mode t)
(diminish 'auto-revert-mode)
(transient-mark-mode t)
(delete-selection-mode t)
(winner-mode t)
(minibuffer-depth-indicate-mode t)
;; (show-paren-mode t)

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

;; "Smooth" mouse scrolling, one line at a time
(csetq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(csetq scroll-conservatively 10000)
(csetq scroll-preserve-screen-position t)
(csetq isearch-allow-scroll t)
(csetq scroll-margin 7)

(csetq fast-but-imprecise-scrolling t)

;; Disable auto-save-mode (I really hate it)
(auto-save-mode -1)
(csetq auto-save-default nil)

;; (defvar user-original-gc-cons gc-cons-threshold)

;; (defun user-minibuffer-setup-hook ()
;;   "Hook to run when entering the minibuffer."
;;   (csetq user-original-gc-cons gc-cons-threshold)
;;   (csetq gc-cons-threshold most-positive-fixnum)
;;   (setq-local show-trailing-whitespace nil))

;; (defun user-minibuffer-exit-hook ()
;;   "Hook to run when exiting the minibuffer."
;;   (csetq gc-cons-threshold user-original-gc-cons))

;; ;; Increase the memory while in the minibuffer
;; (add-hook 'minibuffer-setup-hook #'user-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'user-minibuffer-exit-hook)

;; Needed for some company backends
(when (eq system-type 'windows-nt)
  (csetq w32-pipe-read-delay 0))

;; On my system it handles 50MB files pretty well
(when (eq system-type 'gnu/linux)
  (csetq large-file-warning-threshold 50000000))

(provide 'user-sensible)

;;; user-sensible.el ends here

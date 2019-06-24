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

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

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
  (load-file (expand-file-name "early-init.el" user-emacs-directory))
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

;;
;; Packages needed no matter what, and usually others are depended on it
;;
(eval-when-compile
  (require 'use-package))

(use-package general)

(use-package use-package-chords
  :init
  (csetq key-chord-two-keys-delay 0.1)
  (csetq key-chord-one-key-delay 0.2)
  :config
  (key-chord-mode t))

(use-package minions
  :config
  (minions-mode t))

(use-package parchment-theme
  :config
  (load-theme 'parchment t))

;; (set-background-color "honeydew")

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package ignoramus
  :config
  (ignoramus-setup))

(use-package no-littering)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package hydra
  :defer t
  :config
  (hydra-add-font-lock))

;; TBD
;; (use-package frog-menu)

(use-package bug-hunter :defer t)

;;
;; Some default settings that I like
;;

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

(csetq initial-major-mode 'text-mode)
(csetq initial-scratch-message "")
(csetq inhibit-startup-buffer-menu t)
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

(csetq tramp-default-method "ssh")

(csetq uniquify-buffer-name-style 'post-forward)
(csetq uniquify-separator ":")
(csetq uniquify-ignore-buffers-re "^\\*")
(csetq uniquify-after-kill-buffer-p t)

(csetq auto-revert-verbose nil)
(csetq auto-revert-avoid-polling t)
(global-auto-revert-mode)

(csetq bookmark-save-flag t)

(csetq custom-file user/custom-file)
(csetq custom-buffer-done-kill t)
(csetq custom-unlispify-tag-names nil)
(csetq custom-unlispify-menu-entries nil)

(transient-mark-mode t)
(delete-selection-mode t)
(winner-mode t)
(minibuffer-depth-indicate-mode t)
(blink-cursor-mode -1)
(auto-save-mode -1)
(save-place-mode t)

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
(csetq sh-basic-offset 2)

(add-hook 'text-mode-hook #'auto-fill-mode)

;; Some special file names
(add-to-list 'auto-mode-alist '("\\.?bash.*" . shell-script-mode))

;;
;; Setup the gui appearance
;;
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
(defun show-trailing-whitespace()
  (setq-local show-trailing-whitespace t))

(defun hide-trailing-whitespace()
  (setq-local show-trailing-whitespace nil))

(defun user/results-buffer-hook ()
  "Set various settings on results buffers (compilation, grep, etc.)."
  (setq-local scroll-margin 0)
  (show-trailing-whitespace))

;;
;; Customization that doesn't require use-package
;;

(csetq diff-switches '("-u" "-p" "-w"))
(add-hook 'diff-mode-hook #'diff-delete-empty-files)
(add-hook 'diff-mode-hook #'diff-make-unified)
(add-hook 'diff-mode-hook #'smerge-mode)

(csetq ediff-diff-options "-w")
(csetq ediff-highlight-all-diffs nil)
(csetq ediff-show-clashes-only t)
(csetq ediff-split-window-function #'split-window-horizontally)
(csetq ediff-window-setup-function #'ediff-setup-windows-plain)

(csetq vc-follow-symlinks t)
(csetq vc-git-diff-switches '("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat" "--diff-algorithm=histogram"))
(csetq vc-git-print-log-follow t)

(csetq savehist-additional-variables '(search-ring regexp-search-ring compile-command))
(csetq savehist-autosave-interval 60)
(csetq history-length 1000)
(savehist-mode t)

(csetq flyspell-issue-message-flag nil)

(csetq hexl-bits 8)

(csetq dabbrev-case-replace nil)
(csetq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*$]")

(with-eval-after-load 'xref
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

(general-define-key
 :keymaps '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map)
 "<escape>" #'minibuffer-keyboard-quit)

;; Thank you Fuco1
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
 an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))


;;
;; Some binds that doesn't require use-package (well, we could require simple & misc, but they will be autloaded anyway)
;;

(general-unbind
  "C-x C-z"
  "C-x f"
  "M-o"
  "C-x >"
  "C-x <"
  "<C-next>"
  "<C-prior>")

(use-package simple
  :ensure nil)

(general-define-key "M-u" #'upcase-dwim)
(general-define-key "M-l" #'downcase-dwim)
(general-define-key "M-c" #'capitalize-dwim)
(general-define-key "M-g" #'goto-line)
(general-define-key "C-8" #'repeat-complex-command)
(general-define-key "M-z" #'zap-up-to-char)
(general-define-key "<C-right>" #'forward-to-word)
(general-define-key [remap just-one-space] #'cycle-spacing)
(general-define-key [remap newline] #'newline-and-indent)

(when (display-graphic-p)
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-\M-m] [C-M-m]))

(general-define-key "C-c t d" #'toggle-debug-on-error)
(general-define-key "C-c t q" #'toggle-debug-on-quit)

(key-chord-define-global "jj" #'switch-to-buffer)
(key-chord-define-global "jf" #'find-file)

;;
;; Mostly built-in packages. No real order, they are not dependent on each other...
;;

(use-package whitespace :ensure nil
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

  :ghook 'prog-mode-hook

  :init
  (csetq whitespace-display-mappings
         '((tab-mark ?\t [187 32 32 32 32 32 32 32])))
  (csetq whitespace-style '(face tab-mark trailing)))

(use-package recentf :ensure nil
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

;; (use-package midnight :ensure nil
;;   :init
;;   ;; 5 minutes for special buffers
;;   (csetq clean-buffer-list-delay-special (* 5 60))
;;   (csetq midnight-period (* 15 60))

;;   :config
;;   (midnight-delay-set 'midnight-delay 1)

;;   (add-to-list 'clean-buffer-list-kill-buffer-names
;;                '("*buffer-selection*"
;;                  "*Finder*"
;;                  "*Occur*"
;;                  "*rg*"
;;                  "*ag search*"
;;                  "*compilation*"
;;                  "*Help*"
;;                  "*Ido Completions*"
;;                  "*Finder Category*"
;;                  "*Finder-package*"
;;                  "*RE-Builder*"
;;                  "*vc-change-log*"))

;;   (add-to-list 'clean-buffer-list-kill-regexps
;;                '("\\`\\*Customize .*\\*\\'"
;;                  "\\`\\*magit: .*\\*\\'"
;;                  "\\`\\*magit-.*\\*\\'"
;;                  "\\`\\*Outline .*\\*\\'"
;;                  "\\`\\*.* annots\\*\\'"
;;                  "\\`\\*Contents*\\*\\'"
;;                  "\\`\\*ivy-occur.*\\*\\'"
;;                  "\\`\\*Ido.*\\*\\'"
;;                  "\\`\\*\\(Wo\\)?Man .*\\*\\'"))

;;   (midnight-mode t))

(use-package ibuffer :ensure nil
  :gfhook #'ibuffer-auto-mode
  :general
  ("C-x C-b" #'ibuffer)
  :init
  (csetq ibuffer-saved-filter-groups
         '(("default"

            ("Interactive" (or (mode . lisp-interaction-mode)
                               (name . "\*Messages\*")
                               (name . "\*Customize\*")))

            ("Dired" (mode . dired-mode))

            ;; Need to be before "Programming" otherwise
            ;; `emacs-lisp-mode' will match.
            ("Emacs config" (filename . ".emacs.d"))

            ("Org-Mode" (mode . org-mode))

            ("Programming" (derived-mode . prog-mode))

            ("Magit" (name . "\*magit"))

            ("Help" (or (name . "\*Help\*")
                        (name . "\*Apropos\*")
                        (name . "\*info\*")))

            ("Virtual" (name . "\*")))))

  (defalias 'list-buffers 'ibuffer)

  (csetq ibuffer-default-shrink-to-minimum-size t)
  (csetq ibuffer-show-empty-filter-groups nil))

(use-package comint :ensure nil
  :general
  (:keymaps 'comint-mode-map
   "<down>" #'comint-next-input
   "<up>"   #'comint-previous-input
   "C-n"    #'comint-next-input
   "C-p"    #'comint-previous-input
   "C-r"    #'comint-history-isearch-backward)
  :init
  (csetq comint-process-echoes t)
  (csetq comint-prompt-read-only t)
  (csetq comint-history-isearch t)
  (csetq comint-ignore-dups t)
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m))

(use-package compile :ensure nil
  :chords (("jc" . compile-without-ask))
  :general
  ([remap comment-region] #'compile-without-ask)
  ("C-c c" #'compile-without-ask)
  :preface
  (defun compile-without-ask (ask)
    (interactive "P")
    (let ((compilation-read-command (if ask t nil)))
      (if projectile-mode
          (projectile-compile-project compilation-read-command)
        (compile compile-command))))

  (defun user/switch-to-compilation-window (buffer _msg)
    (select-window (get-buffer-window buffer)))

  :init
  (csetq compilation-always-kill t)
  (csetq compilation-ask-about-save nil)
  (csetq compilation-auto-jump-to-first-error nil)
  (csetq compilation-context-lines nil)
  (csetq compilation-disable-input t)
  (csetq compilation-scroll-output 'first-error)

  :config
  (add-hook 'compilation-mode-hook #'user/results-buffer-hook)
  (add-hook 'compilation-finish-functions #'user/switch-to-compilation-window))

(use-package dired-x :ensure nil
  :general
  (:keymaps 'dired-mode-map
   "SPC" #'dired-mark
   "<C-return>" #'user/open-in-external-app
   "<tab>" #'user/dired-next-window)
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
  (csetq dired-recursive-copies 'always)
  (csetq dired-omit-verbose nil))

(use-package wdired :ensure nil
  :general
  (:keymaps 'dired-mode-map
   "C-c M-w" #'wdired-change-to-wdired-mode)
  :init
  (csetq wdired-create-parent-directories t)
  (csetq wdired-allow-to-change-permissions t))

(use-package dired-du
  :commands dired-du-mode
  :init
  (csetq dired-du-size-format t)
  (csetq dired-du-update-headers t))

(use-package dired-narrow
  :commands dired-narrow
  :general
  (:keymaps 'dired-mode-map
   "/" #'dired-narrow))

;;
;; Editing & navigation
;;

(use-package expand-region
  :general
  ("M-2" #'er/expand-region)
  ("M-1" #'er/contract-region)
  ("M-@" #'er/contract-region)
  :init
  (csetq expand-region-fast-keys-enabled nil)
  (csetq expand-region-autocopy-register "e"))

(use-package easy-kill
  :disabled
  :general
  ([remap kill-ring-save] #'easy-kill)
  ([remap mark-sexp] #'easy-mark))

(use-package symbol-overlay
  :ghook find-file-hook
  :general
  ("M-*" #'symbol-overlay-put)
  ("M-n" #'symbol-overlay-jump-next)
  ("M-p" #'symbol-overlay-jump-prev)
  ("M-8" #'symbol-overlay-toggle-in-scope)
  :init
  (csetq symbol-overlay-displayed-window t))

(use-package iy-go-to-char
  :general
  ("M-m" #'iy-go-to-char)
  ("M-M" #'iy-go-to-char-backward))

(use-package smartparens
  :demand t
  :general
  ("C-M-k"  #'sp-kill-sexp)
  ("C-M-n"  #'sp-next-sexp)
  ("C-M-p"  #'sp-previous-sexp)
  ("C-M-f"  #'sp-forward-sexp)
  ("C-M-b"  #'sp-backward-sexp)
  ("C-M-u"  #'sp-backward-up-sexp)
  ("C-M-d"  #'sp-down-sexp)
  ("C-("    #'sp-wrap-round)
  ("C-{"    #'sp-wrap-curly)
  ("C-M-<right>" #'sp-forward-slurp-sexp)
  ("C-M-<left>"  #'sp-forward-barf-sexp)

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

  :ghook 'prog-mode-hook
         ('minibuffer-setup #'turn-on-smartparens-strict-mode)

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

(use-package multiple-cursors
  :defer t
  :general
  ("C-S-<mouse-1>" #'mc/toggle-cursor-on-click)

  ("C->"         #'mc/unmark-next-like-this)
  ("C-<"         #'mc/unmark-previous-like-this)
  ("C-."         #'mc/mark-next-like-this)
  ("C-,"         #'mc/mark-previous-like-this)
  ("C-c m ^"     #'mc/edit-beginnings-of-lines)
  ("C-c m `"     #'mc/edit-beginnings-of-lines)
  ("C-c m $"     #'mc/edit-ends-of-lines)
  ("C-c m '"     #'mc/edit-ends-of-lines)
  ("C-c m R"     #'mc/reverse-regions)
  ("C-c m S"     #'mc/sort-regions)
  ("C-c m W"     #'mc/mark-all-words-like-this)
  ("C-c m Y"     #'mc/mark-all-symbols-like-this)
  ("C-c m a"     #'mc/mark-all-like-this-dwim)
  ("C-c m c"     #'mc/mark-all-dwim)
  ("C-c m l"     #'mc/insert-letters)
  ("C-c m n"     #'mc/insert-numbers)
  ("C-c m r"     #'mc/mark-all-in-region)
  ("C-c m s"     #'set-rectangular-region-anchor)
  ("C-c m %"     #'mc/mark-all-in-region-regexp)
  ("C-c m t"     #'mc/mark-sgml-tag-pair)
  ("C-c m w"     #'mc/mark-next-like-this-word)
  ("C-c m x"     #'mc/mark-more-like-this-extended)
  ("C-c m y"     #'mc/mark-next-like-this-symbol)
  ("C-c m C-SPC" #'mc/mark-pop)

  ("C-c m ("     #'mc/mark-all-symbols-like-this-in-defun)
  ("C-c m C-("   #'mc/mark-all-words-like-this-in-defun)
  ("C-c m M-("   #'mc/mark-all-like-this-in-defun)
  ("C-c m d"     #'mc/mark-all-symbols-like-this-in-defun)
  ("C-c m C-d"   #'mc/mark-all-words-like-this-in-defun)
  ("C-c m M-d"   #'mc/mark-all-like-this-in-defun)

  ("C-c m ["     #'mc/vertical-align-with-space)
  ("C-c m {"     #'mc/vertical-align)
  :preface
  (defun mc-prompt-once-advice (fn &rest args)
    (setq mc--this-command (lambda () (interactive) (apply fn args)))
    (apply fn args))

  (defun mc-prompt-once (&rest fns)
    (dolist (fn fns)
      (advice-add fn :around #'mc-prompt-once-advice)))

  :config
  (mc-prompt-once #'zap-up-to-char #'sp-rewrap-sexp))

;;
;; Completion-related
;;

(use-package ivy
  :general
  ("C-c C-r" #'ivy-resume)
  ("C-c v s" #'ivy-push-view)
  ("C-c v p" #'ivy-pop-view)
  (:keymaps 'ivy-mode-map
   "<escape>" #'minibuffer-keyboard-quit)
  :init
  (csetq ivy-count-format "(%d/%d) ")
  (csetq ivy-height 9)
  ;; (csetq ivy-on-del-error-function nil)
  ;; (csetq ivy-use-selectable-prompt t)
  (csetq ivy-use-virtual-buffers t)
  (csetq ivy-virtual-abbreviate 'full)
  (csetq ivy-wrap t)
  (ivy-mode t))

(use-package counsel
  :init
  (csetq counsel-describe-function-preselect 'ivy-function-called-at-point)
  (csetq counsel-grep-post-action-hook '(recenter))
  (csetq counsel-mode-override-describe-bindings t)
  :config
  (counsel-mode t))

(use-package ivy-rich
  :init
  (csetq ivy-rich-switch-buffer-align-virtual-buffer t)
  (csetq ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode t))

(use-package ivy-posframe
  :disabled
  :commands ivy-posframe-mode
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

(use-package company
  :preface
  (defun disable-company-mode ()
    (company-mode -1))

  :general
  (:keymaps 'company-active-map
   "ESC" #'company-abort
   "C-l" #'company-show-location
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "C-w" nil)
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
  :config
  (global-company-mode t))

(use-package eacl
  :commands eacl-complete-line
  :general ("C-x C-l" #'eacl-complete-line))

;;
;; Extra modes
;;

(use-package cmake-mode :defer t)

(use-package cmake-font-lock
  :ghook ('cmake-mode #'cmake-font-lock-activate))

(use-package cython-mode :defer t)

(use-package nasm-mode
  :mode "\\.asm\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :general
  (:keymaps 'json-mode-map
   "M-q" #'json-reformat-region)
  :init
  (csetq json-reformat:indent-width 4)
  (csetq json-reformat:pretty-string? t))

;;
;; Searching
;;

(use-package grep :ensure nil
  :gfhook #'user/results-buffer-hook)

(use-package isearch :ensure nil
  :preface

  (defun isearch-delete-previous ()
    "Delete non-matching text or the last character.
If it's a regexp delete only the last char but only if
the error is \"incomplete input\", or \"trailing backslash\".
That way we don't remove the whole regexp for a simple typo.
\(Eg: for \"search-this-\(strni\" it would have deleted the whole \"strni\"\)."
    (interactive)
    (if (= 0 (length isearch-string))
        (ding)

      (if (or (string-equal isearch-error "incomplete input")
              (isearch-backslash isearch-string))
          (setq isearch-string (substring isearch-string 0 (1- (length isearch-string))))
        (setq isearch-string
              (substring isearch-string
                         0
                         (or (isearch-fail-pos) (1- (length isearch-string))))))

      (setq isearch-message
            (mapconcat #'isearch-text-char-description isearch-string "")))

    (funcall (or isearch-message-function #'isearch-message) nil t)

    (if isearch-other-end (goto-char isearch-other-end))
    (isearch-search)
    (isearch-push-state)
    (isearch-update))

  :general
  (:keymaps 'isearch-mode-map
   "M-o"            #'isearch-occur
   "<tab>"          #'isearch-repeat-forward
   "<backtab>"      #'isearch-repeat-backward
   "<C-backspace>"  #'isearch-delete-previous)
  :init
  (csetq isearch-lazy-count t)
  (csetq isearch-allow-scroll 'unlimited)
  (csetq isearch-yank-on-move 'shift))

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

(use-package rg
  :init
  (csetq rg-ignore-case 'smart)
  (csetq rg-hide-command nil)
  :config
  (rg-enable-default-bindings))

(use-package wgrep
  :defer t
  :init
  (csetq wgrep-auto-save-buffer t))

(use-package wgrep-ag)

;;
;; Programming
;;

(add-hook 'prog-mode-hook #'show-trailing-whitespace)
(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package comment-dwim-2
  :general ("M-;" #'comment-dwim-2))

(use-package counsel-etags :defer t)

(use-package dumb-jump
  :defer t
  :config
  (general-add-hook 'dumb-jump-after-jump-hook #'recenter-top-bottom))

(use-package cc-mode :ensure nil
  :ghook ('c-mode-common-hook #'user/c-mode-common-hook)
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
  :demand t

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

  :general
  (:keymaps 'c-mode-base-map
   "M-o" #'user/ccls-show/body)

  :init
  (csetq ccls-initialization-options '(:diagnostics (:onOpen 0 :opSave 0 :onChange -1 :spellChecking :json-false))))

(use-package python :ensure nil
  :defer t

  :config
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

(use-package pipenv
  :ghook 'python-mode-hook
  :init
  (csetq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package pyvenv
  :ghook (python-mode-hook #'user/auto-virtualenv)
  :preface
  (defun user/auto-virtualenv ()
    (pyvenv-mode t)

    ;; A dolist would be appropriate, but I only use venv as virtualenv name
    ;; This also works with lsp-mode since it will use the python inside
    (let ((root (locate-dominating-file default-directory "venv")))
      (if (and root (file-exists-p root))
          (pyvenv-activate (expand-file-name "venv" root))))))

(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (csetq js2-skip-preprocessor-directives t))

(use-package web-mode
  :gfhook #'turn-off-smartparens-mode
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

  (csetq web-mode-engines-alist '(("django" . "\\.html\\'"))))

;; Define this after all the languages (lsp must be added first in lang-mode-hook)
(use-package lsp-mode
  :commands (lsp lsp-mode)
  :ghook
  ('c-mode-common-hook #'lsp)
  ('python-mode-hook #'lsp)
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

(use-package flymake :ensure nil
  :defer t
  :preface
  (defun flymake-display-at-point ()
    "Display the flymake diagnostic text for the thing at point."
    (interactive)
    (when (and flymake-mode
               (get-char-property (point) 'flymake-diagnostic))
      (let ((text (flymake--diag-text (get-char-property (point) 'flymake-diagnostic))))
        (when text (message "%s" text)))))

  :general
  ("C-c f n" #'flymake-goto-next-error)
  ("C-c f p" #'flymake-goto-prev-error)
  ("C-c f s" #'flymake-start)
  ("C-c f f" #'flymake-display-at-point)
  :init
  (csetq flymake-no-changes-timeout nil)
  (csetq flymake-start-syntax-check-on-newline nil))

(use-package flymake-diagnostic-at-point
  :defer t
  :ghook 'flymake-mode-hook
  :init
  (csetq flymake-diagnostic-at-point-display-diagnostic-function
         #'flymake-diagnostic-at-point-display-popup))

(use-package imenu :ensure nil
  :general ("M-i" #'imenu)
  :ghook ('imenu-after-jump-hook #'recenter-top-bottom)
  :init
  (csetq imenu-auto-rescan t)
  (csetq imenu-auto-rescan-maxout (* 1024 1024)))

(use-package imenu-anywhere
  :general ("M-I" #'imenu-anywhere))

(use-package yasnippet
  :init
  (csetq yas-verbosity 1)
  (csetq yas-triggers-in-field t)
  (csetq yas-wrap-around-region t)
  :config
  (yas-global-mode t)
  (yas-reload-all))


;;
;; Small emacs enhacements
;;

(use-package undo-tree
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

(use-package volatile-highlights
  :init
  (csetq Vhl/highlight-zero-width-ranges t)

  :config
  (volatile-highlights-mode t)

  (vhl/define-extension 'vhl-undo-tree #'undo-tree-move #'undo-tree-undo #'undo-tree-redo #'undo)
  (vhl/install-extension 'vhl-undo-tree))

(use-package visual-fill-column
  :commands (visual-fill-column-mode global-visual-fill-column-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package eyebrowse
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

  :general ("C-c e" #'user/eyebrowse-hydra/body)

  :init
  (csetq eyebrowse-new-workspace t)
  (csetq eyebrowse-switch-back-and-forth t)
  (csetq eyebrowse-wrap-around t)

  :config
  (eyebrowse-mode t))

(use-package projectile
  :demand t
  :general ("C-c p" 'projectile-command-map)
  :init
  (csetq projectile-completion-system 'ivy)
  (csetq projectile-enable-caching t)
  (csetq projectile-indexing-method 'alien)
  (csetq projectile-sort-order 'recentf)
  (csetq projectile-use-git-grep t)

  :config
  (projectile-mode t)

  (add-to-list 'projectile-globally-ignored-directories ".vscode")
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package which-key
  :init
  (csetq which-key-side-window-location 'right)
  (csetq which-key-idle-delay 1)
  (csetq which-key-sort-order 'which-key-prefix-then-key-order)
  ;; (csetq which-key-show-transient-maps t)
  (which-key-mode t))

;;
;; Shell and Terminals
;;

(defun shell-like-mode-hook ()
  (disable-company-mode)
  (setq-local scroll-margin 0))

(use-package eshell :ensure nil
  :defer t
  :gfhook #'shell-like-mode-hook

  :ghook ('eshell-first-time-mode
          (lambda ()
            (add-to-list 'eshell-modules-list 'eshell-rebind)
            (add-to-list 'eshell-modules-list 'eshell-smart)
            (add-to-list 'eshell-modules-list 'eshell-xtra)))
  :init
  (csetq eshell-hist-ignoredups t)
  (csetq eshell-history-size 50000)
  (csetq eshell-ls-dired-initial-args (quote ("-h")))
  (csetq eshell-ls-exclude-regexp "~\\'")
  (csetq eshell-ls-initial-args "-hA")
  (csetq eshell-stringify-t nil))

(use-package esh-module :ensure nil
  :defer t)

(use-package multi-term
  :if (eq system-type 'gnu/linux)
  :general
  ("C-z" #'multi-term-next)
  ("C-c z c" #'multi-term)
  ("C-c z d" #'multi-term-dedicated-toggle)
  ("C-c z n" #'multi-term-next)
  ("C-c z p" #'multi-term-prev)
  :gfhook #'shell-like-mode-hook
  :init
  ;; (csetq multi-term-program "screen")
  ;; (csetq multi-term-program-switches "-DR")
  (csetq multi-term-dedicated-select-after-open-p t)
  (csetq multi-term-scroll-show-maximum-output t))

;;
;; Mail
;;

(use-package gnus :ensure nil
  :commands gnus
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

  :general ;; ("C-x m" . #'gnus)
  (:keymaps 'gnus-group-mode-map
   "y" #'user/hydra-gnus-group/body
   "o" #'gnus-group-list-all-groups)
  (:keymaps 'gnus-summary-mode-map
   "y" #'user/hydra-gnus-summary/body
   ">" #'gnus-summary-show-thread
   "<" #'gnus-summary-hide-thread)
  (:keymaps 'gnus-article-mode-map
   "y" #'user/hydra-gnus-article/body)

  :init
  (csetq gnus-init-file (expand-file-name "gnus.el" user-emacs-directory)))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :defer t
  ;; :general (("C-x m" . mu4e))
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

;;
;; Debugging
;;

(use-package gud :ensure nil
  :gfhook #'disable-company-mode
  :init
  (csetq gdb-many-windows t))

(use-package realgud
  :commands (realgud:bashdb realgud:gdb realgud:gub realgud:ipdb
             realgud:jdb realgud:kshdb realgud:nodejs realgud:pdb
             realgud:perldb realgud:zshdb))

;;
;; Version Control
;;

(use-package magit
  :general ("C-x g" #'magit-status)
  :ghook ('git-commit-mode-hook #'git-commit-turn-on-flyspell)
  :init
  (csetq magit-diff-arguments
         '("--ignore-space-change" "--ignore-all-space"
           "--no-ext-diff" "--stat" "--diff-algorithm=histogram"))
  (csetq magit-diff-refine-hunk t)
  (csetq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (csetq magit-process-popup-time 20)
  (csetq magit-refs-show-commit-count 'all))

(use-package magit-gitflow
  :ghook ('magit-mode-hook #'turn-on-magit-gitflow))

;;
;; System replacements
;;

(use-package daemons
  :commands (daemons daemons-start daemons-stop daemons-status)
  :init
  (csetq daemons-always-sudo t))

(use-package disk-usage
  :commands disk-usage)

(use-package docker
  :commands docker)

(use-package rmsbolt
  :commands rmsbolt-starter)


;;
;; Misc (user-defined)
;;

(use-package user-advices :load-path "lisp")

(use-package user-utils
  :load-path "lisp"
  :general
  ("M-j" #'user/join-line)
  ("<C-return>" #'user/open-line-above)
  ("C-a" #'user/move-beginning-of-line)
  ("C-w" #'user/kill-word-or-region)
  ([remap backward-kill-word] #'user/backward-kill-word)
  ("M-]" #'user/next-error)
  ("M-[" #'user/prev-error)
  ;; ([remap forward-paragraph] . #'user/forward-paragraph)
  ;; ([remap backward-paragraph] . #'user/backward-paragraph)
  ("C-`" #'user/open-terminal)
  ([remap scroll-up-command] #'user/scroll-half-page-up)
  ([remap scroll-down-command] #'user/scroll-half-page-down)
  ("C-'" #'push-mark-no-activate)
  ("M-'" #'jump-to-mark))

(provide 'init)

;;; init.el ends here

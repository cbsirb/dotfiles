;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; It's init.el, that says enough.

;;; Code:

;; Easier customization
(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defvar user/gc-cons-threshold (* 16 gc-cons-threshold))
(defvar user/file-name-handler-alist file-name-handler-alist)

(defun user/hide-load-messages (orig-fn file &optional noerror nomessage nosuffix must-suffix)
  (apply orig-fn file (list noerror t nosuffix must-suffix)))

(advice-add #'load :around #'user/hide-load-messages)

(add-hook 'after-init-hook
          (lambda ()
            (csetq gc-cons-threshold user/gc-cons-threshold)

            ;; Don't just blindly set it to the old value, maybe someone decided to add something to it
            (csetq file-name-handler-alist (append file-name-handler-alist user/file-name-handler-alist))

            (advice-remove #'load #'user/hide-load-messages)

            (message "Time to load init file: %s" (emacs-init-time))
            (garbage-collect)))

(csetq gc-cons-threshold most-positive-fixnum)
(csetq file-name-handler-alist nil)
(csetq load-prefer-newer t)
(csetq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(dolist (dir (directory-files (expand-file-name "site-lisp" user-emacs-directory)))
  (when (and (not (string-suffix-p "." dir))
             (file-directory-p dir))
    (add-to-list 'load-path (expand-file-name dir (expand-file-name "site-lisp" user-emacs-directory)))))

;; Install use-package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(csetq use-package-enable-imenu-support t)
(csetq use-package-expand-minimally t)
(csetq use-package-always-ensure t)
(csetq use-package-compute-statistics nil)

(defun package-update-now ()
  "Update all the packages."
  (interactive)

  (split-window-below -10)
  (other-window 1)

  (package-list-packages-no-fetch)
  (package-refresh-contents)
  (package-menu-mark-upgrades)

  (with-demoted-errors "Nothing to update! (%S)"
    (package-menu-execute))

  (when-let* ((buf (get-buffer "*Packages*")))
    (switch-to-buffer buf)
    (kill-buffer-and-window)))

(defun package-rebuild-all ()
  "Rebuild all the packages."
  (interactive)

  (byte-recompile-directory package-user-dir nil 'force))

;;
;; Packages needed no matter what, and usually others are depended on it
;;
(eval-when-compile
  (require 'use-package))

(use-package general)

(use-package modus-operandi-theme
  :if (display-graphic-p)
  :custom
  (modus-operandi-theme-visible-fringes nil)
  (modus-operandi-theme-bold-constructs t)
  (modus-operandi-theme-slanted-constructs nil)
  (modus-operandi-theme-3d-modeline nil)
  (modus-operandi-theme-proportional-fonts nil)
  :config
  (load-theme 'modus-operandi t))

(use-package modus-vivendi-theme
  :disabled
  :if (display-graphic-p)
  :config
  (load-theme 'modus-vivendi t))

(use-package minions
  :custom
  (minions-mode-line-delimiters '("" . ""))
  :config
  (push 'flymake-mode minions-direct)
  (push 'overwrite-mode minions-direct)
  (minions-mode t))

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package ignoramus
  :config
  (ignoramus-setup))

(use-package no-littering)

;; For now disabled, since I don't have any modifications to the $PATH
(use-package exec-path-from-shell
  :disabled
  :config
  (exec-path-from-shell-initialize))

(use-package hydra
  :defer t
  :config
  (hydra-add-font-lock))

;; TBD
;; (use-package frog-menu)

;;
;; Some default settings that I like
;;

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)

;; startup.el
(csetq initial-major-mode 'fundamental-mode)
(csetq initial-scratch-message "")
(csetq inhibit-startup-buffer-menu t)
(csetq inhibit-splash-screen t)
(csetq inhibit-startup-echo-area-message t)
(csetq inhibit-startup-message t)
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; mule.el
(csetq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(when (display-graphic-p)
  (csetq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; files.el
(csetq enable-local-variables :safe)
(csetq backup-by-copying t)
(csetq view-read-only t)
(csetq delete-old-versions t)
(csetq make-backup-files nil)
(csetq auto-save-default nil)
(csetq large-file-warning-threshold (* 50 1024 1024))
(csetq directory-free-space-args "-kh")
(csetq save-silently t)
(csetq require-final-newline t)
(file-name-shadow-mode t)

;; C
(csetq completion-ignore-case t)
(csetq system-time-locale "C")
(csetq auto-window-vscroll nil)
(csetq use-dialog-box nil)
(csetq minibuffer-electric-default-mode t)
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
(csetq undo-limit (* 10 undo-limit))
(csetq undo-strong-limit (* 10 undo-strong-limit))
(csetq inhibit-compacting-font-caches t)
(csetq frame-resize-pixelwise t)
(csetq window-combination-resize t)
(csetq frame-title-format
       '(:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")))
(csetq indicate-buffer-boundaries
       '((top . right)
         (bottom . right)
         (t . nil)))

(csetq display-line-numbers-grow-only t)
(csetq display-line-numbers-width-start t)

(csetq command-line-ns-option-alist nil)

;; misc stuff
(csetq read-file-name-completion-ignore-case t)
(csetq bookmark-save-flag t)
(csetq column-number-indicator-zero-based nil)
(csetq disabled-command-function nil)
(csetq sentence-end-double-space nil)
(csetq colon-double-space nil)
(csetq ad-redefinition-action 'accept)
(csetq woman-use-topic-at-point-default t)
(csetq apropos-do-all t)
(csetq idle-update-delay 1)
(csetq custom-buffer-done-kill t)

;; "Smooth" mouse scrolling, one line at a time
(csetq mouse-wheel-scroll-amount
       '(1
         ((shift) . 5)
         ((meta) . 0.5)
         ((control) . text-scale)))
(csetq scroll-conservatively 101)
(csetq scroll-preserve-screen-position t)
(csetq fast-but-imprecise-scrolling t)

(when (eq system-type 'gnu/linux)
  (csetq x-underline-at-descent-line t)
  (csetq x-stretch-cursor t)
  (csetq x-wait-for-event-timeout nil))

(csetq read-process-output-max (* read-process-output-max 8))

(csetq tooltip-resize-echo-area t)
(csetq tooltip-delay 0.5)
(csetq x-gtk-use-system-tooltips nil)

(csetq tramp-default-method "ssh")
(csetq tramp-verbose 2)

(csetq uniquify-buffer-name-style 'post-forward-angle-brackets)
(csetq uniquify-after-kill-buffer-p t)

;; simple.el
(csetq mark-ring-max 128)
(csetq global-mark-ring-max 256)
(csetq save-interprogram-paste-before-kill t)
(csetq kill-ring-max 128)
(csetq kill-do-not-save-duplicates t)
(csetq read-quoted-char-radix 16)
(csetq eval-expression-print-length nil)
(csetq eval-expression-print-level nil)
(csetq async-shell-command-display-buffer nil)
(csetq async-shell-command-buffer 'new-buffer)
(csetq shell-command-prompt-show-cwd t)
(csetq set-mark-command-repeat-pop t)

(transient-mark-mode t)
(auto-save-mode -1)
(delete-selection-mode t)
(size-indication-mode -1)
(line-number-mode t)
(column-number-mode t)

(winner-mode t)
(minibuffer-depth-indicate-mode t)
(blink-cursor-mode -1)
(save-place-mode t)

(use-package user-auto-revert
  :load-path "lisp")

(add-hook 'focus-out-hook #'garbage-collect)

;;
;; Mode-line
;;
(csetq mode-line-buffer-identification
       '(:eval (format-mode-line
                (propertized-buffer-identification
                 (or (when-let* ((buffer-file-truename buffer-file-truename)
                                 (prj (cdr-safe (project-current)))
                                 (prj-parent (directory-file-name (expand-file-name prj))))
                      (concat (file-relative-name (file-name-directory buffer-file-truename) prj-parent) (file-name-nondirectory buffer-file-truename)))
                  "%b")))))

(defun mode-line-vc ()
  "Will returing the same thing as variable `vc-mode', but with a hard-coded max length."
  (if vc-mode
      (substring vc-mode 0 (min 30 (length vc-mode)))
    ""))

(csetq mode-line-format
       '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
         (:eval (mode-line-vc))
         "  " minions-mode-line-modes mode-line-misc-info mode-line-end-spaces))

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
(csetq fill-column 80)

(add-hook 'text-mode-hook #'auto-fill-mode)

;; Some special file names
(add-to-list 'auto-mode-alist '("\\.?bash.*" . shell-script-mode))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; (csetq display-buffer-alist
;;        `((,(rx string-start
;;                (or "*Compile-Log*"
;;                    "*Warnings*"
;;                    "*compilation"
;;                    "*rg*"
;;                    "*grep*"
;;                    "*Occur*"
;;                    "*xref*"
;;                    "*Flymake diagnostics"
;;                    "*Flycheck"
;;                    "*ivy-"
;;                    ))
;;           (display-buffer-reuse-window
;;            display-buffer-in-side-window)
;;           (side            . bottom)
;;           (reusable-frames . nil)
;;           (window-height   . 0.25))

;;          ;; Show buffer only in the selected frame.
;;          ("." nil (reusable-frames . nil))))

;; Its value is
;; (("\\`\\*compilation"
;;   (display-buffer-reuse-window display-buffer-in-side-window)
;;   (side . bottom)
;;   (reusable-frames)
;;   (window-height . 0.25)))

(push `(,(rx string-start "*compilation")
        (display-buffer-reuse-window
         display-buffer-in-side-window)
        (side            . bottom)
        (reusable-frames . nil)
        (window-height   . 0.25))
      display-buffer-alist)

;;
;; Customization that doesn't require use-package
;;

;; When creating new buffers, use `auto-mode-alist' to automatically set the major mode.
(csetq major-mode (lambda ()
                    (unless buffer-file-name
                      (let ((buffer-file-name (buffer-name)))
                        (set-auto-mode)))))

(use-package diff :ensure nil
  :commands diff-mode
  :gfhook #'diff-delete-empty-files #'diff-make-unified #'smerge-mode
  :custom
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax 'hunk-also)
  (diff-switches '("-u" "-p" "-w")))

(csetq ediff-diff-options "-w")
(csetq ediff-highlight-all-diffs nil)
(csetq ediff-show-clashes-only t)
(csetq ediff-split-window-function #'split-window-horizontally)
(csetq ediff-window-setup-function #'ediff-setup-windows-plain)

(csetq vc-follow-symlinks t)
(csetq vc-git-diff-switches '("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat" "--diff-algorithm=histogram"))
(csetq vc-git-print-log-follow t)

(csetq tab-bar-close-button-show nil)
(csetq tab-bar-show 1)

(use-package savehist :ensure nil
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring compile-command calc-stack))
  (savehist-ignored-variables '(tmm--history yes-or-no-p-history))
  (savehist-autosave-interval 60)
  (savehist-save-minibuffer-history t)
  (history-length 3000)
  (history-delete-duplicates t)
  :config
  (savehist-mode t))

(csetq hexl-bits 8)

(csetq dabbrev-case-replace nil)
(csetq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*$]")

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-list
        try-expand-list-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs))

(csetq ispell-program-name (executable-find "aspell"))
(csetq ispell-extra-args '("--sug-mode=normal" "--keyboard=standard"))

(csetq flyspell-issue-welcome-flag nil)
(csetq flyspell-issue-message-flag nil)
(csetq flyspell-use-meta-tab nil)

(defun hide-trailing-whitespace ()
  "Used for hooks to various modes."
  (setq-local show-trailing-whitespace nil))

(defun user/garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

(defun user/minibuffer-setup-hook ()
  "Hook to run when entering the minibuffer."
  (csetq gc-cons-threshold most-positive-fixnum))

(defun user/minibuffer-exit-hook ()
  "Hook to run when exiting the minibuffer."
  (csetq gc-cons-threshold user/gc-cons-threshold))

;; Increase the memory while in the minibuffer
(add-hook 'minibuffer-setup-hook #'user/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'user/minibuffer-exit-hook)

(with-eval-after-load 'xref
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t)
  (add-hook 'xref-after-return-hook #'recenter))

;; Thank you u/ouroboroslisp
(defun user/calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists.
PARSE-START indicates where the parsing should start in the file (point)."
  ;; This line because `calculate-lisp-indent-last-sexp` was defined with `defvar`
  ;; with it's value ommited, marking it special and only defining it locally. So
  ;; if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line
                      ;; except the first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      ;; First sexp after `containing-sexp' is a keyword. This
                      ;; condition is more debatable. It's so that I can have
                      ;; unquoted plists in macros. It assumes that you won't
                      ;; make a function whose name is a keyword.
                      ;; (when-let (char-after (char-after (1+ containing-sexp)))
                      ;;   (char-equal char-after ?:))

                      ;; Check for quotes or backquotes around.
                      (let* ((positions (elt state 9))
                             (last (car (last positions)))
                             (rest (reverse (butlast positions)))
                             (any-quoted-p nil)
                             (point nil))
                        (or
                         (when-let (char (char-before last))
                           (or (char-equal char ?')
                               (char-equal char ?`)))
                         (progn
                           (while (and rest (not any-quoted-p))
                             (setq point (pop rest))
                             (setq any-quoted-p
                                   (or
                                    (when-let (char (char-before point))
                                      (or (char-equal char ?')
                                          (char-equal char ?`)))
                                    (save-excursion
                                      (goto-char (1+ point))
                                      (looking-at-p
                                       "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                           any-quoted-p))))
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(advice-add #'calculate-lisp-indent :override #'user/calculate-lisp-indent)

(defun occur-at-point ()
  "Just like `occur', but with the default value of symbol at point."
  (interactive)
  (let ((read-regexp-defaults-function 'find-tag-default-as-symbol-regexp))
    (call-interactively #'occur)))

;;
;; Some binds & configs that doesn't require use-package (well, we could require simple & misc, but they will be autloaded anyway)
;;

(general-unbind
  "<f2>"
  "M-o"
  "C-x C-z"
  "C-x f"
  "C-x >"
  "C-x <"
  "<C-next>"
  "<C-prior>")

(use-package user-utils
  :load-path "lisp"
  :general
  ("M-`" #'user/open-terminal)
  ("M-]" #'user/next-error)
  ("M-[" #'user/prev-error)
  ("M-j" #'user/join-line)
  ([remap backward-kill-word] #'user/backward-kill-word)
  ([remap scroll-up-command] #'user/scroll-half-page-up)
  ([remap scroll-down-command] #'user/scroll-half-page-down)
  ([remap split-window-below] #'user/split-window-below)
  ([remap split-window-right] #'user/split-window-right)
  ("C-w" #'user/kill-word-or-region)
  ("<C-return>" #'user/open-line-above)
  ("C-a" #'user/move-beginning-of-line)
  ("C-e" #'move-end-of-line))

(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         ;; if we got this far just use the default so we don't miss
         ;; any upstream changes
         (keyboard-quit))))

(general-define-key
 :keymaps 'completion-list-mode-map
 "C-n" #'next-completion
 "C-p" #'previous-completion
 "n" #'next-completion
 "p" #'previous-completion
 "s" #'isearch-forward
 "r" #'isearch-backward)

(general-define-key "M-u" #'upcase-dwim)
(general-define-key "M-l" #'downcase-dwim)
(general-define-key "M-c" #'capitalize-dwim)
(general-define-key "C-8" #'repeat-complex-command)
(general-define-key "M-z" #'zap-up-to-char)
(general-define-key "<C-right>" #'forward-to-word)
(general-define-key "C-d" #'delete-forward-char)
(general-define-key [remap just-one-space] #'cycle-spacing)
(general-define-key [remap newline] #'newline-and-indent)
(general-define-key [remap isearch-forward] #'isearch-forward-regexp)
(general-define-key [remap isearch-forward-regexp] #'isearch-forward)
(general-define-key [remap isearch-backward] #'isearch-backward-regexp)
(general-define-key [remap isearch-backward-regexp] #'isearch-backward)
(general-define-key [remap keyboard-quit] #'keyboard-quit-context+)
(general-define-key [remap minibuffer-keyboard-quit] #'keyboard-quit-context+)

(general-define-key
 :prefix "M-s"
 "o" #'occur-at-point)

(when (display-graphic-p)
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-\M-m] [C-M-m]))

(general-define-key
 :prefix "C-c t"
 "d" #'toggle-debug-on-error
 "q" #'toggle-debug-on-quit)

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

  :custom-face
  (whitespace-tab ((t (:background nil :foreground "light gray"))))

  :custom
  (whitespace-display-mappings
   '((tab-mark ?\t [187 ?\t])))
  (whitespace-style '(face tabs tab-mark trailing)))

(use-package recentf :ensure nil
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-exclude (list
                    "/elpa/.*\\'"
                    "/.ccls-cache/.*\\'"
                    "/.clangd/.*\\'"
                    "PKGBUILD"
                    "/usr/.*\\'"
                    "/tmp/.*\\'"
                    #'ignoramus-boring-p))
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 100)

  :config
  (when source-directory
    (push (concat source-directory ".*\\'") recentf-exclude))
  (push no-littering-var-directory recentf-exclude)
  (push no-littering-etc-directory recentf-exclude)

  (recentf-mode t)
  (run-at-time (* 1 60) (* 1 60) #'recentf-save-list))

(use-package ibuffer :ensure nil
  :preface
  (defun ibuffer-switch-to-filter ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  :gfhook #'ibuffer-auto-mode #'ibuffer-switch-to-filter #'hl-line-mode
  :general
  ("C-x C-b" #'ibuffer)
  :custom
  (ibuffer-saved-filter-groups
   '(("default"

      ("X" (mode . exwm-mode))

      ("Dired" (mode . dired-mode))

      ;; Need to be before "Programming" otherwise
      ;; `emacs-lisp-mode' will match.
      ("Emacs config" (filename . ".emacs.d"))

      ("Org-Mode" (mode . org-mode))

      ("Programming" (derived-mode . prog-mode))

      ("Magit" (name . "magit"))

      ("Processes" (process))

      ("Special" (or (name . "\*Messages\*")
                  (name . "\*scratch\*")))

      ("Virtual" (name . "\*")))))
  (ibuffer-display-summary nil)
  (ibuffer-default-shrink-to-minimum-size t)
  (ibuffer-show-empty-filter-groups nil))

(use-package comint :ensure nil
  :general
  (:keymaps 'comint-mode-map
            "<down>" #'comint-next-input
            "<up>"   #'comint-previous-input
            "C-n"    #'comint-next-input
            "C-p"    #'comint-previous-input
            "C-r"    #'comint-history-isearch-backward)
  :ghook ('comint-output-filter-functions #'comint-strip-ctrl-m)
  :custom
  (comint-process-echoes t)
  (comint-prompt-read-only t)
  (comint-history-isearch t)
  (comint-ignore-dups t))

(use-package compile :ensure nil
  :preface
  (defun compile-without-ask (ask)
    (interactive "P")
    (if ask
        (call-interactively #'compile)
      (compile compile-command)))

  (defun user/switch-to-compilation-window (buffer _msg)
    (let ((bufwin (get-buffer-window buffer)))
      (when bufwin
        (select-window bufwin)
        (local-set-key (kbd "q") 'kill-buffer-and-window))))

  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  :general
  ([remap comment-region] #'compile-without-ask)
  ("C-c c" #'compile-without-ask)

  :ghook
  ('compilation-mode-hook #'hide-trailing-whitespace)
  ('compilation-finish-functions #'user/switch-to-compilation-window)
  ('compilation-filter-hook 'colorize-compilation-buffer)

  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-auto-jump-to-first-error nil)
  (compilation-context-lines nil)
  (compilation-disable-input t)
  (compilation-scroll-output 'first-error))

(use-package dired-x :ensure nil
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

  :general
  (:keymaps 'dired-mode-map
            "SPC" #'dired-mark
            "<C-return>" #'user/open-in-external-app
            "<tab>" #'user/dired-next-window)

  :custom
  (dired-x-hands-off-my-keys t)
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-information-lines nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-lFaGh1v --group-directories-first")
  (dired-ls-F-marks-symlinks t)
  (dired-recursive-copies 'always)
  (dired-omit-verbose nil))

(use-package wdired :ensure nil
  :general
  (:keymaps 'dired-mode-map
            "C-c M-w" #'wdired-change-to-wdired-mode)
  :custom
  (wdired-create-parent-directories t)
  (wdired-allow-to-change-permissions t))

(use-package dired-du
  :commands dired-du-mode
  :custom
  (dired-du-size-format t)
  (dired-du-update-headers t))

(use-package dired-narrow
  :commands dired-narrow
  :general
  (:keymaps 'dired-mode-map
            "/" #'dired-narrow)
  :custom
  (dired-narrow-exit-when-one-left t))

(use-package diredfl
  :defer t
  :ghook ('dired-mode-hook #'diredfl-mode))

(use-package dired-git-info
  :general
  (:keymaps 'dired-mode-map
            ")" #'dired-git-info-mode))

;;
;; Editing & navigation
;;

(use-package expand-region
  :general
  ("M-2" #'er/expand-region)
  ("M-1" #'er/contract-region)
  :custom
  (expand-region-fast-keys-enabled nil)
  (expand-region-autocopy-register "e"))

(use-package symbol-overlay
  :preface
  (define-global-minor-mode global-symbol-overlay-mode symbol-overlay-mode
    (lambda () (symbol-overlay-mode t)))
  :general
  ("M-*" #'symbol-overlay-put)
  ("M-n" #'symbol-overlay-jump-next)
  ("M-p" #'symbol-overlay-jump-prev)
  ("M-8" #'symbol-overlay-toggle-in-scope)
  :init
  (global-symbol-overlay-mode t)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit underline))))
  :custom
  (symbol-overlay-idle-time 0.25)
  (symbol-overlay-displayed-window nil))

(use-package beginend
  :config
  (beginend-global-mode t))

(use-package iedit
  :preface
  (defun iedit-scoped (orig-fn)
    "Call `iedit-mode' with function-local scope when in
 prog-mode derived mode, or global scope if called with a
 universal prefix."
    (interactive)
    (pcase-exhaustive current-prefix-arg
      ('nil (if iedit-mode
                (funcall orig-fn)
              (if (derived-mode-p 'prog-mode)
                  (funcall orig-fn '(0))
                (funcall orig-fn))))
      ('(4) (funcall orig-fn))))
  :general
  ("C-;" #'iedit-mode)
  :config
  (advice-add #'iedit-mode :around #'iedit-scoped))

(use-package iy-go-to-char
  :general
  ("M-m" #'iy-go-to-char)
  ("M-M" #'iy-go-to-char-backward))

(use-package multiple-cursors
  :general
  ("C-S-<mouse-1>" #'mc/toggle-cursor-on-click)

  ("C->"         #'mc/unmark-next-like-this)
  ("C-<"         #'mc/unmark-previous-like-this)
  ("C-."         #'mc/mark-next-like-this)
  ("C-,"         #'mc/mark-previous-like-this)
  (:prefix "C-c m"
           "^"     #'mc/edit-beginnings-of-lines
           "`"     #'mc/edit-beginnings-of-lines
           "$"     #'mc/edit-ends-of-lines
           "'"     #'mc/edit-ends-of-lines
           "R"     #'mc/reverse-regions
           "S"     #'mc/sort-regions
           "W"     #'mc/mark-all-words-like-this
           "Y"     #'mc/mark-all-symbols-like-this
           "a"     #'mc/mark-all-like-this-dwim
           "c"     #'mc/mark-all-dwim
           "l"     #'mc/insert-letters
           "n"     #'mc/insert-numbers
           "r"     #'mc/mark-all-in-region
           "s"     #'set-rectangular-region-anchor
           "%"     #'mc/mark-all-in-region-regexp
           "t"     #'mc/mark-sgml-tag-pair
           "w"     #'mc/mark-next-like-this-word
           "x"     #'mc/mark-more-like-this-extended
           "y"     #'mc/mark-next-like-this-symbol
           "C-SPC" #'mc/mark-pop
           "("     #'mc/mark-all-symbols-like-this-in-defun
           "C-("   #'mc/mark-all-words-like-this-in-defun
           "M-("   #'mc/mark-all-like-this-in-defun
           "d"     #'mc/mark-all-symbols-like-this-in-defun
           "C-d"   #'mc/mark-all-words-like-this-in-defun
           "M-d"   #'mc/mark-all-like-this-in-defun
           "["     #'mc/vertical-align-with-space
           "{"     #'mc/vertical-align)
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

(use-package smex)

(use-package ivy
  :general
  ("C-c C-r" #'ivy-resume)
  (:keymaps 'ivy-mode-map
            "<escape>" #'keyboard-quit-context+)
  :custom
  (ivy-count-format "")
  (ivy-height 9)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  (ivy-wrap t)
  :init
  (ivy-mode t))

(use-package counsel
  :general
  (:prefix "M-s"
           "c" #'counsel-rg
           "g" #'counsel-git-grep
           "s" #'swiper)
  :preface
  ;; http://xenodium.com/emacss-counsel-m-x-meets-multiple-cursors/index.html
  (defun user/counsel-M-x-action (orig-fun &rest r)
    "Additional support for multiple cursors."
    (apply orig-fun r)
    (let ((cmd (intern (string-trim-left (nth 0 r) "\\^"))))
      (when (and (boundp 'multiple-cursors-mode)
                 multiple-cursors-mode
                 cmd
                 (not (memq cmd mc--default-cmds-to-run-once))
                 (not (memq cmd mc/cmds-to-run-once))
                 (or mc/always-run-for-all
                     (memq cmd mc--default-cmds-to-run-for-all)
                     (memq cmd mc/cmds-to-run-for-all)
                     (mc/prompt-for-inclusion-in-whitelist cmd)))
        (mc/execute-command-for-all-fake-cursors cmd))))
  :custom
  (counsel-describe-function-preselect 'ivy-function-called-at-point)
  (counsel-grep-post-action-hook '(recenter))
  (counsel-mode-override-describe-bindings t)
  :init
  (counsel-mode t)
  (advice-add #'counsel-M-x-action :around #'user/counsel-M-x-action))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode t))

(use-package ivy-posframe
  :after ivy
  :custom
  (ivy-posframe-display-functions-alist
   '((swiper . nil)
     (t . ivy-posframe-display-at-point)))
  (ivy-posframe-parameters '((internal-border-width . 3)))
  :config
  (ivy-posframe-mode t))

(use-package company
  :general
  (:keymaps 'company-active-map
            "ESC" #'company-abort
            "<tab>" #'company-complete-selection
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "C-w" nil)
  :custom
  (company-global-modes '(not term-mode gud-mode shell-mode))
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above nil)
  ;; (company-occurrence-weight-function #'company-occurrence-prefer-any-closest)
  ;; (company-transformers '(company-sort-by-occurrence))
  :init
  (global-company-mode t))

(use-package company-posframe
  :after company
  :custom
  (company-posframe-quickhelp-delay nil)
  (company-posframe-show-indicator nil)
  (company-posframe-show-metadata nil)
  :config
  (company-posframe-mode t))

(use-package eacl
  :general ("C-x C-l" #'eacl-complete-line))

;;
;; Extra modes
;;

(use-package cmake-mode :defer t)

(use-package yaml-mode :defer t)

(use-package cmake-font-lock
  :ghook ('cmake-mode-hook #'cmake-font-lock-activate))

(use-package cython-mode :defer t)

(use-package nasm-mode
  :mode "\\.asm\\'")

(use-package log4j-mode
  :mode "\\.log\\'"
  :defer t)

(use-package json-mode
  :mode "\\.json\\'"
  :general
  (:keymaps 'json-mode-map
            "M-q" #'json-reformat-region)
  :custom
  (json-reformat:indent-width 4)
  (json-reformat:pretty-string? t))

;;
;; Searching
;;

(add-hook 'grep-mode-hook #'hide-trailing-whitespace)

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

  :custom
  (isearch-lazy-count t)
  (lazy-highlight-initial-delay 0)
  (isearch-allow-scroll 'unlimited)
  (isearch-yank-on-move 'shift))

(push ".ccls-cache" grep-find-ignored-directories)
(push ".vscode" grep-find-ignored-directories)
(push ".clangd" grep-find-ignored-directories)

(use-package rg
  :general
  (:prefix "M-s"
           "d" #'rg-dwim
           "r" #'rg
           "p" #'rg-project
           "l" #'rg-literal)
  :custom
  (rg-custom-type-aliases nil)
  (rg-ignore-case 'smart)
  (rg-hide-command nil))

(use-package wgrep
  :defer t
  :custom
  (wgrep-auto-save-buffer t))

;;
;; Programming
;;
(use-package prog-mode :ensure nil
  :defer t
  :gfhook
  #'hide-trailing-whitespace
  ;; #'which-function-mode
  #'hs-minor-mode
  #'electric-indent-local-mode
  #'electric-pair-local-mode)

(defun validate-balance ()
  "Check for unbalanced parentheses in the current buffer.
More accurately, check the narrowed part of the buffer for unbalanced
expressions (\"sexps\") in general.  This is done according to the
current syntax table and will find unbalanced brackets or quotes as
appropriate.  (See Info node `(emacs)Parentheses'.)  If imbalance is
found, an error is signaled."
  (interactive)
  (condition-case data
      ;; Buffer can't have more than (point-max) sexps.
      (scan-sexps (point-min) (point-max))
    (scan-error
     ;; Could print (nth 1 data), which is either
     ;; "Containing expression ends prematurely" or
     ;; "Unbalanced parentheses", but those may not be so
     ;; accurate/helpful, e.g. quotes may actually be
     ;; mismatched.
     (user-error
      "Unmatched bracket or quote at line %d"
      (line-number-at-pos (nth 2 data))))))

(add-hook 'after-save-hook #'validate-balance)

(use-package paren :ensure nil
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :ghook ('after-init-hook #'show-paren-mode t))

(use-package string-inflection)
;; :general
;; (:keymaps 'prog-mode-map
;;           "C-M-j" #'string-inflection-all-cycle))

(use-package comment-dwim-2
  :general ("M-;" #'comment-dwim-2))

(use-package counsel-etags :defer t)

(use-package dumb-jump
  :defer t
  :ghook ('dumb-jump-after-jump-hook #'recenter-top-bottom))

(use-package cc-mode :ensure nil
  :general
  (:keymaps 'c-mode-base-map
            "C-=" #'user/c-mode-toggle-funcall
            [remap newline] #'c-context-line-break
            [remap open-line] #'c-context-open-line)
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
    ;; (c-toggle-hungry-state t)
    ;; (c-toggle-auto-newline -1)
    (c-toggle-syntactic-indentation t)

    (modify-syntax-entry ?' ".")

    (with-eval-after-load 'lsp-mode
      (setq-local flymake-diagnostic-functions (remove #'flymake-cc flymake-diagnostic-functions))))

  (defun user/c-mode-toggle-funcall ()
    "Transpose multi-line call to one-line and vice-versa."
    (interactive)
    (let ((initial-pos (point))
          (bosl) (eosl)
          (bos) (eos))
      (save-excursion
        (c-end-of-statement)
        (setq eos (point))
        (setq eosl (line-number-at-pos))

        (c-beginning-of-statement 1)
        (setq bos (point))
        (setq bosl (line-number-at-pos))

        (save-restriction
          (condition-case nil
              (progn
                (narrow-to-region bos eos)
                (search-forward "(" eos t)
                (while (< (point) (point-max))
                  (if (= eosl bosl)
                      (progn
                        (re-search-forward "(\\|,\\|)\\|\"")
                        (cl-case (char-before)
                          (?\( (progn (backward-char) (forward-sexp)))
                          (?\" (progn (backward-char) (forward-sexp)))
                          (?, (newline-and-indent))))
                    (re-search-forward "\n")
                    (when (/= (point) (point-max))
                      (backward-char)
                      (join-line -1)))))
            (error nil))))))

  :ghook ('c-mode-common-hook #'user/c-mode-common-hook)

  :custom
  (c-tab-always-indent nil)
  (c-backspace-function #'backward-delete-char)
  (c-hanging-semi&comma-criteria nil)

  :config
  (c-add-style "allman" user/allman-style)
  (c-add-style "sane-k&r" user/k&r-style)

  (csetq c-default-style '((java-mode . "java")
                           (awk-mode . "awk")
                           (other . "sane-k&r"))))

(use-package modern-cpp-font-lock
  :after cc-mode
  :ghook ('c++-mode-hook #'modern-c++-font-lock-mode))

(use-package ccls
  :load-path "site-lisp/ccls" ;; ccls requires projectile (and doesn't use it at all)
  :after cc-mode
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

  :custom
  (ccls-sem-highlight-method 'font-lock)
  (ccls-initialization-options
   '(:diagnostics (:onOpen 0 :onSave 0 :onChange -1 :spellChecking :json-false)
     :highlight (:largeFileSize 0)))
  :config
  (push "compile_commands.json" ccls-root-files))

(use-package python :ensure nil
  :commands python-mode

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

(use-package pyvenv
  :preface
  (defun user/auto-virtualenv ()
    (pyvenv-mode t)

    ;; A dolist would be appropriate, but I only use venv as virtualenv name
    ;; This also works with lsp-mode since it will use the python inside
    (let ((root (locate-dominating-file default-directory "venv")))
      (if (and root (file-exists-p root))
          (pyvenv-activate (expand-file-name "venv" root)))))

  :ghook ('python-mode-hook #'user/auto-virtualenv))

(use-package haskell-mode
  :defer t)

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js2-skip-preprocessor-directives t))

(use-package web-mode
  ;; :gfhook #'turn-off-smartparens-mode
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
  :custom
  (web-mode-code-indent-offset 4)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-sql-indent-offset 4)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)

  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-element-content-fontification t)
  (web-mode-enable-element-tag-fontification t)
  (web-mode-enable-html-entities-fontification t)
  (web-mode-enable-inlays t)
  (web-mode-enable-sql-detection t)
  (web-mode-enable-block-face t)
  (web-mode-enable-part-face t)

  (web-mode-engines-alist '(("django" . "\\.html\\'"))))

;; Define this after all the languages (lsp must be added first in lang-mode-hook)
(use-package lsp-mode
  :commands (lsp lsp-mode)

  :ghook
  ('c-mode-common-hook #'lsp t)
  ('python-mode-hook #'lsp t)
  ('lsp-after-open-hook #'lsp-enable-imenu)

  :custom
  ;; performance reasons (and they're not really usefull for me)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-before-save-edits nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-auto-guess-root nil)
  (lsp-restart 'auto-restart)
  (lsp-pyls-plugins-rope-completion-enabled nil "This is very very slow (allow only jedi completions)")
  (lsp-file-watch-threshold 9000)
  (lsp-eldoc-render-all t)

  ;; for now disable it since I have >50k per repo
  (lsp-enable-file-watchers nil)

  ;; Enable this when things are slow
  ;; (lsp-print-performance t)

  :config
  (push "[/\\\\]\\.ccls-cache$" lsp-file-watch-ignored)
  (push "[/\\\\]\\.clangd$" lsp-file-watch-ignored)
  (push "[/\\\\]\\.vscode$" lsp-file-watch-ignored)

  (require 'lsp-clients)
  (csetq lsp-clients-clangd-args
         '("-j=7"
           "--all-scopes-completion"
           "--completion-style=detailed"
           "--header-insertion=never"
           "--pch-storage=memory"
           "--background-index"
           "--clang-tidy"
           "--suggest-missing-includes"
           "--log=error"
           )))

(use-package lsp-ui
  :commands lsp-ui-mode
  :general
  (:keymaps 'lsp-mode-map
            :prefix "M-g"
            "r" #'lsp-ui-peek-find-references
            "d" #'lsp-ui-peek-find-definitions)
  :custom
  (lsp-ui-doc-enable nil "Enable it per file if really needed")
  (lsp-ui-doc-include-signature t)

  (lsp-ui-peek-always-show t "Usefull for peeking definitions")

  (lsp-ui-sideline-enable nil "Enable it per file if really needed")
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-async t "Really needed to be responsive")
  (company-lsp-cache-candidates nil "Don't cache them, let the server handle it"))

(use-package flymake :ensure nil
  :preface
  (defun flymake-display-at-point ()
    "Display the flymake diagnostic text for the thing at point."
    (interactive)
    (when (and flymake-mode
               (get-char-property (point) 'flymake-diagnostic))
      (let ((text (flymake--diag-text (get-char-property (point) 'flymake-diagnostic))))
        (when text (message "%s" text)))))

  :general
  (:keymaps 'flymake-mode-map
            :prefix "M-g"
            "f n" #'flymake-goto-next-error
            "f p" #'flymake-goto-prev-error
            "f s" #'flymake-start
            "f f" #'flymake-display-at-point)
  :custom
  (flymake-no-changes-timeout nil "Don't check after changes, only after save")
  (flymake-start-syntax-check-on-newline nil "Don't check on newlines"))

(use-package flymake-diagnostic-at-point
  :defer t
  :ghook 'flymake-mode-hook
  :custom
  (flymake-diagnostic-at-point-display-diagnostic-function
   #'flymake-diagnostic-at-point-display-popup
   "Display a posframe with the diagnostic at point (echo is for eldoc)"))

(use-package imenu :ensure nil
  :general
  (:keymaps 'prog-mode-map
            "M-i" #'imenu)
  :ghook ('imenu-after-jump-hook #'recenter-top-bottom)
  :custom
  (imenu-auto-rescan t "Rescan before showing results")
  (imenu-auto-rescan-maxout (* 1024 1024) "Ignore buffers bigger than this"))

(use-package imenu-anywhere
  :general
  (:keymaps 'prog-mode-map
            "M-I" #'imenu-anywhere))

(use-package yasnippet
  :custom
  (yas-verbosity 1 "Only errors")
  (yas-triggers-in-field t "Snippets inside snippets")
  (yas-wrap-around-region t)
  (yas-also-auto-indent-first-line t)
  :config
  (yas-global-mode t)
  (yas-reload-all))

;;
;; Small emacs enhacements
;;

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region nil)

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
  :disabled
  :custom
  (Vhl/highlight-zero-width-ranges t)

  :config
  (volatile-highlights-mode t)

  (vhl/define-extension 'vhl-undo-tree #'undo-tree-move #'undo-tree-undo #'undo-tree-redo #'undo)
  (vhl/install-extension 'vhl-undo-tree))

(use-package visual-fill-column
  :commands (visual-fill-column-mode global-visual-fill-column-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(use-package which-key
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-idle-delay 0.75)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode t))

;;
;; Shell and Terminals
;;

(use-package eshell :ensure nil
  :commands eshell
  :ghook ('eshell-load-hook
          (lambda ()
            (push 'eshell-tramp eshell-modules-list)
            (push 'eshell-rebind eshell-modules-list)
            ;; (push 'eshell-smart eshell-modules-list)
            (push 'eshell-xtra eshell-modules-list)
            (delq 'eshell-banner eshell-modules-list)))
  :custom
  (eshell-hist-ignoredups t)
  (eshell-history-size 50000)
  (eshell-ls-dired-initial-args (quote ("-h")))
  (eshell-ls-exclude-regexp "~\\'")
  (eshell-ls-initial-args "-hA")
  (eshell-stringify-t nil)
  (eshell-where-to-jump 'begin)
  (eshell-smart-space-goes-to-end t)
  :config
  (require 'esh-module))

(use-package multi-term
  :if (eq system-type 'gnu/linux)

  :general
  ("C-z" #'multi-term-next)
  ("C-c z c" #'multi-term)
  ("C-c z d" #'multi-term-dedicated-toggle)
  ("C-c z n" #'multi-term-next)
  ("C-c z p" #'multi-term-prev)

  :custom
  ;; (multi-term-program "screen")
  ;; (multi-term-program-switches "-DR")
  (term-buffer-maximum-size 0)
  (multi-term-dedicated-select-after-open-p t)
  (multi-term-scroll-show-maximum-output t))

;;
;; Debugging
;;

(use-package gud :ensure nil
  :custom
  (gdb-many-windows t))

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

  :custom
  (magit-diff-arguments
   '("--ignore-space-change" "--ignore-all-space"
     "--no-ext-diff" "--stat" "--diff-algorithm=histogram"))
  (git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line))
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-process-popup-time 20)
  (magit-refs-show-commit-count 'all))

(use-package magit-gitflow
  :ghook ('magit-mode-hook #'turn-on-magit-gitflow))

(use-package git-timemachine
  :commands (git-timemachine git-timemachine-toggle))

(use-package project
  :ensure nil
  :general ("C-c p f" 'project-find-file)
  :preface
  (defvar user/project-roots '("compile_commands.json" "requirements.txt" "pyproject.toml")
    "Files or directories that mark the root of a project.")

  (defun user/project-find-root (path)
    "Search (recursive) for root markers in PATH."

    (unless (file-directory-p path)
      (setq path (file-name-directory path)))

    (catch 'done
      (dolist (proot user/project-roots)
        (if-let ((root (locate-dominating-file path proot)))
            (throw 'done (cons 'transient root))
          nil))))
  :config
  (push #'user/project-find-root project-find-functions))

(use-package vc :ensure nil
  :config
  (push ".ccls-cache" vc-directory-exclusion-list)
  (push ".vscode" vc-directory-exclusion-list)
  (push ".clangd" vc-directory-exclusion-list))

(use-package vc-msg
  :commands (vc-msg-show)
  :custom
  (vc-msg-git-show-commit-function #'magit-show-commit))

(use-package diff-hl
  :after vc
  :custom
  (diff-hl-draw-borders t)
  (diff-hl-side 'right)
  :config
  (global-diff-hl-mode t))

(use-package rmsbolt
  :commands rmsbolt-starter)

;;
;; Misc (user-defined)
;;
(use-package user-advices :load-path "lisp")

;;
;; org. stuff
;;
(use-package calendar :ensure nil
  :defer t
  :custom
  (calendar-week-start-day 1)
  (calendar-date-style 'iso))

(use-package org
  :defer t
  :general
  ("C-c n" #'org-capture
   "C-c a" #'org-agenda)
  :pin manual
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\n  %i\n  %a" :empty-lines 1)
     ("n" "Note" entry (file "~/org/notes.org")
      "* NOTE %? %^G\n%U" :empty-lines 1))))

;;
;; No category
;;
(use-package nov
  :ghook ('nov-mode-hook #'turn-on-visual-line-mode)
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 80))

(use-package elfeed
  :commands (elfeed elfeed-update)
  :preface
  (defun user/elfeed-visual-line-toggle ()
    (interactive)
    (setq-local fill-column 120)
    (visual-fill-column-mode 'toggle)
    (visual-line-mode 'toggle))

  :general
  ("C-x w" #'elfeed)
  (:keymaps 'elfeed-show-mode-map
            "v" #'user/elfeed-visual-line-toggle
            "w" #'elfeed-show-visit)
  (:keymaps 'elfeed-search-mode-map
            "U" #'elfeed-update)

  :init
  (csetq elfeed-search-title-max-width 100)
  (csetq elfeed-search-title-min-width 30)
  (csetq elfeed-search-filter "@1-weeks-ago")
  (csetq elfeed-feeds
         '(("https://www.youtube.com/feeds/videos.xml?channel_id=UC3ts8coMP645hZw9JSD3pqQ" tech)        ;; Andreas Kling
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7S6EpMQ5QNGRg7uJmJWXNw" tech)        ;; QueueQueueHack
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBa659QWEk1AI4Tg--mrJ2A" tech)        ;; Tom Scott
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKTehwyGCKF-b2wo0RKwrcg" tech)        ;; Bisqwit
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbfYPyITQ-7l4upoX8nvctg" tech)        ;; Two Minute Papers
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCS0N5baNlQWJCUrhCEo8WlA" tech)        ;; Ben Eater
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxHAlbZQNFU2LgEtiqd2Maw" tech)        ;; Jason Turner

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCNf56PUyMI0wUyZ8KRhg2AQ" cinema)      ;; Cinema Nippon
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7GV-3hrA9kDKrren0QMKMg" cinema)      ;; CinemaTyler
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAGkSxTlleqxRauEqupyVPw" cinema)      ;; The Cinema Cartography
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCErSSa3CaP_GJxmFpdjG9Jw" cinema)      ;; Lessons from the Screenplay
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKlnkrjfJKGpuZ3-kkhiaXQ" cinema)      ;; Film Histories
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbphDfwSJmxk1Ny_3Oicrng" cinema)      ;; Storytellers
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCx0L2ZdYfiq-tsAXb8IXpQg" cinema)      ;; JustWrite
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxO_ya-RmAXCXJCU54AxYFw" cinema)      ;; New Frame Plus
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCVmAQ9b63_oxitZ9Zgh8nA" cinema)      ;; Lucas Gravey
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZ7g7HfH1gWmhgxW47IcW7Q" cinema)      ;; Beyond the Frame
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCI9DUIgtRGHNH_HmSTcfUbA" cinema)      ;; The Closer Look
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCL5kBJmBUVFLYBDiSiK1VDw" cinema)      ;; Criswell
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWTFGPpNQ0Ms6afXhaWDiRw" cinema)      ;; Now You See It
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCf29Sq6-XxLQG_XuJwMHaFg" cinema)      ;; Nando v Movies

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxTdWpLJurbGlFMWOwXWG_A" history)     ;; Step Back History
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQD-0MjUbDBwm2UTVYr0Dag" history)     ;; Suibhne
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKMnl27hDMKvch--noWe5CA" history)     ;; Cogito

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCO6nDCimkF79NZRRb8YiDcA" art)         ;; Storied (Monstrum)
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmQThz1OLYt8mb2PU540LOA" art)         ;; The Art Assignment

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoW-z7WLeh9IAcr85tnDG6Q" literature)  ;; Malazan

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q" science)     ;; Kurzgesagt
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-3SbfTPJsL8fJAPKiVqBLg" science)     ;; "Deep Look"
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCotwjyJnb-4KW7bmsOoLfkg" science)     ;; "Art of the Problem"
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCzR-rom72PHN9Zg7RML9EbA" science)     ;; "PBS Eons"
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdJ9oJ2GUF8Vmb-G63ldGWg" science)     ;; "Reactions"
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCgRBRE1DUP2w7HTH9j_L4OQ" science)     ;; "Medlife Crisis"
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7_gcs09iThXybpVgjHZ_7g" science)     ;; "PBS Space Time"
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSIvk78tK2TiviLQn4fSHaw" science)     ;; "Up and Atom"
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMk_WSPy3EE16aK5HLzCJzw" science)     ;; "NativLang"

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8XjmAEDVZSCQjI150cb4QA" pop)         ;; Knowing Better
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSriLWQC2J6wNeWXOGlIV4w" pop)         ;; Corporis
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2C_jShtL725hvbm1arSV9w" pop)         ;; GCP Grey
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCuPgdqQKpq4T4zeqmTelnFg" pop)         ;; kaptainkristian
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-gjznzViwMols6dz89qLbg" pop)         ;; Entertain The Elk
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHiwtz2tCEfS17N9A-WoSSw" pop)         ;; Pop Culture Detective
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqJ-Xo29CKyLTjn6z2XwYAw" pop)         ;; Game Maker's Toolkit
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCaPRCWnFAzeI3_tr--Qw5qg" pop)         ;; Human Interests
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCiB8h9jD2Mlxx96ZFnGDSJw" pop)         ;; Origin Of Everything
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCs_bV79AmugXVvjaASibPnw" pop)         ;; KhAnubis
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXkNod_JcH7PleOjwK_8rYQ" pop)         ;; Polyphonic
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9RM-iSvTu1uPJb8X5yp3EQ" pop)         ;; Wendover

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCct9aR7HC79Cv2g-9oDOTLw" religion)    ;; ReligionForBreakfast
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtCTSf3UwRU14nYWr_xm-dQ" religion)    ;; Jonathan Pageau

           ("https://fabiensanglard.net/rss.xml" tech)
           ("https://danluu.com/atom.xml" tech)
           ("https://trofi.github.io/feed/atom.xml" tech)
           ("https://planet.emacslife.com/atom.xml" emacs)
           ("https://www.phoronix.com/rss.php" linux phoronix)
           ("https://xkcd.com/atom.xml" comic)
           ))
  :config
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "1 months ago"
                                :remove 'unread)))

(provide 'init)

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(symbol-overlay-default-face ((t (:inherit underline))))
 '(whitespace-tab ((t (:background nil :foreground "light gray")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(modus-operandi-theme-3d-modeline nil)
 '(modus-operandi-theme-bold-constructs t)
 '(modus-operandi-theme-proportional-fonts nil)
 '(modus-operandi-theme-slanted-constructs nil)
 '(modus-operandi-theme-visible-fringes nil)
 '(package-selected-packages
   '(beginend cmake-font-lock cmake-mode comment-dwim-2 company company-lsp company-posframe counsel counsel-etags cython-mode diff-hl dired-du dired-git-info dired-narrow diredfl dumb-jump eacl elfeed expand-region flymake-diagnostic-at-point general git-timemachine haskell-mode hl-todo hydra iedit ignoramus imenu-anywhere ivy ivy-posframe ivy-rich iy-go-to-char js2-mode json-mode log4j-mode lsp-mode lsp-ui magit magit-gitflow minions modern-cpp-font-lock modus-operandi-theme modus-vivendi-theme multi-term multiple-cursors nasm-mode no-littering nov pyvenv rainbow-delimiters rainbow-mode realgud rg rmsbolt smex string-inflection swiper symbol-overlay undo-tree use-package vc-msg visual-fill-column web-mode wgrep which-key yaml-mode yasnippet)))


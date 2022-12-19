;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; It's init.el, that says enough.

;;; Code:

;; Easier customization
(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defmacro setq-if-exists (variable value)
  "Set VARIABLE to VALUE. Error out if VARIABLE is not special."
  `(if (special-variable-p ',variable)
       (setq ,variable ,value)
     (error (format "Variable %s does not exist" ',variable))))

(defvar user/gc-cons-threshold 134217728)

(csetq custom-file (expand-file-name "custom-unused.el" user-emacs-directory))

(csetq
 package-selected-packages
 '(adoc-mode avy clang-format+ comment-dwim-2 company consult
   cycle-at-point cython-mode denote diff-hl dired-du dired-git-info
   dired-narrow dumb-jump elfeed embark embark-consult eterm-256color
   expand-region fish-mode general git-timemachine haskell-mode hl-todo
   ignoramus imenu-list js2-mode json-mode json-reformat just-mode lin
   log4j-mode magit magit-gitflow marginalia markdown-mode minions modus-themes
   multiple-cursors mwim nasm-mode nix-mode no-littering nov orderless
   org-bullets projectile python-black racket-mode rainbow-delimiters
   rainbow-mode realgud rg rust-mode string-inflection symbol-overlay undo-fu
   undo-fu-session use-package vc-msg vertico visual-fill-column web-mode wgrep
   which-key yaml-mode yasnippet))

(defun user/after-init ()
  "Will run after init to restore some stuff."
  (message "emacs-init-time: %s" (emacs-init-time))
  (csetq gc-cons-threshold user/gc-cons-threshold)
  (garbage-collect))

;; (setq garbage-collection-messages t)

(add-hook 'after-init-hook #'user/after-init)
(csetq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(dolist (dir (directory-files (expand-file-name "site-lisp" user-emacs-directory) t))
  (when (and (not (string-suffix-p "." dir))
             (file-directory-p dir))
    (add-to-list 'load-path (expand-file-name dir (expand-file-name "site-lisp" user-emacs-directory)))))

;; Install use-package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(csetq use-package-enable-imenu-support t)
(csetq use-package-expand-minimally nil)
(csetq use-package-always-ensure t)
(csetq use-package-compute-statistics nil)

(eval-when-compile
  (require 'use-package))

(defun package-rebuild-all ()
  "Rebuild all the user-installed packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun user/gc-on-last-frame-out-of-focus ()
  "GC if all frames are inactive."
  (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
      (garbage-collect)))

(add-function :after after-focus-change-function #'user/gc-on-last-frame-out-of-focus)

;;
;; Packages needed no matter what, and usually others are depended on it
;;

(use-package general)

(use-package modus-themes
  :pin "melpa"
  :ghook ('after-init-hook (lambda () (load-theme 'modus-operandi t)))
  :custom
  (modus-themes-variable-pitch-ui t)
  (modus-themes-fringes 'subtle)
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-paren-match '(bold intense))
  (modus-themes-mixed-fonts t)
  (modus-themes-hl-line '(intense))
  (modus-themes-completions '((matches . (extrabold background intense))
                              (selection . (extrabold accented intense))
                              (popup . (accented)))))

(use-package lin
  :ghook ('after-init-hook #'lin-global-mode)
  ('after-init-hook #'global-hl-line-mode))

(use-package minions
  :custom
  (minions-mode-line-lighter ";")
  :config
  (push 'flycheck-mode minions-prominent-modes)
  (push 'overwrite-mode minions-prominent-modes)
  (minions-mode t))

(use-package ignoramus
  :config
  (ignoramus-setup))

(use-package no-littering)

;; For now disabled, since I don't have any modifications to the $PATH
(use-package exec-path-from-shell
  :disabled
  :config
  (exec-path-from-shell-initialize))

;;
;; Some default settings that I like
;;

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)

(csetq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(csetq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(csetq enable-local-variables :safe)
(csetq backup-by-copying t)
(csetq view-read-only t)
(csetq delete-old-versions t)
(csetq make-backup-files nil)
(csetq auto-save-default nil)
(csetq large-file-warning-threshold (* 50 1024 1024))
(csetq save-silently t)
(csetq require-final-newline t)
(file-name-shadow-mode t)

(csetq completion-ignore-case t)
(csetq system-time-locale "C")
(csetq auto-window-vscroll nil)
(csetq use-dialog-box nil)
(csetq minibuffer-electric-default-mode t)
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
(csetq frame-title-format '(:eval "%b - Emacs"))
(csetq indicate-buffer-boundaries
       '((top . right)
         (bottom . right)
         (t . nil)))

(csetq command-line-ns-option-alist nil)

(csetq read-file-name-completion-ignore-case t)
(csetq bookmark-save-flag t)
(csetq column-number-indicator-zero-based nil)
(csetq disabled-command-function nil)
(csetq sentence-end-double-space nil)
(csetq colon-double-space nil)
(csetq ad-redefinition-action 'accept)
(csetq woman-use-topic-at-point-default t)
(csetq apropos-do-all t)
;; (csetq idle-update-delay 1)
(csetq custom-buffer-done-kill t)

(csetq bidi-paragraph-direction 'left-to-right)
(csetq bidi-inhibit-bpa t)

;; Disable some decorations in c/c++
;; (setq font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t)))

;; "Smooth" mouse scrolling, one line at a time
(csetq mouse-wheel-scroll-amount
       '(1
         ((shift) . 5)
         ((meta) . 0.5)
         ((control) . text-scale)))
(csetq scroll-conservatively 101)
;; (csetq scroll-preserve-screen-position t)
(csetq fast-but-imprecise-scrolling t)

(csetq x-underline-at-descent-line t)
(csetq x-stretch-cursor t)
(csetq x-wait-for-event-timeout nil)

(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 1024 1024))

(csetq tooltip-resize-echo-area t)
(csetq tooltip-delay 0.5)
(csetq x-gtk-use-system-tooltips nil)

(csetq uniquify-buffer-name-style 'post-forward-angle-brackets)
(csetq uniquify-after-kill-buffer-p t)

(csetq warning-suppress-types '((comp)))

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

;; window-system is not enough when the daemon is used
(when (or (memq window-system '(pgtk))
          (getenv "WAYLAND_DISPLAY"))
  (push 'text/plain\;charset=utf-8 x-select-request-type))

(transient-mark-mode t)
(auto-save-mode -1)
(delete-selection-mode t)
(size-indication-mode -1)
(line-number-mode t)
(column-number-mode t)
(global-so-long-mode t)

(winner-mode t)
(minibuffer-depth-indicate-mode t)
(blink-cursor-mode -1)
(save-place-mode t)
(global-auto-revert-mode t)

(pixel-scroll-precision-mode t)

;;
;; Mode-line
;;
;; (csetq mode-line-compact 'long)

(defun mode-line-vc ()
  "Will returing the same thing as variable `vc-mode', but with a hard-coded max length and no Git:."
  (if vc-mode
      (concat (substring vc-mode 5 (min 30 (length vc-mode))) "    ")
    ""))

(csetq mode-line-format
       '("%e"
         mode-line-front-space
         mode-line-mule-info
         mode-line-client
         mode-line-modified
         ;; mode-line-remote
         mode-line-frame-identification
         mode-line-buffer-identification
         "    "
         mode-line-position
         (:eval (mode-line-vc))
         minions-mode-line-modes
         "    "
         mode-line-misc-info
         mode-line-end-spaces))

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

(defun toggle-tabs-spaces ()
  "Toggle between tabs and spaces in the current file."
  (interactive)
  (if indent-tabs-mode
      (progn
        (message "Switch from TABS to SPACES")
        (setq-local indent-tabs-mode nil))
    (message "Switch from SPACES to TABS")
    (setq-local indent-tabs-mode t)))

(add-hook 'text-mode-hook #'auto-fill-mode)

;; Some special file names
(add-to-list 'auto-mode-alist '("\\.?bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Makefile.*" . makefile-mode))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(when (member "all-the-icons" (font-family-list))
  (set-fontset-font t 'unicode "all-the-icons" nil 'append))

(push `(,(rx string-start "*" (or "Fly" "compilation"))
        (display-buffer-reuse-window
         display-buffer-in-side-window)
        (side            . bottom)
        (reusable-frames . nil)
        (window-height   . 0.25)
        (slot . -1))
      display-buffer-alist)

(push `(,(rx string-start "*" (or "Backtrace" "Warnings" "Compile-Log"))
        (display-buffer-reuse-window
         display-buffer-in-side-window)
        (side            . bottom)
        (reusable-frames . nil)
        (window-height   . 0.33)
        (slot . 0))
      display-buffer-alist)

(push `(,(rx "*Completions*")
        (display-buffer-use-some-window
         display-buffer-pop-up-window)
        (window-height 0.25)
        (reusable-frames . nil)
        (inhibit-same-window . t))
      display-buffer-alist)

(push `(,(rx "*Pp ")
        (display-buffer-reuse-window
         display-buffer-in-side-window)
        (side            . bottom)
        (reusable-frames . nil)
        (window-height   . 0.25)
        (slot . -1))
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
  :commands diff-mode diff-delete-empty-files diff-make-unified
  :gfhook #'diff-delete-empty-files #'diff-make-unified #'smerge-mode
  :custom
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax 'hunk-also)
  (diff-switches '("-u" "-p" "-w")))

(use-package ediff :ensure nil
  :defer
  :custom
  (ediff-highlight-all-diffs t)
  (ediff-diff-options "-w")
  (ediff-show-clashes-only t)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package tab-bar :ensure nil
  :defer
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-show 1))

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

(use-package hexl :ensure nil
  :defer
  :custom
  (hexl-bits 8))

(use-package dabbrev :ensure nil
  :defer
  :custom
  (dabbrev-case-replace nil)
  (dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*$]"))

(use-package hippie-exp :ensure nil
  :defer
  :custom
  (hippie-expand-try-functions-qlist
   '(try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-expand-list
     try-expand-list-all-buffers
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs))
  (hippie-expand-verbose nil))

(use-package ispell :ensure nil
  :defer
  :custom
  (ispell-program-name (executable-find "aspell"))
  (ispell-extra-args '("--sug-mode=normal" "--keyboard=standard")))

(use-package flyspell :ensure nil
  :defer
  :custom
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil)
  (flyspell-use-meta-tab nil))

(defun hide-trailing-whitespace ()
  "Used for hooks to various modes."
  (setq-local show-trailing-whitespace nil))

(defun user/garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used-r = (* used size)
                    for free-r = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used-r free-r))
                    for used-h = (file-size-human-readable used-r)
                    for free-h = (file-size-human-readable free-r)
                    concat (format "%s: %s + %s = %s\n" type used-h free-h total))))

(defun user/minibuffer-setup-hook ()
  "Hook to run when entering the minibuffer."
  (csetq gc-cons-threshold most-positive-fixnum))

(defun user/minibuffer-exit-hook ()
  "Hook to run when exiting the minibuffer."
  (csetq gc-cons-threshold user/gc-cons-threshold))

;; Increase the memory while in the minibuffer
(add-hook 'minibuffer-setup-hook #'user/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'user/minibuffer-exit-hook)

(use-package xref :ensure nil
  :defer
  :config
  (csetq xref-search-program 'ripgrep)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

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
  "C-x C-z"
  "C-x f"
  "C-x m"
  "C-x >"
  "C-x <"
  "<C-next>"
  "<C-prior>"
  "M-`"
  "M-'")

(use-package user-utils :ensure nil
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
  ("M-3" #'user/split-window-right)
  ("M-0" #'delete-window)
  ("M-1" #'delete-other-windows)
  ("M-o" #'other-window)
  ("C-w" #'user/kill-word-or-region)
  ("<C-return>" #'user/open-line-above)
  ;; mwim replaced this
  ;; ("C-a" #'user/move-beginning-of-line)
  ;; ("C-e" #'move-end-of-line)
  )

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

(general-define-key [remap keyboard-quit] #'keyboard-quit-context+)
(general-define-key [remap minibuffer-keyboard-quit] #'keyboard-quit-context+)
(general-define-key [remap dabbrev-expand] #'hippie-expand)

;; (general-define-key [remap eval-last-sexp] #'pp-eval-last-sexp)
(general-define-key [remap eval-expression] #'pp-eval-expression)

(general-define-key "S-<down-mouse-1>" #'mouse-set-mark)

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
(use-package profiler :ensure nil
  :general
  ("<f8>" #'profiler-start)
  ("<f9>" #'profiler-stop))

;; (use-package docker-tramp
;;   :disabled
;;   :custom
;;   (docker-tramp-docker-:executable "podman"))

(use-package tramp
  :defer
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 2)
  :config
  (push
   (cons
    "docker"
    '((tramp-login-program "podman")
      (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
      (tramp-remote-shell "/bin/bash")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods)
  (push '("/docker:" "direct-async-process" t) tramp-connection-properties))

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
  (whitespace-tab ((t (:background "unspecified" :foreground "light gray"))))

  :custom
  (whitespace-display-mappings
   '((tab-mark ?\t [187 ?\t])))
  (whitespace-style '(face tabs tab-mark trailing)))

(use-package display-line-numbers :ensure nil
  :custom
  (display-line-numbers-grow-only nil)
  (display-line-numbers-width-start nil)
  :ghook
  ('prog-mode-hook #'display-line-numbers-mode)
  ('text-mode-hook #'display-line-numbers-mode))

(use-package recentf :ensure nil
  :preface
  (defadvice write-region (around recentf-no-message)
    (ad-set-arg 4 'nomsg)
    ad-do-it
    (set-buffer-modified-p nil))

  (defadvice recentf-save-list (around no-message activate)
    "Suppress the output from `write-region' to minibuffer during the `recentf-save-list'."
    (let ((activated (ad-is-active 'write-region)))
      (ad-enable-advice 'write-region 'around 'recentf-no-message)
      (ad-activate 'write-region)
      (unwind-protect
          ad-do-it
        (ad-disable-advice 'write-region 'around 'recentf-no-message)
        (if activated
            (ad-activate 'write-region)
          (ad-deactivate 'write-region)))))

  :custom
  (recentf-auto-cleanup 'never)
  (recentf-exclude (list
                    "/elpa/.*\\'"
                    "/.ccls-cache/.*\\'"
                    "/.cache/.*\\'"
                    "PKGBUILD"
                    "/usr/.*\\'"
                    "/tmp/.*\\'"
                    "/nix/store/.*\\'"
                    "/.cache/pypoetry/virtualenvs/.*\\'"
                    #'ignoramus-boring-p))
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 100)

  :config
  (when source-directory
    (push (concat source-directory ".*\\'") recentf-exclude))
  (push no-littering-var-directory recentf-exclude)
  (push no-littering-etc-directory recentf-exclude)

  (run-at-time (* 1 60) (* 1 60) #'recentf-save-list)

  :ghook ('after-init-hook #'recentf-mode))

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
  ;; TODO: Is this needed on Linux? Disabled 06/14/2022. Re-evaluate in a few months
  ;; :ghook ('comint-output-filter-functions #'comint-strip-ctrl-m)
  :custom
  (comint-process-echoes t)
  (comint-prompt-read-only t)
  (comint-history-isearch t)
  (comint-ignore-dups t))

(use-package compile :ensure nil
  :preface
  (defun compile-without-ask (ask)
    (interactive "P")
    (let (compile-fn)
      (if (projectile-project-root)
          (setq compile-fn #'projectile-compile-project)
        (setq compile-fn #'compile))
    (if ask
        (call-interactively compile-fn)
      (let ((compilation-read-command nil))
        (funcall compile-fn nil)))))

  (defun user/compilation-done (buffer msg)
    (let ((bufwin (get-buffer-window buffer))
          (abnormal (or (string-match-p "abnormally" msg)
                        (> 0 (+ compilation-num-errors-found compilation-num-warnings-found))))
          (interrupted (string-match-p "interrupt" msg)))
      (when (and bufwin (string-equal (buffer-name buffer) "*compilation*"))
        (if interrupted
            (delete-window bufwin)
          (unless abnormal
            (run-with-timer 0.5 nil #'delete-window bufwin))))))

  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  :general
  ("<f5>" #'compile-without-ask)
  ([remap comment-region] #'compile-without-ask)
  ("C-c c" #'compile-without-ask)
  (:keymaps '(compilation-mode-map comint-mode-map)
            "q" #'kill-buffer-and-window)

  :ghook
  ('compilation-mode-hook #'hide-trailing-whitespace)
  ('compilation-finish-functions #'user/compilation-done)
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
  (dired-listing-switches "-lFAGh1v --group-directories-first")
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
  :disabled
  :ghook 'dired-mode-hook)

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
  ;; ("M-1" #'er/contract-region)
  :custom
  (expand-region-fast-keys-enabled nil)
  (expand-region-autocopy-register "e"))

(use-package symbol-overlay
  :disabled ;; Until fixes the vertico & company break
  :preface
  (define-globalized-minor-mode global-symbol-overlay-mode
    symbol-overlay-mode symbol-overlay-mode :group 'symbol-overlay)
  :general
  ("M-*" #'symbol-overlay-put)
  ("<M-down>" #'symbol-overlay-jump-next)
  ("<M-up>" #'symbol-overlay-jump-prev)
  ("M-8" #'symbol-overlay-toggle-in-scope)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit underline))))
  :custom
  (symbol-overlay-idle-time 0.25)
  (symbol-overlay-displayed-window t)
  :ghook
  ('after-init-hook #'global-symbol-overlay-mode))

(use-package mwim
  :general
  ([remap move-beginning-of-line] #'mwim-beginning-of-code-or-line) ;; maybe
  ([remap move-end-of-line] #'mwim-end-of-code-or-line))

(use-package avy
  :general
  ("C-'" #'avy-goto-char-timer)
  :preface
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  :config
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
  )

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
  (mc-prompt-once #'zap-up-to-char))

;;
;; Completion-related
;;

(use-package orderless
  :pin "melpa"
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch))
  (completion-styles '(orderless))
  :init
  (require 'orderless))

(use-package vertico
  :custom
  (enable-recursive-minibuffers t)
  (vertico-resize t)
  :general
  (:keymaps 'vertico-map
            "<escape>" #'keyboard-quit-context+
            "<prior>" #'vertico-scroll-down
            "<next>"  #'vertico-scroll-up)
  :init
  (vertico-mode t)
  (vertico-reverse-mode t))

(use-package marginalia
  :after vertico
  :general
  (:keympas 'minibuffer-local-map
            "C-o" #'marginalia-cycle)
  :config
  (marginalia-mode t))

(use-package consult
  :general
  ([remap switch-to-buffer] #'consult-buffer)
  ([remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  ([remap copy-to-register] #'consult-register)
  ([remap bookmark-jump] #'consult-bookmark)
  ([remap yank-pop] #'consult-yank-pop)
  ([remap imenu] #'consult-imenu)
  ([remap goto-line] #'consult-goto-line)
  ("M-g i" #'consult-imenu)
  ("M-g I" #'consult-imenu-multi)
  ("M-s l" #'consult-line)
  ("M-s L" #'consult-line-multi)
  ("M-g o" #'consult-outline)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key (kbd "M-."))
  (consult-narrow-key "<"))

(use-package embark
  :general
  ("C-;" #'embark-act)
  ("C-:" #'embark-dwim)
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (push '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
          nil
          (window-parameters (mode-line-format . none)))
        display-buffer-alist))

(use-package embark-consult
  :after (embark consult))

(use-package company
  :preface
  (defun user/setup-company-backends ()
    (csetq company-backends (remove 'company-capf company-backends))
    (csetq company-backends (remove 'company-clang company-backends))
    (push 'company-capf company-backends))
  :general
  ("C-c C-." #'company-complete)
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
  ;; (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above nil)
  ;; (company-occurrence-weight-function #'company-occurrence-prefer-any-closest)
  ;; (company-transformers '(company-sort-by-occurrence))
  :ghook
  ('after-init-hook #'global-company-mode)
  ('global-company-mode-hook #'user/setup-company-backends))

;;
;; Extra modes
;;

(use-package hl-todo
  :ghook ('after-init-hook #'global-hl-todo-mode))

(use-package cython-mode :defer)

(use-package nasm-mode
  :mode "\\.asm\\'")

(use-package log4j-mode
  :mode "\\.log\\'")

(use-package yaml-mode :defer)

(use-package json-reformat :defer)

(use-package json-mode
  :mode "\\.json\\'"
  :general
  (:keymaps 'json-mode-map
            "M-q" #'json-reformat-region)
  :custom
  (json-reformat:indent-width 4)
  (json-reformat:pretty-string? t))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package adoc-mode
  :defer t)

(use-package nov
  :gfhook #'turn-on-visual-line-mode
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 80))

;; (use-package pdf-tools
;;   :disabled
;;   :mode ("\\.pdf\\'" . pdf-view-mode)
;;   :ghook ('pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
;;   :custom
;;   (pdf-view-display-size 'fit-page)
;;   (pdf-view-resize-factor 1.1))

;;
;; Searching
;;
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
  ("C-s" #'isearch-forward-regexp)
  ("C-M-s" #'isearch-forward)
  ("C-r" #'isearch-backward-regexp)
  ("C-M-r" #'isearch-backward)
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

(use-package grep :ensure nil
  :ghook ('grep-mode-hook #'hide-trailing-whitespace)
  :config
  (push ".ccls-cache" grep-find-ignored-directories)
  (push ".vscode" grep-find-ignored-directories)
  (push ".cache" grep-find-ignored-directories)
  (push ".idea" grep-find-ignored-directories))

(use-package rg
  :general
  (:prefix "M-s"
           "d" #'rg-dwim
           "r" #'rg
           "p" #'rg-project)
  :custom
  (rg-custom-type-aliases nil)
  (rg-ignore-case 'smart)
  (rg-hide-command nil))

(use-package wgrep
  :defer
  :custom
  (wgrep-auto-save-buffer t))

;;
;; Programming
;;
(use-package prog-mode :ensure nil
  :defer
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

;; (add-hook 'after-save-hook #'validate-balance)

(use-package paren :ensure nil
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :ghook ('after-init-hook #'show-paren-mode))

(use-package string-inflection :defer)

(use-package comment-dwim-2
  :general ("M-;" #'comment-dwim-2))

(use-package cycle-at-point
  :general ("C-=" #'cycle-at-point))

(use-package treesit
  :pin "manual"

  ;; FIXME: Find a package which does this, or write one :^)
  ;; :preface
  ;; (defun remap-some ()
  ;;     (when (and (eq (char-before) last-command-event) ; Sanity check.
  ;;                (not executing-kbd-macro)
  ;;                (not noninteractive)
  ;;                (equal (buffer-substring (- (point) 2) (point)) ";;"))
  ;;       (replace-string ";;" "::" nil (- (point) 2) (point))))
  ;; :ghook
  ;; ('post-self-insert-hook #'remap-some)

  :custom
  (treesit-font-lock-level 4)
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'k&r)
  :config
  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(sh-mode . bash-ts-mode) major-mode-remap-alist)
  (push '(c-mode . c-ts-mode) major-mode-remap-alist)
  (push '(c++-mode . c++-ts-mode) major-mode-remap-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dumb-jump
  :ghook ('dumb-jump-after-jump-hook #'recenter-top-bottom))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package just-mode)

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
      (c-cleanup-list                . (brace-else-brace
                                        brace-elseif-brace
                                        brace-catch-brace
                                        list-close-comma
                                        defun-close-semi
                                        empty-defun-braces
                                        scope-operator))
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
                          (statement-cont . +)))
      (c-cleanup-list  . (brace-else-brace
                          brace-elseif-brace
                          brace-catch-brace
                          list-close-comma
                          defun-close-semi
                          empty-defun-braces
                          scope-operator
                          compact-empty-funcall))))

  (defconst user/webkit-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)
      (c-comment-only-line-offset . 0)
      (c-offsets-alist . ((arglist-close           .  4)
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
                          (brace-list-intro        .  4)
                          (namespace-open          .  0)
                          (namespace-close         .  0)
                          (innamespace             .  0)
                          ))
      (c-cleanup-list  . (brace-else-brace
                          brace-elseif-brace
                          brace-catch-brace
                          list-close-comma
                          defun-close-semi
                          empty-defun-braces
                          scope-operator
                          compact-empty-funcall))))

  (defun user/c-mode-common-hook ()
    "Hook for C/C++ mode."
    (c-toggle-auto-newline t)
    (c-toggle-syntactic-indentation t)

    (modify-syntax-entry ?' "."))

  (defun user/c-mode-toggle-funcall ()
    "Transpose multi-line call to one-line and vice-versa."
    (interactive)
    (let (bosl eosl bos eos)
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

  (defun user/cleanup-empty-lines (orig-func &rest args)
    (when (eq last-input-event ?})
      (save-mark-and-excursion
        (when (looking-at-p "\\([[:space:]]*\n\\)+")
          (delete-horizontal-space)
          (delete-blank-lines)

          (when (looking-at-p "^[[:space:]]*\n")
            (kill-line)))))

    (apply orig-func args))

  :ghook ('c-mode-common-hook #'user/c-mode-common-hook)

  :custom
  (c-tab-always-indent nil)
  (c-backspace-function #'backward-delete-char)
  (c-hanging-semi&comma-criteria nil)
  (c-doc-comment-style '((java-mode . javadoc)
                         (pike-mode . autodoc)
                         (c-mode . doxygen)
                         (c++-mode . doxygen)))

  :init
  (c-add-style "allman" user/allman-style)
  (c-add-style "sane-k&r" user/k&r-style)
  (c-add-style "webkit" user/webkit-style)

  ;; (advice-add 'c-electric-brace :around #'user/cleanup-empty-lines)

  (csetq c-default-style '((java-mode . "java")
                           (awk-mode . "awk")
                           (c++-mode . "webkit")
                           (other . "sane-k&r"))))

(use-package clang-format+
  :after cc-mode
  ;; :ghook ('c-mode-common-hook #'clang-format+-mode)
  :custom (clang-format+-context 'modification))

;; Leave it commented for now. CCLS is useful only for lsp (also disabled for now)
;; (use-package ccls
;;   :after cc-mode
;;   :when (executable-find "ccls")
;;   :custom
;;   (ccls-sem-highlight-method 'font-lock)
;;   (ccls-initialization-options
;;    '(:diagnostics (:onOpen 0 :onSave 0 :onChange -1 :spellChecking :json-false)
;;      :highlight (:largeFileSize 0)
;;      :index (:blacklist ["/app/.*" "/examples/.*"]) ))
;;   :config
;;   (push "compile_commands.json" ccls-root-files))

(use-package rust-mode
  :commands rust-mode
  :general
  (:keymaps 'rust-mode-map
            "C-c C-c" #'rust-compile))

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

(use-package python-black
  :after python
  :defer)

(use-package haskell-mode :defer)

(use-package racket-mode
  :mode "\\.rkt\\'"
  :preface
  (defun user/racket-xp-hide-info ()
    (remove-hook 'pre-redisplay-functions
                 #'racket-xp-pre-redisplay
                 t))
  :ghook
  ('racket-mode-hook #'racket-xp-mode)
  ('racket-xp-mode-hook #'user/racket-xp-hide-info))

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js2-skip-preprocessor-directives t))

(use-package web-mode
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
;; (use-package lsp-mode
;;   :commands (lsp lsp-mode)

;;   :ghook
;;   ('c-mode-common-hook #'lsp-deferred t)
;;   ('python-mode-hook #'lsp-deferred t)
;;   ('rust-mode-hook #'lsp-deferred t)
;;   ('cmake-mode-hook #'lsp-deferred t)
;;   ('sh-mode-hook #'lsp-deferred t)
;;   ('yaml-mode-hook #'lsp-deferred t)
;;   ('lsp-mode-hook #'lsp-enable-which-key-integration)

;;   :custom
;;   ;; performance reasons (and they're not really useful for me)
;;   (lsp-enable-on-type-formatting nil)
;;   (lsp-enable-folding nil)
;;   (lsp-enable-indentation nil)
;;   (lsp-before-save-edits nil)
;;   (lsp-enable-symbol-highlighting nil)
;;   (lsp-enable-semantic-highlighting nil)
;;   (lsp-enable-imenu t)
;;   (lsp-lens-enable nil)
;;   (lsp-auto-guess-root nil)
;;   (lsp-restart 'interactive)
;;   (lsp-pylsp-plugins-flake8-max-line-length 100)
;;   (lsp-eldoc-render-all nil)
;;   (lsp-keymap-prefix "<C-m>")

;;   ;; Not usefull for now
;;   (lsp-modeline-code-actions-enable nil)
;;   (lsp-modeline-diagnostics-enable nil)
;;   (lsp-modeline-workspace-status-enable nil)

;;   ;; for now disable it since I have >50k per repo
;;   (lsp-file-watch-threshold 9000)
;;   (lsp-enable-file-watchers nil)

;;   (lsp-modeline-diagnostics-scope :file)

;;   ;; (lsp-log-io t)

;;   :config
;;   (csetq lsp-clients-clangd-args
;;          '("-j=6"
;;            "--completion-style=detailed"
;;            "--header-insertion=never"
;;            "--pch-storage=memory"
;;            "--background-index"
;;            "--cross-file-rename"
;;            "--clang-tidy"
;;            "--suggest-missing-includes"
;;            ))

;;   (defun lsp-tramp-connection (local-command &optional generate-error-file-fn)
;;     "Create LSP stdio connection named name.
;; LOCAL-COMMAND is either list of strings, string or function which
;; returns the command to execute."

;;     (list :connect (lambda (filter sentinel name environment-fn)
;;                      (let* ((final-command (list "sh" "-c"
;;                                                  (string-join (append
;;                                                                (cons "stty raw > /dev/null;" (lsp-resolve-final-function local-command))
;;                                                                (list
;;                                                                 (concat "2>"
;;                                                                         (or (when generate-error-file-fn
;;                                                                               (funcall generate-error-file-fn "name"))
;;                                                                             (format "/tmp/%s-%s-stderr" "name"
;;                                                                                     (cl-incf lsp--stderr-index))))))
;;                                                               " "))
;;                                                    )
;;                             (process-name (generate-new-buffer-name name))
;;                             ;; (stderr-buf (stderr-buf (format "*%s::stderr*" process-name)))
;;                             (process-environment
;;                              (lsp--compute-process-environment environment-fn))
;;                             (proc (make-process
;;                                    :name process-name
;;                                    :buffer (format "*%s*" process-name)
;;                                    :command (lsp-resolve-final-function final-command)
;;                                    :connection-type 'pipe
;;                                    :coding 'utf-8-emacs-unix
;;                                    :noquery t
;;                                    :filter filter
;;                                    :sentinel sentinel
;;                                    :file-handler t)))
;;                        (cons proc proc)))
;;           :test? (lambda () (-> local-command lsp-resolve-final-function
;;                                 lsp-server-present?))))

;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
;;                     :major-modes '(c-mode c++-mode)
;;                     :remote? t
;;                     :server-id 'clangd-remote)))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :general
;;   (:keymaps 'lsp-mode-map
;;             "<f2>" #'lsp-ui-doc-glance)
;;   (:keymaps 'lsp-mode-map
;;             :prefix "M-g"
;;             "r" #'lsp-ui-peek-find-references
;;             "d" #'lsp-ui-peek-find-definitions)
;;   :custom
;;   (lsp-ui-doc-enable nil "Enable it per file if really needed")
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-header nil)
;;   (lsp-ui-doc-position 'at-point)

;;   (lsp-ui-peek-always-show t "Useful for peeking definitions")

;;   (lsp-ui-sideline-enable nil "Enable it per file if really needed")
;;   (lsp-ui-sideline-show-diagnostics t)
;;   (lsp-ui-sideline-show-hover nil)
;;   (lsp-ui-sideline-show-symbol t)
;;   (lsp-ui-sideline-ignore-duplicate t))

;; (use-package flycheck
;;   :ghook ('after-init-hook #'global-flycheck-mode)
;;   :custom
;;   (flycheck-keymap-prefix (kbd "M-'"))
;;   (flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled))
;;   (flycheck-idle-buffer-switch-delay 0.1))

;; (use-package consult-flycheck
;;   :after consult)

;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :ghook 'global-flycheck-mode-hook)

(use-package eglot
  :pin "elpa"
  :ghook
  ('c-mode-common-hook #'eglot-ensure t)
  ('c-ts-mode-hook #'eglot-ensure t)
  ('c++-ts-mode-hook #'eglot-ensure t)
  ('python-mode-hook #'eglot-ensure t)
  ('python-ts-mode-hook #'eglot-ensure t)
  ('rust-mode-hook #'eglot-ensure t)
  ('cmake-ts-mode-hook #'eglot-ensure t)
  ;; ('sh-mode-hook #'eglot-ensure t)
  ;; ('bash-ts-mode-hook #'eglot-ensure t)
  ('yaml-mode-hook #'eglot-ensure t)
  :defer
  :custom
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (eglot-ignored-server-capabilities
   '(:hoverProvider :documentHighlightProvider))                   ;; eldoc + this takes up a lot of space
  (eglot-send-changes-idle-time 1.0)
  :config
  (add-to-list
   'eglot-server-programs
   `((c-mode c++-mode) .
     ,(eglot-alternatives
       '("ccls"
         ("clangd"
          "-j=6"
          ;; "--all-scopes-completion"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--pch-storage=memory"
          ;; "--log=error"
          "--background-index"
          "--cross-file-rename"
          "--clang-tidy")))))
  ;; Just so I don't forget about this
  ;; (setq-default eglot-workspace-configuration
  ;;               '((haskell
  ;;                  (maxCompletions . 200))))
  (push '((eglot (styles orderless))) completion-category-overrides))

(use-package flymake
  :preface
  (defun user/flymake-mode-line-format ()
    (if (bound-and-true-p flymake-mode)
        flymake-mode-line-format
      ""))
  :general
  (:prefix "M-'"
           "l" #'flymake-show-buffer-diagnostics
           "n" #'flymake-goto-next-error
           "p" #'flymake-goto-prev-error)
  :init
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-to-list 'mode-line-misc-info '(:eval (user/flymake-mode-line-format)) t))

(use-package imenu :ensure nil
  :general
  (:keymaps 'prog-mode-map
            "M-i" #'imenu)
  :ghook ('imenu-after-jump-hook #'recenter-top-bottom)
  :custom
  (imenu-auto-rescan t "Rescan before showing results")
  (imenu-auto-rescan-maxout (* 2 1024 1024) "Ignore buffers bigger than this"))

(use-package imenu-list
  :defer t
  :custom
  (imenu-list-auto-resize t)
  (imenu-list-position 'left))

(use-package yasnippet
  :custom
  (yas-verbosity 1 "Only errors")
  (yas-triggers-in-field t "Snippets inside snippets")
  (yas-wrap-around-region t)
  (yas-also-auto-indent-first-line t)
  :ghook
  ('after-init-hook #'yas-global-mode))

;;
;; Small emacs enhacements
;;

(use-package undo-fu
  :general
  ([remap undo] #'undo-fu-only-undo)
  ([remap undo-redo] #'undo-fu-only-redo)
  :custom
  (undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :ghook ('after-init-hook #'global-undo-fu-session-mode)
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package visual-fill-column :defer)

(use-package rainbow-mode :defer)

(use-package rainbow-delimiters :defer)

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode t))

;;
;; Shell and Terminals
;;

(use-package fish-mode :defer)

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

(use-package vterm
  :pin "manual"
  :ghook ('vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  :general
  ("C-z" #'vterm)
  :config
  ;; (csetq vterm-keymap-exceptions (remove "C-u" vterm-keymap-exceptions))
  ;; custom doesn't work for this, it's a defvar not a defcustom
  (csetq vterm-timer-delay nil))

(use-package eterm-256color
  :ghook 'term-mode-hook)

;;
;; Debugging
;;

(use-package gud :ensure nil
  :defer
  :custom
  (gdb-many-windows t))

(use-package realgud :defer)

;;
;; Version Control
;;

(use-package magit
  :ghook ('git-commit-mode-hook #'git-commit-turn-on-flyspell)
  :general
  ("C-c g" #'magit-file-dispatch)
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

(use-package projectile
  :general
  ("M-p" 'projectile-command-map)
  :ghook ('after-init-hook #'projectile-mode))

(use-package vc :ensure nil
  :defer
  :custom
  (vc-follow-symlinks t)
  (vc-git-diff-switches
   '("--ignore-space-change" "--ignore-all-space"
     "--no-ext-diff" "--stat" "--diff-algorithm=histogram"))
  (vc-git-print-log-follow t)
  :config
  (push ".ccls-cache" vc-directory-exclusion-list)
  (push ".vscode" vc-directory-exclusion-list)
  (push ".cache" vc-directory-exclusion-list)
  (push ".idea" vc-directory-exclusion-list))

(use-package vc-msg
  :defer
  :custom
  (vc-msg-git-show-commit-function #'magit-show-commit))

;;
;; Misc
;;
(use-package diff-hl
  :after vc
  :custom
  (diff-hl-draw-borders t)
  (diff-hl-side 'left)
  :ghook ('after-init-hook #'global-diff-hl-mode))

;;
;; Misc (user-defined)
;;
(use-package user-advices :ensure nil)

;;
;; org. stuff
;;
(use-package calendar :ensure nil
  :defer
  :custom
  (calendar-week-start-day 1)
  (calendar-date-style 'iso))

(use-package org
  :pin "elpa"
  :general
  ("C-c n" #'org-capture
   "C-c a" #'org-agenda)
  (:keymaps 'org-mode-map
            "RET" #'org-return-and-maybe-indent)

  :ghook ('org-mode-hook (lambda () (toggle-word-wrap -1)))

  :custom
  ;; with modernize only
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis markers t)
  (org-pretty-entities t)

  (org-ellipsis "…")

  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\n  %i\n  %a" :empty-lines 1)
     ("n" "Note" entry (file "~/org/notes.org")
      "* NOTE %? %^G\n%U" :empty-lines 1))))

(use-package org-bullets
  :after org
  :ghook 'org-mode-hook)

(use-package org-modern
  :disabled
  :after org
  :ghook
  ('org-mode-hook #'org-modern-mode)
  ('org-agenda-finalized-hook #'org-modern-agenda)
  :custom
  (org-modern-hide-stars 'leading))

(use-package denote
  :ghook
  ('dired-mode-hook #'denote-dired-mode-in-directories)
  :custom
  (denote-known-keywords '("emacs" "nix" "work" "cpp" "tech" "general")))

(require 'feeds)

(provide 'init)
;;; init.el ends here

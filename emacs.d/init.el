;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; It's init.el, that says enough.

;;; Code:

;; Easier customization
(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defvar user/gc-cons-threshold (* 16 gc-cons-threshold))
(csetq gc-cons-threshold most-positive-fixnum)

(csetq user/file-name-handler-alist file-name-handler-alist)
(csetq file-name-handler-alist '())

(add-hook 'after-init-hook
          (lambda ()
            (csetq gc-cons-threshold user/gc-cons-threshold)

            ;; Don't just blindly set it to the old value, maybe someone decided to add something to it
            (csetq file-name-handler-alist (append file-name-handler-alist user/file-name-handler-alist))

            (message "Time to load init file: %s" (emacs-init-time))
            (garbage-collect)))

;; (if (display-graphic-p)
;;     (load-file (expand-file-name "exwm.el" user-emacs-directory))
;;   (message "exwm not available if emacs started without gui..."))

(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(defconst user/custom-file (expand-file-name "custom.el" user-emacs-directory)
  "File used to store settings from Customization UI.")

(when (file-exists-p user/custom-file)
  (load-file user/custom-file))

(csetq custom-file user/custom-file)
(csetq custom-buffer-done-kill t)
(csetq custom-unlispify-tag-names nil)
(csetq custom-unlispify-menu-entries nil)

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

(use-package parchment-theme
  :disabled
  :if (display-graphic-p)
  :config
  (load-theme 'parchment t))

(use-package eclipse-theme
  :if (display-graphic-p)
  :config
  (load-theme 'eclipse t))

;; (set-background-color "honeydew")

(use-package minions
  :custom
  (minions-mode-line-delimiters '("" . ""))
  :config
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

(use-package bug-hunter :defer t)

;;
;; Some default settings that I like
;;
(csetq initial-major-mode 'text-mode)
(csetq initial-scratch-message "")
(csetq inhibit-startup-buffer-menu t)
(csetq inhibit-splash-screen t)
(csetq inhibit-startup-echo-area-message t)
(csetq inhibit-startup-message t)
(fset 'display-startup-echo-area-message #'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)

(csetq enable-local-variables :safe)

(csetq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(csetq system-time-locale "C")

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

(csetq async-shell-command-display-buffer nil)
(csetq async-shell-command-buffer 'new-buffer)

(csetq read-file-name-completion-ignore-case t)

(csetq column-number-indicator-zero-based nil)

(csetq disabled-command-function nil)

;; "Smooth" mouse scrolling, one line at a time
(csetq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(csetq scroll-conservatively 10000)
(csetq scroll-preserve-screen-position t)
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
(csetq tramp-verbose 2)

(csetq uniquify-buffer-name-style 'post-forward)
(csetq uniquify-separator ":")
(csetq uniquify-ignore-buffers-re "^\\*")
(csetq uniquify-after-kill-buffer-p t)

(csetq auto-revert-avoid-polling t)
(global-auto-revert-mode t)

(csetq bookmark-save-flag t)

(csetq indicate-buffer-boundaries
       '((top . right)
         (bottom . right)
         (t . nil)))

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

(set-face-attribute 'default nil
                    :family "IBM Plex Mono"
                    :height 105
                    :weight 'normal)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 105
                    :weight 'normal)

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(size-indication-mode -1)
(line-number-mode t)
(column-number-mode t)
(csetq visible-cursor nil)

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

;;
;; Customization that doesn't require use-package
;;


;; When creating new buffers, use `auto-mode-alist' to automatically set the major mode.
(csetq major-mode (lambda ()
                    (unless buffer-file-name
                      (let ((buffer-file-name (buffer-name)))
                        (set-auto-mode)))))

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

(csetq savehist-additional-variables '(search-ring regexp-search-ring compile-command calc-stack))
(csetq savehist-ignored-variables '(tmm--history yes-or-no-p-history))
(csetq savehist-autosave-interval 60)
(csetq history-length 1000)
(csetq history-delete-duplicates t)
(savehist-mode t)

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

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun hide-trailing-whitespace ()
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

(add-hook 'minibuffer-setup-hook #'hide-trailing-whitespace)

;; Increase the memory while in the minibuffer
;; (add-hook 'minibuffer-setup-hook #'user/minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'user/minibuffer-exit-hook)

(general-define-key
 :keymaps '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map)
 "<escape>" #'minibuffer-keyboard-quit)

(with-eval-after-load 'xref
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

;; Thank you u/ouroboroslisp
(advice-add #'calculate-lisp-indent :override #'user/calculate-lisp-indent)

(defun user/calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists."
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

(defun occur-at-point ()
  "Just like `occur', but with the default value of symbol at point."
  (interactive)
  (let ((read-regexp-defaults-function 'find-tag-default-as-symbol-regexp))
    (call-interactively #'occur)))

;;
;; Some binds & configs that doesn't require use-package (well, we could require simple & misc, but they will be autloaded anyway)
;;

(general-unbind
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


;; (key-chord-define-global "jj" #'switch-to-buffer)
;; (key-chord-define-global "jf" #'find-file)
;; (key-chord-define-global "jg" #'find-file-other-window)

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

  :custom
  (whitespace-display-mappings
   '((tab-mark ?\t [187 32 32 32 32 32 32 32])))
  (whitespace-style '(face tab-mark trailing)))

(use-package recentf :ensure nil
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-exclude (list
                    "/usr/share/emacs/.*\\'"
                    "/elpa/.*\\'"               ; Package files
                    "/.ccls-cache/.*\\'"        ; Package files
                    "PKGBUILD"                  ; ArchLinux aur
                    "crontab.*"
                    "/usr/lib/python.*\\'"      ;
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
  (run-at-time (* 5 60) (* 5 60) #'recentf-save-list))

(use-package ibuffer :ensure nil
  :preface
  (defun ibuffer-switch-to-filter ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  :gfhook #'ibuffer-auto-mode #'ibuffer-switch-to-filter
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
    (let ((compilation-read-command (if ask t nil)))
      (if projectile-mode
          (projectile-compile-project compilation-read-command)
        (compile compile-command))))

  (defun user/switch-to-compilation-window (buffer _msg)
    (let ((bufwin (get-buffer-window buffer)))
      (when bufwin
        (select-window bufwin))))

  :general
  ([remap comment-region] #'compile-without-ask)
  ("C-c c" #'compile-without-ask)

  :ghook
  ('compilation-mode-hook #'hide-trailing-whitespace)
  ('compilation-finish-functions #'user/switch-to-compilation-window)

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
            "/" #'dired-narrow))

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
  :ghook 'text-mode-hook 'prog-mode-hook
  :general
  (:keymap 'symbol-overlay-map
           "M-*" #'symbol-overlay-put
           "M-n" #'symbol-overlay-jump-next
           "M-p" #'symbol-overlay-jump-prev
           "M-8" #'symbol-overlay-toggle-in-scope)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit underline))))
  :custom
  (symbol-overlay-idle-time 0.25)
  (symbol-overlay-displayed-window t))

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

(use-package ivy
  :general
  ("C-c C-r" #'ivy-resume)
  (:keymaps 'ivy-mode-map
            "<escape>" #'minibuffer-keyboard-quit)
  :custom
  (ivy-count-format "")
  (ivy-height 9)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  (ivy-wrap t)
  :config
  (ivy-mode t))

(use-package counsel
  :general
  (:prefix "M-s"
           "c" #'counsel-rg
           "g" #'counsel-git-grep
           "s" #'swiper)
  :custom
  (counsel-describe-function-preselect 'ivy-function-called-at-point)
  (counsel-grep-post-action-hook '(recenter))
  (counsel-mode-override-describe-bindings t)
  :config
  (counsel-mode t))

(use-package ivy-rich
  :custom
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode t))

(use-package ivy-posframe
  :disabled
  :custom
  (ivy-posframe-display-functions-alist
   '((swiper . nil)
     (t . ivy-posframe-display-at-point)))

  :config
  (ivy-posframe-mode t))

(use-package company
  :preface
  (defun disable-company-mode ()
    (company-mode -1))

  :general
  (:keymaps 'company-active-map
            "ESC" #'company-abort
            "<tab>" #'company-complete-selection
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "C-w" nil)

  :custom
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-occurrence-weight-function #'company-occurrence-prefer-any-closest)
  (company-transformers '(company-sort-by-occurrence))

  :init
  (global-company-mode t))

(use-package eacl
  :commands eacl-complete-line
  :general ("C-x C-l" #'eacl-complete-line))

;;
;; Extra modes
;;

(use-package cmake-mode :defer t)

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

(use-package rg
  :general
  (:prefix "M-s"
           "d" #'rg-dwim
           "r" #'rg
           "p" #'rg-project
           "l" #'rg-literal)
  :custom
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

(use-package elisp-mode :ensure nil
  :defer t
  :ghook ('after-save-hook #'check-parens))

(use-package lisp-mode :ensure nil
  :defer t
  :ghook ('after-save-hook #'check-parens))

(use-package paren :ensure nil
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  :config
  (show-paren-mode t))

(use-package comment-dwim-2
  :general ("M-;" #'comment-dwim-2))

(use-package counsel-etags :defer t)

(use-package dumb-jump
  :defer t
  :ghook ('dumb-jump-after-jump-hook #'recenter-top-bottom))

(use-package cc-mode :ensure nil
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
    (c-toggle-auto-newline t)
    (c-toggle-syntactic-indentation t)

    (setq-local c-hanging-semi&comma-criteria nil)

    (modify-syntax-entry ?' ".")

    (require 'ccls)

    (with-eval-after-load 'lsp-mode
      (setq-local flymake-diagnostic-functions (remove #'flymake-cc flymake-diagnostic-functions))))

  :ghook ('c-mode-common-hook #'user/c-mode-common-hook)

  :config
  (c-add-style "allman" user/allman-style)
  (c-add-style "sane-k&r" user/k&r-style)

  (csetq c-default-style '((java-mode . "java")
                           (awk-mode . "awk")
                           (other . "sane-k&r"))))

(use-package ccls
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
  (ccls-initialization-options
   '(:diagnostics (:onOpen 0 :onSave 0 :onChange -1 :spellChecking :json-false)
     :highlight (:largeFileSize 0))))

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

(use-package pipenv
  :ghook 'python-mode-hook
  :custom
  (pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

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

;; (use-package eglot
;;   :ghook
;;   ('c-mode-common-hook #'eglot-ensure)
;;   :gfhook
;;   ('eglot-server-initialized-hook #'company-eglot-setup)
;;   :preface
;;   (defun company-eglot-setup (_server)
;;     (delq 'company-capf company-backends)
;;     (push 'company-capf company-backends))
;;   :init
;;   (csetq eglot-ignored-server-capabilites '(:documentHighlightProvider))
;;   :config
;;   (push '((c++-mode c-mode)
;;           "ccls" "--init={\"diagnostics\": {\"onChange\": -1, \"spellChecking\": false}, \"highlight\": {\"largeFileSize\": 0}}")
;;         eglot-server-programs))

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
  (lsp-enable-indentation nil)
  (lsp-before-save-edits nil)
  (lsp-enable-symbol-highlighting nil "`symbol-overlay' it's more usefull (for me).")
  (lsp-auto-guess-root t "Sometimes it doesn't work, but it's ok")
  (lsp-pyls-plugins-rope-completion-enabled nil "This is very very slow (we allow jedi only)")

  ;; Enable this when things are slow
  ;; (lsp-print-performance t)

  :config
  (push "[/\\\\]\\.ccls-cache$" lsp-file-watch-ignored)
  (push "[/\\\\]\\.vscode$" lsp-file-watch-ignored)
  (push "[/\\\\]_build$" lsp-file-watch-ignored)

  (require 'lsp-clients)

  ;; Until I have a method of selecting the prefered lsp-client (per project or globally)
  (remhash 'clangd lsp-clients))

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
  (flymake-start-syntax-check-on-newline nil "Don't check on newlines (I hate it)"))

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
  :commands yas-minor-mode
  :ghook
  ('(text-mode-hook prog-mode-hook) #'yas-minor-mode)
  :custom
  (yas-verbosity 1 "Only errors")
  (yas-triggers-in-field t "Snippets inside snippets")
  (yas-wrap-around-region t)
  (yas-also-auto-indent-first-line t)
  :config
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

(use-package eyebrowse
  :preface
  (defhydra user/eyebrowse-hydra
    (:color pink
            :pre (eyebrowse-mode t))
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

  :general ("C-c w" #'user/eyebrowse-hydra/body)

  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-switch-back-and-forth t)
  (eyebrowse-wrap-around t))

(use-package projectile
  :demand t
  :general ("C-c p" 'projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-sort-order 'recentf)
  (projectile-use-git-grep t)

  :config
  (projectile-mode t)

  (push "_build" projectile-globally-ignored-directories)
  (push ".vscode" projectile-globally-ignored-directories)
  (push ".ccls-cache" projectile-globally-ignored-directories))

(use-package which-key
  :custom
  (which-key-side-window-location 'right)
  (which-key-idle-delay 0.75)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode t))

;;
;; Shell and Terminals
;;

(defun shell-like-mode-hook ()
  (disable-company-mode)
  (setq-local scroll-margin 0))

(add-hook 'term-mode-hook #'shell-like-mode-hook)

(use-package eshell :ensure nil
  :defer t
  :gfhook #'shell-like-mode-hook

  :ghook ('eshell-load-hook
          (lambda ()
            (push 'eshell-tramp eshell-modules-list)
            (push 'eshell-rebind eshell-modules-list)
            (push 'eshell-smart eshell-modules-list)
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

  :gfhook #'shell-like-mode-hook

  :custom
  ;; (multi-term-program "screen")
  ;; (multi-term-program-switches "-DR")
  (multi-term-dedicated-select-after-open-p t)
  (multi-term-scroll-show-maximum-output t))

(use-package shell :ensure nil
  :if (eq system-type 'gnu/linux)
  :defer t
  :ghook ('shell-mode-hook #'shell-like-mode-hook))

;;
;; Debugging
;;

(use-package gud :ensure nil
  :gfhook #'disable-company-mode
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
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-process-popup-time 20)
  (magit-refs-show-commit-count 'all))

(use-package magit-gitflow
  :ghook ('magit-mode-hook #'turn-on-magit-gitflow))

;;
;; System replacements
;;

(use-package daemons
  :commands (daemons daemons-start daemons-stop daemons-status)
  :custom
  (daemons-always-sudo t))

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

;;
;; org. stuff
;;
(use-package calendar :ensure nil
  :defer t
  :custom
  (calendar-week-start-day 1)
  (calendar-date-style 'iso))


(use-package org
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
;; mail
;;
(use-package mu4e
  :disabled
  :load-path "/usr/share/emacs/site-lisp/mu4e"

  :preface
  (defun user/mu4e-boring-function (msg param)
    (let ((value (or (mu4e-msg-field msg :subject) "")))
      (or (string-prefix-p "[Bamboo]" value)
          (and (string-prefix-p "[JIRA]" value)
               (not (string-prefix-p "[JIRA] Created" value))))))

  (defun mu4e-handle-boring-messages ()
    (interactive)

    (mu4e-headers-mark-for-each-if '(read . n) #'user/mu4e-boring-function)

    (when (hash-table-count mu4e~mark-map)
      (message "Marking %d boring messages as read..." (hash-table-count mu4e~mark-map))
      (mu4e-mark-execute-all t)

      (mu4e-headers-mark-for-each-if '(refile . y) #'user/mu4e-boring-function)

      (message "Moving %d boring messages..." (hash-table-count mu4e~mark-map))
      (mu4e-mark-execute-all t)))

  (defun mu4e-refile-all ()
    (interactive)
    (mu4e-headers-for-each
     (lambda (msg)
       (mu4e-headers-mark-for-refile)))
    (when mu4e~mark-map
      (message "Moving %d messages..." (hash-table-count mu4e~mark-map))
      (mu4e-mark-execute-all t)))

  :general
  ("C-x m" #'mu4e)
  (:keymaps 'mu4e-headers-mode-map
            "k" #'mu4e-handle-boring-messages
            "o" #'mu4e-refile-all)

  :custom
  (message-kill-buffer-on-exit t)

  (mu4e-maildir (expand-file-name "~/.mail"))
  (mu4e-view-show-images t)
  (mu4e-view-show-addresses t)
  (mu4e-update-interval 120)
  (mu4e-confirm-quit nil)
  (mu4e-attachment-dir (expand-file-name "~/Downloads"))
  (mu4e-change-filenames-when-moving t)
  (mu4e-context-policy 'pick-first)
  (mu4e-display-update-status-in-modeline t)
  (mu4e-use-fancy-chars t)
  (mu4e-save-multiple-attachments-without-asking t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-format-flowed t)
  (mu4e-completing-read-function 'completing-read)

  (mu4e-headers-include-related t)
  (mu4e-headers-auto-update t)
  (mu4e-headers-date-format "%d-%m-%Y %H:%M")
  (mu4e-headers-fields '((:human-date . 16) (:flags . 8) (:from . 30) (:subject . nil)))

  (mu4e-headers-unread-mark '("u" . "✉"))
  ;; TODO mu4e-view-attachment-assoc

  :config
  (load-file (expand-file-name "mu4e.el" user-emacs-directory)))


(provide 'init)

;;; init.el ends here

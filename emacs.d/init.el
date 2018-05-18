;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; It's init.el, that says enough.

;;; Code:

;; Easier customization
(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

;; Apparently, when using default values, the input lag and hangs disappears
;; (csetq gc-cons-threshold (* 384 1024 1024))

;; (defconst user-original-gc-cons (* 7 gc-cons-threshold)
;;   "My default value of `gc-cons-threshold'.")

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (csetq gc-cons-threshold user-original-gc-cons)))

(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(defconst user-custom-file (expand-file-name "custom.el" user-emacs-directory)
  "File used to store settings from Customization UI.")

(csetq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(csetq package-enable-at-startup nil)
(csetq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(add-hook 'after-init-hook (lambda () (message "Time to load init file: %s"
                                               (emacs-init-time))))

(package-initialize)

;; Install use-package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(csetq use-package-enable-imenu-support t)
(csetq use-package-expand-minimally t)
(require 'use-package)

(use-package diminish :ensure t)

(use-package ignoramus :ensure t
  :init
  (ignoramus-setup))

(when (= 25 emacs-major-version)
  (use-package tramp
    :config
    ;; Work-around for tramp which apparently doesn't know 'default
    (when (eq tramp-syntax 'default)
      (setq tramp-syntax 'ftp))))

(use-package no-littering :ensure t
  :config
  (use-package recentf
    :init
    (csetq recentf-max-saved-items 500)
    (csetq recentf-max-menu-items 20)
    (csetq recentf-exclude (list "/\\.git/.*\\'"        ; Git contents
                                 "/elpa/.*\\'"          ; Package files
                                 "PKGBUILD"             ; ArchLinux aur
                                 "COMMIT_MSG"
                                 "COMMIT_EDITMSG"
                                 "crontab.*"
                                 #'ignoramus-boring-p))
    ;; disable recentf-cleanup on Emacs start, because it can
    ;; cause problems with remote files
    (csetq recentf-auto-cleanup 'never)

    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)

    :config
    (recentf-mode t)))

;; Some special file names
(add-to-list 'auto-mode-alist '("\\.?bash.*" . shell-script-mode))

(use-package cus-edit
  :defer t
  :init
  (if (file-exists-p user-custom-file)
      (load-file user-custom-file))
  :config
  (csetq custom-file user-custom-file)
  (csetq custom-buffer-done-kill nil)
  (csetq custom-buffer-verbose-help nil)
  (csetq custom-unlispify-tag-names nil)
  (csetq custom-unlispify-menu-entries nil))

(use-package midnight
  :init
  (midnight-delay-set 'midnight-delay 1)
  (csetq midnight-period (* 15 60))

  ;; 5 minutes for special buffers
  (csetq clean-buffer-list-delay-special (* 5 60))

  (midnight-mode t)
  :config
  (add-to-list 'clean-buffer-list-kill-buffer-names
               '("*buffer-selection*"
                 "*Finder*"
                 "*Occur*"
                 "*rg*"
                 "*ripgrep-search*"
                 "*ag search*"
                 "*compilation*"
                 "*Help*"
                 "*Flycheck error messages*"
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
                 "\\`\\*\\(Wo\\)?Man .*\\*\\'")))

(use-package bug-hunter :ensure t :defer t)

(use-package hydra :ensure t
  :config
  (hydra-add-font-lock))

;; Setup the gui appearance
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(set-face-attribute 'default nil
                    :family "Iosevka" :height 105 :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :family "Noto Sans" :height 105 :weight 'regular)

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(size-indication-mode -1)
(line-number-mode t)
(column-number-mode t)
(csetq visible-cursor nil)

(use-package habamax-theme :ensure t
  :defer t
  :init
  (load-theme 'habamax t))

;; (use-package leuven-theme :ensure t
;;   :init
;;   (csetq leuven-scale-outline-headlines nil)
;;   (csetq leuven-scale-org-agenda-structure nil)
;;   (load-theme 'leuven t))

(csetq mode-line-position
       '((line-number-mode ("%l" (column-number-mode ":%2c")))))

(csetq mode-line-format
       '("%e"
         mode-line-front-space
         (anzu-mode
          (:eval
           (anzu--update-mode-line)))
         mode-line-client
         mode-line-modified
         mode-line-mule-info
         " "
         mode-line-position
         " (%o/%I) "
         mode-line-buffer-identification
         mode-line-modes
         (vc-mode vc-mode)
         " "
         mode-line-misc-info
         mode-line-end-spaces))

(show-paren-mode t)
;; (blink-cursor-mode t)

(use-package whitespace
  :diminish
  :hook (prog-mode . whitespace-mode)
  :init
  (csetq whitespace-style '(face tab-mark trailing))
  (csetq whitespace-display-mappings '((tab-mark ?\t [187 32 32 32 32 32 32 32]))))

(use-package hl-todo :ensure t
  :config
  (global-hl-todo-mode t))

(use-package visual-fill-column :ensure t :defer t)

(use-package user-sensible)

(use-package user-advices)

(use-package vlf :ensure t
  :config
  (require 'vlf-setup)
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))

(use-package uniquify
  :init
  (csetq uniquify-buffer-name-style 'post-forward)
  (csetq uniquify-separator ":")
  (csetq uniquify-ignore-buffers-re "^\\*")
  (csetq uniquify-after-kill-buffer-p t))

;; Disable auto-save-mode (I really hate it)
(auto-save-mode -1)
(csetq auto-save-default nil)

(when (>= emacs-major-version 25)
  (use-package saveplace
    :config
    (save-place-mode t)))

(use-package savehist
  :init
  (csetq savehist-additional-variables
         '(search-ring
           regexp-search-ring
           compile-command))
  (csetq savehist-autosave-interval 60)
  (csetq history-length 1000)
  :config
  (savehist-mode t))

(use-package bookmark
  :defer t
  :config
  (csetq bookmark-save-flag 1))

(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode)
  :config
  (csetq auto-revert-verbose nil)
  (csetq global-auto-revert-non-file-buffers t))

(use-package which-key :ensure t
  :diminish
  :init (which-key-mode t)
  :config
  (csetq which-key-idle-delay 0.5)
  (csetq which-key-sort-order 'which-key-prefix-then-key-order))

(use-package jump-char :ensure t
  :bind (("M-m" . #'jump-char-forward)
         ("M-M" . #'jump-char-backward))
  :init
  (unless (boundp 'lazy-highlight-face)
    (defvar lazy-highlight-face 'lazy-highlight
      "Was removed from emacs 26, so redefine it until jump-char fixes it.")))

(use-package expand-region :ensure t
  :bind (("M-2" . #'er/expand-region)     ; let it be overwritten in magit
         ("M-1" . #'er/contract-region)
         ("M-@" . #'er/contract-region)
         ("M-e" . #'user-expand-region/body))
  :init
  (csetq expand-region-fast-keys-enabled nil)
  (csetq expand-region-autocopy-register "e")

  (defhydra user-expand-region (:exit t)
    "Mark region"
    ("c" er/contract-region "contract" :exit nil)
    ("e" er/expand-region "expand" :exit nil)
    ("u" er/mark-url "url")
    ("w" er/mark-word "word")
    ("." er/mark-symbol "symbol")
    (";" er/mark-comment "comment")
    ("d" er/mark-defun "defun")
    ("p" er/mark-inside-pairs "inside pairs")
    ("P" er/mark-outside-pairs "outside pairs")
    ("'" er/mark-inside-quotes "inside quotes")
    ("\"" er/mark-outside-quotes "outside quotes")
    ("q" nil "quit")))

(use-package smartparens :ensure t
  :diminish
  :demand t
  :bind (("C-M-k" . #'sp-kill-sexp)
         ("C-M-n" . #'sp-next-sexp)
         ("C-M-p" . #'sp-previous-sexp)
         ("C-M-f" . #'sp-forward-sexp)
         ("C-M-b" . #'sp-backward-sexp))
  :hook ((prog-mode . smartparens-mode)
         (minibuffer-setup . turn-on-smartparens-strict-mode))
  :config
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

  (require 'smartparens-config)

  (sp-local-pair 'c-mode "{" nil :post-handlers '(:add user-open-block-c-mode))
  (sp-local-pair 'c++-mode "{" nil :post-handlers '(:add user-open-block-c-mode))

  (show-smartparens-global-mode t))

  ;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode))

(use-package undo-tree :ensure t
  :diminish
  :init
  (csetq undo-tree-visualizer-diff t)
  (csetq undo-tree-visualizer-timestamps t)
  (csetq undo-tree-auto-save-history nil)

  (global-undo-tree-mode t)

  ;; Keep region when undoing in region
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

(use-package volatile-highlights :ensure t
  :diminish
  :config
  (volatile-highlights-mode t)
  (csetq Vhl/highlight-zero-width-ranges t)

  (with-eval-after-load "undo-tree"
    (vhl/define-extension 'vhl-undo-tree #'undo-tree-move #'undo-tree-undo #'undo-tree-redo)
    (vhl/install-extension 'vhl-undo-tree)))

(use-package misc
  :bind (("M-z" . #'zap-up-to-char)
         ("<C-right>" . #'forward-to-word)))

(use-package mouse-copy)

(use-package ibuffer
  :bind (("C-x C-b" . #'ibuffer))
  :config
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

  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)      ;auto update
               (ibuffer-switch-to-saved-filter-groups "default"))))

(csetq display-buffer-alist
       `((,(rx bos
               (or "*Compile-Log*"                 ; Emacs byte compiler log
                   "*Warnings*"                    ; Emacs warnings
                   "*compilation"                  ; Compilation buffers
                   "*Flycheck errors*"             ; Flycheck error list
                   "*rg*"                          ; Ripgrep search results
                   "*ripgrep-search*"              ; Ripgrep search results
                   "*ag search*"                   ; Ag search results
                   "*Occur*"
                   (and (1+ nonl) " output*")      ; AUCTeX command output
                   ))
          (display-buffer-reuse-window
           display-buffer-in-side-window)
          (side            . bottom)
          (reusable-frames . nil)
          (window-height   . 0.25))
         ;; Let `display-buffer' reuse visible frames for all buffers. This must be
         ;; the last entry in `display-buffer-alist', because it overrides any later
         ;; entry with more specific actions.
         ("." nil (reusable-frames . nil))))

(use-package user-utils
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
         ("M-'" . #'user-open-terminal)
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
  :bind (([remap scroll-up-command] . #'user-scroll-half-page-up)
         ([remap scroll-down-command] . #'user-scroll-half-page-down)
         ("C-c w d" . #'user-toggle-current-window-dedication)
         ("C-c w q" . #'user-quit-all-side-windows)
         ("C-c w b" . #'user-switch-to-minibuffer))
  :init
  (bind-key "C-c w w" #'other-window)
  (bind-key "C-c w =" #'balance-windows)
  (bind-key "C-c w k" #'delete-window)
  (bind-key "C-c w /" #'split-window-right)
  (bind-key "C-c w \\" #'split-window-right)
  (bind-key "C-c w -" #'split-window-below)
  (bind-key "C-c w m" #'delete-other-windows)
  (bind-key "C-c w f" #'toggle-frame-fullscreen)
  (bind-key "C-c w 1" #'delete-other-windows))

(use-package windmove
  :bind (("C-c w <left>"  . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>"    . windmove-up)
         ("C-c w <down>" . windmove-down)))

;; (use-package user-files
;;   :bind (("C-c f D" . user-delete-file-and-buffer)
;;          ("C-c f r" . user-rename-file-and-buffer)))

(use-package projectile :ensure t
  :diminish
  :init
  (csetq projectile-completion-system 'ivy)
  (csetq projectile-indexing-method 'alien)
  (csetq projectile-enable-caching t)
  (csetq projectile-use-git-grep t)

  (projectile-mode t)

  :config
  (add-to-list 'projectile-globally-ignored-directories ".vscode")
  (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index"))

;; Searching
(defun user-results-buffer-hook ()
  "Set various settings on results buffers (compilation, grep, etc.)."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local show-trailing-whitespace nil))

(use-package grep
  :defer t
  :config
  :hook (grep-mode . user-results-buffer-hook))

(use-package ag :ensure t
  :bind (("C-c s a" . ag))
  :init
  (csetq ag-reuse-buffers t)
  (csetq ag-reuse-window t)
  :config
  (dolist (ign-file grep-find-ignored-files)
    (add-to-list 'ag-ignore-list ign-file))

  (dolist (ign-dir grep-find-ignored-directories)
    (add-to-list 'ag-ignore-list ign-dir)))

(use-package wgrep :ensure t
  :defer t
  :init
  (csetq wgrep-auto-save-buffer t))

(use-package wgrep-ag :ensure t :defer t)

(use-package anzu :ensure t
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ("C-c r" . anzu-query-replace-at-cursor-thing)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :demand t
  :diminish
  :init
  (csetq anzu-replace-to-string-separator " => ")
  :config
  (csetq anzu-cons-mode-line-p nil)
  (global-anzu-mode t))

(use-package ripgrep :ensure t
  :bind (("C-c s s" . user-ripgrep)
         ("C-c s r" . ripgrep-regexp))
  :init
  (use-package projectile-ripgrep :ensure t
    :bind (("C-c p s s" . user-projectile-ripgrep)
           ("C-c p s r" . projectile-ripgrep)))

  (use-package wgrep-ack :ensure t
    :defer t
    :init
    :hook (ripgrep-search-mode . wgrep-ack-and-a-half-setup))

  :config
  (defvar user-rg-type-aliases (make-hash-table :test 'equal)
    "Make a hash-table to map `major-mode' to 'rg --type-list' values.")

  (puthash "nasm-mode" "asm" user-rg-type-aliases)
  (puthash "asm-mode" "asm" user-rg-type-aliases)
  (puthash "awk-mode" "awk" user-rg-type-aliases)
  (puthash "c-mode" "c" user-rg-type-aliases)
  (puthash "c++-mode" "cpp" user-rg-type-aliases)
  (puthash "cmake-mode" "cmake" user-rg-type-aliases)
  (puthash "css-mode" "css" user-rg-type-aliases)
  (puthash "emacs-lisp-mode" "elisp" user-rg-type-aliases)
  (puthash "go-mode" "go" user-rg-type-aliases)
  (puthash "html-mode" "html" user-rg-type-aliases)
  (puthash "java-mode" "java" user-rg-type-aliases)
  (puthash "js2-mode" "js" user-rg-type-aliases)
  (puthash "javascript-mode" "js" user-rg-type-aliases)
  (puthash "json-mode" "json" user-rg-type-aliases)
  (puthash "lisp-mode" "lisp" user-rg-type-aliases)
  (puthash "python-mode" "py" user-rg-type-aliases)
  (puthash "perl-mode" "perl" user-rg-type-aliases)
  (puthash "makefile-mode" "mk" user-rg-type-aliases)
  (puthash "ruby-mode" "ruby" user-rg-type-aliases)
  (puthash "shell-script-mode" "sh" user-rg-type-aliases)
  (puthash "sql-mode" "sql" user-rg-type-aliases)
  (puthash "TeX-mode" "tex" user-rg-type-aliases)
  (puthash "tex-mode" "tex" user-rg-type-aliases)
  (puthash "LaTeX-mode" "tex" user-rg-type-aliases)
  (puthash "latex-mode" "tex" user-rg-type-aliases)

  (defun user-call-ripgrep (func)
    "Call `FUNC' after setting `ripgrep-arguments' to '--type' based on major mode.
See `user-rg-type-aliases' for more details."
    (let ((type-rg (gethash (symbol-name major-mode) user-rg-type-aliases)))
      (if type-rg
          (let ((ripgrep-arguments '()))
            (add-to-list 'ripgrep-arguments (concat "--type " type-rg))
            (call-interactively func))
        (call-interactively func))))

  (defun user-ripgrep ()
    "Call `user-call-ripgrep' for `ripgrep-regexp'."
    (interactive)
    (user-call-ripgrep #'ripgrep-regexp))

  (defun user-projectile-ripgrep ()
    "Call `user-call-ripgrep' for `projectile-ripgrep'."
    (interactive)
    (user-call-ripgrep #'projectile-ripgrep)))

(use-package company :ensure t
  :diminish
  :bind (("C-j" . company-complete)
         :map company-active-map
         ("ESC" . company-abort)
         ("C-l" . company-show-location)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-w" . nil))
  :hook (after-init . global-company-mode)
  :init
  (csetq company-dabbrev-downcase nil)
  (csetq company-dabbrev-ignore-case t)

  (csetq company-dabbrev-code-ignore-case t)

  (csetq company-transformers '(company-sort-by-occurrence))
  (csetq company-idle-delay 0)
  (csetq company-minimum-prefix-length 3)
  (csetq company-tooltip-align-annotations t)
  (csetq company-selection-wrap-around t)

  (use-package user-completion
    :bind (("C-c /" . user-complete-line))))

;; (csetq completion-ignore-case t)

(require 'dabbrev)
(csetq dabbrev-case-replace nil)
(csetq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*$]")
(add-hook 'find-file-hook (lambda () (abbrev-mode -1)))

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :config
  :hook (prog-mode . yas-minor-mode)
  (yas-reload-all))

(use-package dired
  :defer t
  :bind (:map dired-mode-map
         ("C-c C-w" . wdired-change-to-wdired-mode))
  :hook (dired-mode . user-dired-hook)
  :init
  (defun user-dired-hook ()
    (toggle-truncate-lines))

  (csetq dired-listing-switches "-laGh1v --group-directories-first")
  (csetq dired-dwim-target t)
  (csetq dired-auto-revert-buffer t)
  (csetq dired-ls-F-marks-symlinks t)
  (csetq dired-recursive-copies 'always)

  :config
  (use-package dired+
    :bind (:map dired-mode-map
           ("SPC" . dired-mark)
           ("<M-right>" . diredp-find-file-reuse-dir-buffer)
           ("<M-left>" . diredp-up-directory-reuse-dir-buffer)
           ("<C-prior>"  . diredp-up-directory-reuse-dir-buffer))
    :demand t
    :config
    (csetq diredp-hide-details-initially-flag nil)
    (csetq diredp-dwim-any-frame-flag t)
    (diredp-toggle-find-file-reuse-dir t))

  (use-package dired-narrow :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow))))

(use-package selected :ensure t
  :diminish selected-minor-mode
  :init
  (selected-global-mode t))

(use-package multiple-cursors :ensure t
  :bind (("C-," . mc/unmark-next-like-this)
         ("C-<" . mc/unmark-previous-like-this)
         ("C-." . mc/mark-next-like-this)
         ("C->" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c m m" . mc/mark-all-dwim)
         ("C-c m d" . mc/mark-all-symbols-like-this-in-defun)
         ("C-c m i" . mc/insert-numbers)
         ("C-c m w" . mc/mark-all-words-like-this)
         ("C-c m h" . mc-hide-unmatched-lines-mode)
         ("C-c m l" . mc/edit-lines)
         ("C-c m n" . mc/skip-to-next-like-this)
         ("C-c m p" . mc/skip-to-previous-like-this)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-end-of-lines)
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
  :config
  (defun mc-prompt-once-advice (fn &rest args)
    (setq mc--this-command (lambda () (interactive) (apply fn args)))
    (apply fn args))

  (defun mc-prompt-once (&rest fns)
    (dolist (fn fns)
      (advice-add fn :around #'mc-prompt-once-advice)))

  (mc-prompt-once #'zap-up-to-char #'sp-rewrap-sexp))

(use-package smex :ensure t
  :defer t
  :config
  (smex-initialize))

(use-package ivy :ensure t
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-c v s" . ivy-push-view)
         ("C-c v p" . ivy-pop-view)
         :map ivy-mode-map
         ([escape] . user-minibuffer-keyboard-quit))
  :init
  (csetq ivy-on-del-error-function nil) ; don't exit with backspace
  (csetq ivy-display-style 'fancy)
  (csetq ivy-use-virtual-buffers t)
  (csetq ivy-count-format "(%d/%d) ")
  (csetq ivy-height 11)
  (csetq ivy-wrap t)
  (csetq ivy-dynamic-exhibit-delay-ms 200)
  (csetq ivy-use-selectable-prompt t)
  :config
  (ivy-mode t))

(use-package counsel :ensure t
  :diminish
  :bind (("C-c f r" . counsel-recentf)
         ("C-c f f" . counsel-find-file)
         ("C-c s c" . counsel-rg))
  :init
  (csetq counsel-ag-base-command "ag --ignore tags --ignore TAGS --ignore elpa --vimgrep %s")
  (counsel-mode t))

(use-package comint
  :bind (:map comint-mode-map
         ("<down>" . comint-next-input)
         ("<up>" . comint-previous-input)
         ("C-n" . comint-next-input)
         ("C-p" . comint-previous-input)
         ("C-r" . comint-history-isearch-backward))
  :init
  (csetq comint-process-echoes t)
  (csetq comint-prompt-read-only t)
  (csetq comint-history-isearch t)
  (csetq comint-ignore-dups t)

  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(use-package comment-dwim-2 :ensure t
  :bind (("M-;" . #'comment-dwim-2)
         :map selected-keymap
         (";" . #'comment-dwim-2)))

(defun user-programming-setup ()
  "Settings for all programming modes."
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'user-programming-setup)

(use-package editorconfig :ensure t
  :diminish
  :config
  :hook (prog-mode . editorconfig-mode))

(use-package which-func
  :init
  (which-function-mode t))

(use-package hexl
  :init
  (csetq hexl-bits 8))

(use-package highlight-symbol :ensure t
  :diminish
  :hook ((find-file . highlight-symbol-mode)
         (find-file . highlight-symbol-nav-mode))
  :init
  (csetq highlight-symbol-idle-delay 0.2)
  (csetq highlight-symbol-highlight-single-occurrence nil))

(use-package compile
  :diminish compilation-in-progress
  :bind (("C-c c" . compile))
  :hook ((compilation-start . user-compile-start)
         (compilation-filter . user-colorize-compilation-buffer)
         (compilation-mode . user-results-buffer-hook))
  :config
  (csetq compilation-scroll-output 'first-error)
  (csetq compilation-always-kill t)
  (csetq compilation-disable-input t)
  (csetq compilation-context-lines 3)
  (csetq compilation-auto-jump-to-first-error nil)

  (defvar user-compile-process nil
    "The current compilation process or nil if none.")

  (defun user-delete-window (window)
    "Kill the WINDOW, ignoring errors."
    (ignore-errors
      (delete-window window)))

  (defun user-compile-start (proc)
    (when (string-equal (buffer-name (current-buffer)) "*compilation*")
      (setq user-compile-process proc)))

  (defun user-compile-done (buffer _msg)
    (when (string-equal "*compilation*" (buffer-name buffer))
      (let ((exit-status (process-exit-status user-compile-process))
            (has-errors)
            (window (get-buffer-window buffer)))

        (setq has-errors
              (if (= 0 exit-status)
                  (save-mark-and-excursion
                    (condition-case nil
                        (progn
                          (first-error)
                          t)
                      (error nil)))
                t))

        (when (and window (not has-errors))
          (run-with-timer 1 nil #'user-delete-window window)))

      (setq user-compile-process nil)))

  (add-hook 'compilation-finish-functions #'user-compile-done)

  (require 'ansi-color)

  (defun user-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer.
Taken from http://stackoverflow.com/a/3072831/355252."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(use-package multi-term :ensure t
  :if (eq system-type 'gnu/linux)
  :bind (("C-; C-;" . multi-term-next)
         ("C-; c" . multi-term)
         ("C-; d" . multi-term-dedicated-toggle)
         ("C-; n" . multi-term-next)
         ("C-; p" . multi-term-prev))
  :hook (term-mode . user-term-mode-hook)
  :init
  (csetq multi-term-dedicated-select-after-open-p t)

  (defun user-term-mode-hook ()
    (company-mode -1)
    (setq-local scroll-margin 0)))

(use-package eshell
  :defer t
  :hook (eshell-mode . user-eshell-hook)
  :config
  (defun user-eshell-hook ()
    (company-mode -1)
    (setq-local scroll-margin 0)))

;; Error checking
(use-package flycheck :ensure t
  :bind (("C-c e" . user-flycheck-errors/body)
         ("C-c t f" . flycheck-mode))
  :init
  (global-flycheck-mode t)
  :config
  (defhydra user-flycheck-errors ()
    "Flycheck errors"
    ("c" flycheck-buffer "check" :exit t)
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list" :exit t)
    ("w" flycheck-copy-errors-as-kill "copy message")
    ("q" nil "quit"))
  (csetq flycheck-check-syntax-automatically '(save mode-enabled))
  (csetq flycheck-standard-error-navigation nil)
  (csetq flycheck-display-errors-function
         #'flycheck-display-error-messages-unless-error-list))

;; generic programming
(use-package lsp-mode :ensure t
  :hook (ls-after-open . lsp-enable-imenu)
  :init
  (csetq lsp-highlight-symbol-at-point nil)

  :config
  (use-package company-lsp :ensure t
    :hook (lsp-mode . user-lsp-mode-hook)
    :init
    (csetq company-lsp-async t)
    (csetq company-lsp-cache-candidates nil)
    (csetq company-lsp-enable-recompletion t)
    (defun user-lsp-mode-hook ()
      (add-to-list 'company-backends 'company-lsp)))

  (use-package lsp-ui :ensure t
    :hook (lsp-mode . lsp-ui-mode)
    :init
    ;; Disable type information shown in sideline (enable it per mode if needed)
    ;; Allow it to display linting errors only.
    (csetq lsp-ui-sideline-show-hover nil)
    (csetq lsp-ui-sideline-show-symbol nil)

    (csetq lsp-ui-flycheck-live-reporting nil)))

(use-package xref
  :init
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

(use-package origami :ensure t
  :hook (prog-mode . origami-mode)
  :bind (("C-x n n" . origami-forward-toggle-node)
         ("C-x n t" . origami-recursively-toggle-node)
         ("C-x n a" . origami-toggle-all-nodes)
         ("C-x n o" . origami-open-node)
         ("C-x n c" . origami-close-node)
         ("C-x n u" . origami-undo)
         ("C-x n _" . origami-redo)
         ("C-x n r" . origami-reset)))

;; C/C++
(use-package user-c)

(use-package nasm-mode :ensure t :defer t)

;; cmake
(use-package cmake-mode :ensure t :defer t
  :mode "CMakeLists\\.txt\\'")

;; Debugging
(use-package gud
  :defer t
  :hook (gud-mode . user-gud)
  :config
  (csetq gdb-many-windows t)

  (defun user-gud ()
    "Hook to run when GUD mode is activated."
    (company-mode -1)))

(use-package realgud :ensure t
  :commands (realgud:bashdb realgud:gdb realgud:gub realgud:ipdb
             realgud:jdb realgud:kshdb realgud:nodejs realgud:pdb
             realgud:perldb realgud:zshdb))

;; Web stuff
(use-package js2-mode :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (csetq js2-skip-preprocessor-directives t))

(use-package json-mode :ensure t
  :bind (:map json-mode-map
         ("M-q" . json-reformat-region))
  :config
  (csetq json-reformat:indent-width 4)
  (csetq json-reformat:pretty-string? t))

(use-package web-mode :ensure t
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
  :init
  (defun user-web-mode-hook ()
    "Hook to run when `web-mode' is active."
    (smartparens-mode -1))

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

;; Shell
(use-package sh-script
  :init
  (csetq sh-basic-offset 2))

;; Python
(use-package cython-mode :ensure t :defer t)

(use-package lsp-python :ensure t
  :hook (python-mode . lsp-python-enable))

(use-package pyvenv :ensure t
  :hook (python-mode . user-auto-virtualenv)
  :init
  (defun user-auto-virtualenv ()
    (pyvenv-mode t)

    ;; A dolist would be appropriate, but I only use venv as virtualenv name
    ;; This also works with lsp-mode since it will use the python inside
    (let ((root (locate-dominating-file default-directory "venv")))
      (if (and root (file-exists-p root))
          (pyvenv-activate (expand-file-name "venv" root))))))

;; imenu
(use-package imenu
  :bind ("M-i" . imenu)
  :hook (imenu-after-jump . recenter-top-bottom)
  :init
  (csetq imenu-auto-rescan t)
  (csetq imenu-auto-rescan-maxout (* 1024 1024)))

(use-package imenu-anywhere :ensure t
  :bind (("M-I" . imenu-anywhere)))

;; Git
(use-package magit :ensure t
  :bind (("C-x g" . magit-status))
  :init
  (csetq magit-display-buffer-function
         #'magit-display-buffer-fullframe-status-v1)
  (csetq magit-completing-read-function #'magit-builtin-completing-read)

  :config
  (use-package magit-gitflow :ensure t
    :defer t
    :hook (magit-mode . turn-on-magit-gitflow)))

;; Keybindings
(bind-key "C-c t d" #'toggle-debug-on-error)
(bind-key "C-c t q" #'toggle-debug-on-quit)
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key "M-g" #'goto-line)
(bind-key "C-8" #'repeat-complex-command)

(bind-key "C-o" #'isearch-occur isearch-mode-map)
(bind-key "<tab>" #'isearch-repeat-forward isearch-mode-map)
(bind-key "<backtab>" #'isearch-repeat-backward isearch-mode-map)

(use-package key-chord :ensure t
  :init
  (csetq key-chord-two-keys-delay 0.1)
  (csetq key-chord-one-key-delay 0.2)

  (key-chord-mode t)

  (use-package key-seq :ensure t)

  (key-seq-define-global "jc" #'compile)
  (key-seq-define-global "jj" #'ivy-switch-buffer)
  (key-seq-define-global "jf" #'counsel-find-file))

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

(provide 'init)

;;; init.el ends here

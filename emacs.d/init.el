;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; It's init.el, that says enough.

;;; Code:

;; Easier customization
(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

;; Will reset at the end of loading
(defconst user-original-gc-cons (* 5 gc-cons-threshold)
  "The original/default value of `gc-cons-threshold'.")

(csetq gc-cons-threshold (* 128 1024 1024))

(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(defconst user-cache-directory (expand-file-name ".cache" user-emacs-directory)
  "Directory to save recent files, etc.")

(defconst user-is-windows (eq system-type 'windows-nt)
  "Set when the OS is windows.")

(defconst user-is-cygwin (eq system-type 'cygwin)
  "Set when the OS is cygwin.")

(defconst user-is-linux (eq system-type 'gnu/linux)
  "Set when the OS is linux.")

(defconst user-custom-file (expand-file-name "custom.el" user-emacs-directory)
  "File used to store settings from Customization UI.")

(if user-is-cygwin
    (csetq source-directory "/cygdrive/d/tools/prog/src/emacs/")
  (csetq source-directory "D:/tools/prog/src/emacs/"))

(add-hook 'after-init-hook
          (lambda ()
            (csetq gc-cons-threshold user-original-gc-cons)))

(csetq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(csetq package-enable-at-startup nil)
(csetq load-prefer-newer t)

(when user-is-windows
  ;; Use mingw64 & msys2 first
  (setenv "PATH"
          (concat
           "d:\\tools\\prog\\x64\\msys64\\mingw64\\bin" ";"
           "d:\\tools\\prog\\x64\\msys64\\usr\\bin" ";"
           (getenv "PATH")))

  (csetq exec-path (append '("d:/tools/prog/x64/msys64/mingw64/bin"
                             "d:/tools/prog/x64/msys64/usr/bin")
                           exec-path)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(add-hook 'after-init-hook (lambda ()
                             (message "Time to load init file: %s"
                                      (emacs-init-time))))

(package-initialize)

;; Install use-package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(csetq use-package-enable-imenu-support t)

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

;; (autoload 'rabin-mode "rabin-mode.el" "" t nil)

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))

(setq default-frame-alist '((height . 55)
                            (width . 125)))

(defun user-gui ()
  "Setups the gui appearance.
Runs after init is done."
  (when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

  (set-face-attribute 'default nil
                      :family "Cousine" :height 110)
  (set-face-attribute 'variable-pitch nil
                      :family "Noto Sans" :height 110 :weight 'regular)

  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend))

  (size-indication-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (csetq visible-cursor nil)

  (use-package gruvbox-theme
    :ensure t
    :config
    (load-theme 'gruvbox t)))

(add-hook 'after-init-hook #'user-gui)

(csetq fast-but-imprecise-scrolling t)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-compile
   'user
   ;; Left side of the mode line (all the important stuff)
   '(((buffer-modified buffer-size input-method) :face highlight-face)
     anzu
     '(buffer-id remote-host buffer-encoding-abbrev)
     ((point-position line-column buffer-position selection-info)
      :separator " | ")
     major-mode
     process
     (flycheck-error flycheck-warning flycheck-info)
     (python-pyvenv :fallback python-pyenv)
     ((which-function projectile-root) :separator " @ ")
     ((minor-modes :separator spaceline-minor-modes-separator) :when active))
   ;; Right segment (the unimportant stuff)
   '((version-control :when active)))
  (csetq powerline-default-separator 'utf-8)
  (csetq mode-line-format '("%e" (:eval (spaceline-ml-user)))))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

(global-hl-line-mode)

(when user-is-linux
  (global-linum-mode t))

(use-package visual-fill-column
  :ensure t
  :defer t)

(use-package user-sensible)

(use-package user-advices)

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

(use-package uniquify
  :init
  (csetq uniquify-buffer-name-style 'post-forward)
  (csetq uniquify-separator ":")
  (csetq uniquify-ignore-buffers-re "^\\*")
  (csetq uniquify-after-kill-buffer-p t))

;; Disable this mode!
(csetq auto-save-file-name-transforms `((".*" ,(expand-file-name "saves" user-cache-directory) t)))
(csetq auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" user-cache-directory))
(auto-save-mode -1)
(csetq auto-save-default nil)

(when (>= emacs-major-version 25)
  (use-package saveplace
    :init
    (csetq save-place-file (expand-file-name "places" user-cache-directory))
    :config
    (save-place-mode t)))

(use-package savehist
  :init
  (csetq savehist-file (expand-file-name "history" user-cache-directory))
  (csetq savehist-additional-variables
         '(search-ring
           regexp-search-ring
           compile-command))
  (csetq savehist-autosave-interval 60)
  (csetq history-length 1000)
  :config
  (savehist-mode t))

(use-package recentf
  :init
  (csetq recentf-save-file (expand-file-name "recentf" user-cache-directory))
  (csetq recentf-max-saved-items 500)
  (csetq recentf-max-menu-items 20)
  (csetq recentf-exclude (list "/\\.git/.*\\'"   ; Git contents
                               "/elpa/.*\\'"      ; Package files
                               "PKGBUILD"         ; ArchLinux aur
                               "COMMIT_MSG"
                               "COMMIT_EDITMSG"
                               "crontab.*"
                               #'ignoramus-boring-p))
  ;; disable recentf-cleanup on Emacs start, because it can
  ;; cause problems with remote files
  (csetq recentf-auto-cleanup 'never)
  :config
  (recentf-mode t))

(use-package tramp
  :defer t
  :init
  (csetq tramp-persistency-file-name (expand-file-name "tramp" user-cache-directory)))

(use-package bookmark
  :defer t
  :config
  (csetq bookmark-save-flag 1)
  (csetq bookmark-default-file (expand-file-name "bookmarks" user-cache-directory)))

(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode)
  :config
  (csetq auto-revert-verbose nil)
  (csetq global-auto-revert-non-file-buffers t))

(use-package ignoramus
  :ensure t
  :config
  (ignoramus-setup))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode t)
  :config
  (csetq which-key-idle-delay 0.5)
  (csetq which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-setup-side-window-bottom))
  ;;(which-key-setup-side-window-right-bottom))

(use-package jump-char
  :ensure t
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-in-line)
         ("C-;" . avy-goto-word-1)
         ("C-\"" . avy-goto-char-timer))
  :init
  (csetq avy-background t)
  (csetq avy-all-windows nil)
  (csetq avy-all-windows-alt 'all-frames)
  :config
  (avy-setup-default))

(use-package expand-region
  :ensure t
  :bind (("M-2" . er/expand-region)
         ("M-@" . er/contract-region)
         ("C-c m r" . user-expand-region/body))
  :init
  (csetq expand-region-fast-keys-enabled nil)
  (csetq er--show-expansion-message t)
  (defhydra user-expand-region (:exit t)
    "Mark region"
    ("c" er/contract-region "contract" :exit nil)
    ("r" er/expand-region "expand" :exit nil)
    ("u" er/mark-url "url")
    ("e" er/mark-email "email")
    ("w" er/mark-word "word")
    ("." er/mark-symbol "symbol")
    (";" er/mark-comment "comment")
    ("d" er/mark-defun "defun")
    ("p" er/mark-inside-pairs "inside pairs")
    ("P" er/mark-outside-pairs "outside pairs")
    ("'" er/mark-inside-quotes "inside quotes")
    ("\"" er/mark-outside-quotes "outside quotes")
    ("q" nil "quit")))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (csetq Vhl/highlight-zero-width-ranges t)

  (vhl/define-extension 'vhl-undo-tree #'undo-tree-move #'undo-tree-undo #'undo-tree-redo)
  (vhl/install-extension 'vhl-undo-tree))

(use-package move-text
  :ensure t
  :bind (("<M-up>" . move-text-up)
         ("<M-down>" . move-text-down)))

(use-package smartparens
  :diminish smartparens-mode
  :demand t
  :bind (("C-M-k" . sp-kill-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp))
  :ensure t
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

  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook #'smartparens-mode))

  ;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (csetq undo-tree-visualizer-diff t)
  (csetq undo-tree-visualizer-timestamps t)
  (csetq undo-tree-auto-save-history t)
  (csetq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" user-cache-directory))))
  :config
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

(use-package misc
  :bind (("M-z" . zap-up-to-char)
         ("<C-right>" . forward-to-word)))

(use-package mouse-copy)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
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
                              (mode . ruby-mode)
                              (mode . perl-mode)
                              (mode . python-mode)
                              (mode . js-mode)
                              (mode . js2-mode)
                              (mode . css-mode)
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
                   "*ag search*"                   ; Ag search results
                   (and (1+ nonl) " output*")      ; AUCTeX command output
                   ))
          (display-buffer-reuse-window
           display-buffer-in-side-window)
          (side            . bottom)
          (reusable-frames . visible)
          (window-height   . 0.25))
         ;; Let `display-buffer' reuse visible frames for all buffers. This must be
         ;; the last entry in `display-buffer-alist', because it overrides any later
         ;; entry with more specific actions.
         ("." nil (reusable-frames . nil))))

(use-package user-utils
  :bind (("<C-return>" . user-open-line-above)
         ("C-a" . user-smarter-move-beginning-of-line)
         ("C-w" . user-smarter-kill-word-or-region)
         ([remap backward-kill-word] . user-smarter-backward-kill-word)
         ("M-]" . user-next-error)
         ("M-[" . user-prev-error)
         ([remap forward-paragraph] . user-forward-paragraph)
         ([remap backward-paragraph] . user-backward-paragraph)
         ("M-j" . user-join-line)
         :map isearch-mode-map
         ("<backspace>" . user-isearch-delete)
         :map minibuffer-local-map
         ("<escape>" . user-minibuffer-keyboard-quit)
         :map minibuffer-local-ns-map
         ("<escape>" . user-minibuffer-keyboard-quit)
         :map minibuffer-local-completion-map
         ("<escape>" . user-minibuffer-keyboard-quit)
         :map minibuffer-local-must-match-map
         ("<escape>" . user-minibuffer-keyboard-quit)
         :map minibuffer-local-isearch-map
         ("<escape>" . user-minibuffer-keyboard-quit)))

(use-package user-window
  :bind (("C-c w d" . user-toggle-current-window-dedication)
         ("C-c w q" . user-quit-all-side-windows)
         ("C-c w b" . user-switch-to-minibuffer))
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

(use-package user-files
  :bind (("C-c f D" . user-delete-file-and-buffer)
         ("C-c f r" . user-rename-file-and-buffer)))

(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

;; Searching
(use-package grep
  :defer t
  :config
  (add-hook 'grep-mode-hook #'user-results-buffer-hook))

(use-package ag
  :ensure t
  :bind ("C-c s a" . ag)
  :init
  (csetq ag-reuse-buffers t)
  (csetq ag-reuse-window t)
  :config
  (dolist (ign-file grep-find-ignored-files)
    (add-to-list 'ag-ignore-list ign-file))

  (dolist (ign-dir grep-find-ignored-directories)
    (add-to-list 'ag-ignore-list ign-dir)))

(use-package wgrep
  :ensure t
  :defer t
  :init
  (csetq wgrep-auto-save-buffer t)
  (defun user-enable-wgrep-for-map (map)
    "Enables the wgrep standard key (C-c C-p) for the given MAP."
    (bind-key "C-c C-p" #'wgrep-change-to-wgrep-mode map)))

(use-package wgrep-ag
  :ensure t
  :defer t)

(use-package anzu
  :ensure t
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ("C-c r" . anzu-query-replace-at-cursor-thing)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :demand t
  :diminish anzu-mode
  :init
  (csetq anzu-replace-to-string-separator " => ")
  :config
  (csetq anzu-cons-mode-line-p nil)
  (global-anzu-mode t))

(use-package rg
  :ensure t
  :bind (("C-c s s" . rg)
         ("C-c p s r" . projectile-rg))
  :config
  (defun projectile-rg (regexp)
    "Run a rg search with `REGEXP' rooted at the current projectile project root."
    (interactive
     (list
      (read-from-minibuffer "[project] rg search for: " (thing-at-point 'symbol))))

    (let ((rg-command (concat rg-command " "
                              (mapconcat
                               (lambda (ign) (concat "--glob !" ign))
                               (append projectile-globally-ignored-files
                                       projectile-globally-ignored-directories)
                               " "))))
      (rg regexp "everything" (projectile-project-root)))))

(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map
         ("ESC" . company-abort))
  :init
  (csetq company-dabbrev-downcase nil)
  (csetq company-dabbrev-ignore-case t)

  (csetq company-dabbrev-code-ignore-case t)

  (csetq company-transformers '(company-sort-by-occurrence))
  (csetq company-idle-delay 0)
  (csetq company-minimum-prefix-length 3)
  (csetq company-tooltip-align-annotations t)

  (add-hook 'after-init-hook 'global-company-mode))

;; (csetq completion-ignore-case t)

(require 'dabbrev)
(csetq dabbrev-case-replace nil)
(csetq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*$]")
(csetq abbrev-file-name (expand-file-name "abbrev_defs" user-cache-directory))
(add-hook 'find-file-hook (lambda () (abbrev-mode -1)))

(use-package dired
  :defer t
  :bind (:map dired-mode-map
         ("C-c C-w" . wdired-change-to-wdired-mode))
  :init
  (defun user-dired-hook ()
    (dired-hide-details-mode -1)
    (toggle-truncate-lines))

  (add-hook 'dired-mode-hook #'user-dired-hook)

  (csetq dired-listing-switches "-laGh1v --group-directories-first")
  (csetq dired-dwim-target t)
  (csetq dired-auto-revert-buffer t)
  (csetq dired-ls-F-marks-symlinks t)
  (csetq dired-recursive-copies 'always)

  :config
  (use-package dired+
    :ensure t
    :bind (:map dired-mode-map
                ("SPC" . dired-mark)
                ("<M-right>" . diredp-find-file-reuse-dir-buffer)
                ("<M-left>" . diredp-up-directory-reuse-dir-buffer)
                ("<C-prior>"  . diredp-up-directory-reuse-dir-buffer))
    :demand t
    :config
    (csetq diredp-dwim-any-frame-flag t)
    (diredp-toggle-find-file-reuse-dir t)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/unmark-next-like-this)
         ("C-<" . mc/unmark-previous-like-this)
         ("C-." . mc/mark-next-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-c m m" . mc/mark-all-dwim)
         ("C-c m d" . mc/mark-all-symbols-like-this-in-defun)
         ("C-c m i" . mc/insert-numbers)
         ("C-c m w" . mc/mark-all-words-like-this)
         ("C-c m h" . mc-hide-unmatched-lines-mode)
         ("C-c m l" . mc/edit-lines)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-end-of-lines))
  :init
  (csetq mc/list-file (expand-file-name ".mc-lists.el" user-cache-directory)))

(use-package smex
  :ensure t
  :defer t
  :init
  (csetq smex-save-file (expand-file-name "smex-items" user-cache-directory))
  :config
  (smex-initialize))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-mode-map
         ([escape] . user-minibuffer-keyboard-quit))
  :init
  (csetq ivy-on-del-error-function nil) ; don't exit with backspace
  (csetq ivy-display-style 'fancy)
  (csetq ivy-use-virtual-buffers t)
  (csetq ivy-count-format "(%d/%d) ")
  (csetq ivy-height 11)
  (csetq ivy-wrap t)
  ;; I still prefer space as separator
  ;; (csetq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode t))

(use-package ivy-hydra
  :ensure t
  :after ivy
  :bind ((:map ivy-minibuffer-map
          ("C-o" hydra-ivy/body))))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (("C-c g l" . counsel-git-log)
         ("C-c g g" . counsel-git-grep)
         ("C-c f r" . counsel-recentf)
         ("C-c f f" . counsel-find-file)
         ("C-c s c" . counsel-ag))
  :init
  (if (executable-find "rg")
      (csetq counsel-ag-base-command "rg --glob !elpa --no-heading --vimgrep %s")
    ;; on windows it doesn't work without the '--vimgrep' part
    (csetq counsel-ag-base-command "ag --ignore tags --ignore TAGS --ignore elpa --vimgrep %s"))
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
  (csetq comint-history-isearch t))

(defun user-programming-setup ()
  "Settings for all programming modes."
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'user-programming-setup)

(use-package which-func
  :init
  (which-function-mode t)
  :config
  (set-face-attribute 'which-func nil :foreground "red1")
  (face-foreground 'which-func))

(defun user-results-buffer-hook ()
  "Set various settings on results buffers (compilation, grep, etc.)."
  (setq-local truncate-lines t)
  (setq-local show-trailing-whitespace nil))

(use-package compile
  :diminish compilation-in-progress
  :bind (("C-c c" . compile))
  :config
  (csetq compilation-scroll-output 'first-error)
  (csetq compilation-always-kill t)
  (csetq compilation-disable-input t)
  (csetq compilation-context-lines 3)

  (require 'ansi-color)

  (defun user-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer.
Taken from http://stackoverflow.com/a/3072831/355252."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook #'user-colorize-compilation-buffer)
  (add-hook 'compilation-mode-hook #'user-results-buffer-hook))

(use-package eldoc
  :defer t
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))
  ;;(csetq eldoc-documentation-function #'describe-char-eldoc))

;; Error checking
(use-package flycheck
  :ensure t
  :defer t
  :bind (("C-c e" . user-flycheck-errors/body)
         ("C-c t f" . flycheck-mode))
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
  (global-flycheck-mode)
  (csetq flycheck-standard-error-navigation nil)
  (csetq flycheck-display-errors-function
         #'flycheck-display-error-messages-unless-error-list))

(use-package user-c)

(use-package gud
  :defer t
  :config
  (csetq gdb-many-windows t)

  (defun user-gud ()
    "Hook to run when GUD mode is activated."
    (company-mode -1))

  (add-hook 'gud-mode-hook #'user-gud))

(use-package nasm-mode
  :ensure t
  :defer t)

;; Web stuff
(use-package js2-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (setf js2-skip-preprocessor-directives t)
  (setq-default js2-additional-externs
                '("$" "unsafeWindow" "localStorage" "jQuery"
                  "setTimeout" "setInterval" "location" "skewer"
                  "console" "phantom")))

(use-package json-mode
  :ensure t
  :bind (:map json-mode-map
         ("M-q" . json-reformat-region))
  :config
  (csetq json-reformat:indent-width 4)
  (csetq json-reformat:pretty-string? t))


(defun user-web-mode-hook ()
  "Hook to run when `web-mode' is active."
  (csetq web-mode-markup-indent-offset 2)
  (csetq web-mode-css-indent-offset 2))

(use-package web-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (csetq web-mode-engines-alist
         '(("django" . "\\.html\\'")))

  (add-hook 'web-mode-hook #'user-web-mode-hook))

;; Python
(use-package cython-mode
  :ensure t
  :defer t)

(use-package imenu
  :bind ("M-i" . imenu)
  :init
  (add-hook 'imenu-after-jump-hook (lambda () (recenter-top-bottom))))

;; Project
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (csetq projectile-cache-file
         (expand-file-name "projectile.cache" user-cache-directory))
  (csetq projectile-known-projects-file
         (expand-file-name "projectile-bookmarks.eld" user-cache-directory))

  (csetq projectile-project-root-files-bottom-up
         '(".projectile" ".git" ".hg"))

  (csetq projectile-completion-system 'ivy)
  (csetq projectile-indexing-method 'alien)
  (csetq projectile-enable-caching t)
  (csetq projectile-verbose t)
  (csetq projectile-use-git-grep t)

  (projectile-mode t)

  :config
  (add-to-list 'projectile-globally-ignored-directories ".vscode")

  (csetq projectile-switch-project-action
         (lambda ()
           (dired (projectile-project-root)))))

; Git
(use-package magit
  :ensure t
  :bind (("<f9>" . magit-status))
  :init
  (csetq magit-display-buffer-function
         #'magit-display-buffer-fullframe-status-v1)
  :config
  (use-package magit-gitflow
    :ensure t
    :defer t
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk))
  :init
  (csetq git-gutter:disabled-modes '(dired-mode
                                     image-mode shell-mode eshell-mode
                                     compilation-mode grep-mode
                                     ag-mode magit-mode))
  :config
  (global-git-gutter-mode t))

;; Mail
(use-package notmuch
  :ensure t
  :bind (("C-x m" . notmuch-hello)
         ("<f10>" . notmuch-hello)
         :map notmuch-search-mode-map
         ("a" . nil))
  :init
  ;; Send mail configuration
  (csetq mail-user-agent 'message-user-agent)

  (csetq message-kill-buffer-on-exit t)

  (csetq sendmail-program "/usr/bin/msmtp")
  ;; (csetq send-mail-function #'sendmail-send-it)

  ;; (csetq message-sendmail-f-is-evil nil)
  (csetq message-send-mail-function #'message-send-mail-with-sendmail)
  (csetq mail-specify-envelope-from t)
  (csetq mail-envelope-from 'header)
  (csetq message-sendmail-envelope-from 'header)

  (csetq message-directory "~/.mail")
  (csetq message-confirm-send t)

  ;; Notmuch configuration
  (csetq notmuch-mua-cite-function #'message-cite-original-without-signature)

  (csetq notmuch-search-oldest-first nil)
  (csetq notmuch-hello-thousands-separator ".")
  (csetq notmuch-column-control 1.0)
  (csetq notmuch-hello-sections '(notmuch-hello-insert-search
                                  notmuch-hello-insert-saved-searches
                                  notmuch-hello-insert-recent-searches
                                  notmuch-hello-insert-alltags))
  (csetq notmuch-show-indent-messages-width 1)
  (csetq notmuch-show-only-matching-messages t)

  (csetq notmuch-saved-searches
         '((:key "i" :name "Inbox" :query "tag:mine")
           (:key "j" :name "Jira" :query "tag:jira")
           (:key "b" :name "Bamboo" :query "tag:bamboo")
           (:key "m" :name "Meetings" :query "tag:meeting")
           (:key "S" :name "Sent" :query "folder:sent")
           (:key "h" :name "HVMI" :query "tag:hvmi")
           (:key "a" :name "All" :query "tag:inbox")
           (:key "u" :name "Unread" :query "tag:unread")))

  ;;(csetq mm-text-html-renderer 'w3m-standalone)
  (csetq mm-text-html-renderer 'links)

  ;; (defun user-message-display-hook ()
  ;;   (setq-local fill-column 100)
  ;;   (visual-fill-column-mode t))
  ;; (add-hook 'notmuch-show-hook #'user-message-display-hook)

  :config
  (require 'notmuch-address)

  (add-to-list 'notmuch-tag-formats '("signed" (propertize tag 'invisible t))))

(use-package notmuch-tree
  :defer t
  :after notmuch
  :bind (:map notmuch-tree-mode-map
         ("a" . nil)
         ("A" . nil)
         ("Z" . notmuch-search-from-tree-current-query))
  :init
  (csetq notmuch-tree-show-out t))

;; Keybindings
(bind-key "C-c t d" #'toggle-debug-on-error)
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key "M-g" #'goto-line)
(bind-key "C-o" #'isearch-occur isearch-mode-map)

(unbind-key "C-z")
(unbind-key "C-x C-z")
(unbind-key "C-x f")

(provide 'init)

;;; init.el ends here

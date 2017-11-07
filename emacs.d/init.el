;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; It's init.el, that says enough.

;;; Code:

;; Easier customization
(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

;; Will reset at the end of loading
(defconst user-original-gc-cons (* 7 gc-cons-threshold)
  "The original/default value of `gc-cons-threshold'.")

(csetq gc-cons-threshold (* 128 1024 1024))

(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(defconst user-custom-file (expand-file-name "custom.el" user-emacs-directory)
  "File used to store settings from Customization UI.")

(add-hook 'after-init-hook
          (lambda ()
            (csetq gc-cons-threshold user-original-gc-cons)))

(csetq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(csetq package-enable-at-startup nil)
(csetq load-prefer-newer t)

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

(csetq use-package-enable-imenu-support t)
(require 'use-package)

(use-package ignoramus
  :ensure t
  :config
  (ignoramus-setup))

(use-package tramp
  :init
  ;; Work-around for tramp which apparently doesn't knwo 'default
  (when (eq tramp-syntax 'default)
    (setq tramp-syntax 'ftp)))

(use-package no-littering
  :ensure t
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

  ;; 10 minutes for special buffers
  (csetq clean-buffer-list-delay-special (* 10 60))

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

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))

;; Setup the gui appearance
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(set-face-attribute 'default nil
                    :family "Iosevka" :height 110 :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :family "Noto Sans" :height 110 :weight 'regular)

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(size-indication-mode -1)
(line-number-mode t)
(column-number-mode t)
(csetq visible-cursor nil)

;; (when (fboundp #'global-display-line-numbers-mode)
;;   (global-display-line-numbers-mode t))

;; (use-package flatui-theme
;;   :ensure t)

(use-package solarized-theme
  :ensure t
  :init
  (csetq solarized-scale-org-headlines nil)
  (csetq solarized-use-variable-pitch nil)
  (csetq solarized-high-contrast-mode-line t)
  :config
  (load-theme 'solarized-light t))

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
         evil-mode-line-tag
         mode-line-buffer-identification
         ;; " "
         mode-line-modes
         (vc-mode vc-mode)
         " "
         (org-agenda-mode
          (:eval (format "%s" org-agenda-filter)))
         mode-line-misc-info
         mode-line-end-spaces))

;; (setq default-frame-alist '((height . 55)
;;                             (width . 125)))

(csetq fast-but-imprecise-scrolling t)

(show-paren-mode t)
(global-hl-line-mode)

(use-package whitespace
  :diminish whitespace-mode
  :init
  (csetq whitespace-style '(face tab-mark trailing))
  (csetq whitespace-display-mappings '((tab-mark ?\t [187 183 183 183 183 183 183 183])))
  (add-hook 'prog-mode-hook #'whitespace-mode))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

(use-package visual-fill-column
  :ensure t
  :defer t)

(use-package user-sensible)

(use-package user-advices)

(use-package vlf
  :ensure t
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

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode t)
  :config
  (csetq which-key-idle-delay 0.5)
  (csetq which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-setup-side-window-right-bottom))

;; Used by evil-unimpaired
(use-package f
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
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

(use-package evil
  :ensure t
  :init
  (csetq evil-want-C-d-scroll t)
  (csetq evil-want-C-u-scroll t)
  (csetq evil-want-C-i-jump t)
  (csetq evil-want-C-w-delete t)
  (csetq evil-want-visual-char-semi-exclusive t)
  (csetq evil-want-Y-yank-to-eol t)
  (csetq evil-want-abbrev-expand-on-insert-exit nil)

  :config
  (add-to-list 'evil-emacs-state-modes 'ectags-mode)
  (evil-mode t)

  (require 'evil-unimpaired)

  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "SPC")
    (global-evil-leader-mode t))

  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "gj") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "gk") 'evil-previous-line)

  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "gj") 'evil-next-line)
  (define-key evil-visual-state-map (kbd "gk") 'evil-previous-line)

  (define-key evil-normal-state-map (kbd "g ]") (kbd "C-]"))
  (define-key evil-visual-state-map (kbd "g ]") (kbd "C-]"))

  (define-key evil-normal-state-map (kbd "<escape>") #'keyboard-escape-quit)
  (define-key evil-visual-state-map (kbd "<escape>") #'keyboard-quit)

  (define-key evil-insert-state-map (kbd "<C-backspace>") (kbd "C-w"))

  (use-package evil-args
    :ensure t
    :config
    (define-key evil-inner-text-objects-map "," 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "," 'evil-outer-arg))

  (use-package evil-commentary
    :ensure t
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode t))

  (use-package evil-lion
    :ensure t
    :config
    (evil-lion-mode t))

  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode t))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode t))

  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode t))

  (use-package evil-rsi
    :ensure t
    :diminish evil-rsi-mode
    :config
    (evil-rsi-mode t)))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (csetq Vhl/highlight-zero-width-ranges t)

  (vhl/define-extension 'vhl-undo-tree #'undo-tree-move #'undo-tree-undo #'undo-tree-redo)
  (vhl/install-extension 'vhl-undo-tree))

(use-package mouse-copy)

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
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
  :bind (("M-]" . user-next-error)
         ("M-[" . user-prev-error)
         ([remap forward-paragraph] . user-forward-paragraph)
         ([remap backward-paragraph] . user-backward-paragraph)
         ("M-j" . user-join-line)
         ("M-'" . user-open-terminal)
         :map evil-insert-state-map
         ("C-a" . user-smarter-move-beginning-of-line)
         ("<C-return>" . user-open-line-above)
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


(use-package user-files
  :defer t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (csetq projectile-project-root-files-bottom-up
         '(".projectile" ".git" ".hg"))

  (csetq projectile-completion-system 'helm)
  (csetq projectile-indexing-method 'alien)
  (csetq projectile-enable-caching t)
  (csetq projectile-verbose t)
  (csetq projectile-use-git-grep t)

  (projectile-mode t)

  :config
  (add-to-list 'projectile-globally-ignored-directories ".vscode")

  (define-key evil-normal-state-map (kbd "SPC p") projectile-command-map))

;; Searching
(defun user-results-buffer-hook ()
  "Set various settings on results buffers (compilation, grep, etc.)."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local show-trailing-whitespace nil))

(use-package grep
  :defer t
  :config
  (add-hook 'grep-mode-hook #'user-results-buffer-hook))

(use-package ag
  :ensure t
  :commands (ag)
  :bind (("C-c s a" . ag))
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
  (global-anzu-mode t)

  (use-package evil-anzu
    :ensure t))

(use-package ripgrep
  :ensure t
  :bind (("C-c s s" . user-ripgrep)
         ("C-c s r" . ripgrep-regexp))
  :init
  (use-package projectile-ripgrep
    :ensure t
    :bind (("C-c p s s" . user-projectile-ripgrep)
           ("C-c p s r" . projectile-ripgrep)))

  (use-package wgrep-ack
    :ensure t
    :defer t
    :init
    (add-hook 'ripgrep-search-mode-hook #'wgrep-ack-and-a-half-setup))

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
  (puthash "org-mode" "org" user-rg-type-aliases)
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

(use-package company
  :ensure t
  :diminish company-mode
  :bind (("C-j" . company-complete)
         :map company-active-map
         ("ESC" . company-abort)
         ("C-l" . company-show-location)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-w" . nil))
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
    :bind (("C-x C-l" . user-complete-line)))

  (add-hook 'after-init-hook 'global-company-mode))

(use-package ycmd
  :ensure t
  :defer t
  :init
  (set-variable 'ycmd-server-command `("python2" ,(file-truename "~/src/ycmd/ycmd/")))
  (csetq ycmd-force-semantic-completion t)
  (csetq ycmd-tag-files 'auto)

  (use-package company-ycmd
    :ensure t
    :defer t
    :init
    (csetq company-ycmd-enable-fuzzy-matching nil))

  :config
  (require 'ycmd-eldoc)
  (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

  ;; (add-hook 'ycmd-mode-hook (lambda ()
  ;;                             )))
  (evil-define-key 'normal ycmd-mode-map (kbd "SPC y") ycmd-command-map))

;; (csetq completion-ignore-case t)

(require 'dabbrev)
(csetq dabbrev-case-replace nil)
(csetq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*$]")
(add-hook 'find-file-hook (lambda () (abbrev-mode -1)))

;; (use-package yasnippet
;;   :ensure t
;;   :diminish yas-minor-mode
;;   :config
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package dired
  :defer t
  :bind (:map dired-mode-map
         ("C-c C-w" . wdired-change-to-wdired-mode))
  :init
  (defun user-dired-hook ()
    ;; (dired-hide-details-mode -1)
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
           ;; ("SPC" . dired-mark)
           ("<M-right>" . diredp-find-file-reuse-dir-buffer)
           ("<M-left>" . diredp-up-directory-reuse-dir-buffer)
           ("<C-prior>"  . diredp-up-directory-reuse-dir-buffer))
    :demand t
    :config
    (csetq diredp-dwim-any-frame-flag t)
    (diredp-toggle-find-file-reuse-dir t))

  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow))))

(use-package smex
  :ensure t
  :defer t
  :config
  (smex-initialize))

(use-package helm
  :bind (("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ("C-z" . helm-select-action)
         ("<tab>" . helm-execute-persistent-action)
         :map helm-find-files-map
         ("C-z" . helm-select-action)
         ("<tab>" . helm-execute-persistent-action))
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-config)

  (csetq helm-google-suggest-use-curl-p t)
  (csetq helm-split-window-in-side-p t)
  (csetq helm-move-to-line-cycle-in-source t)
  (csetq helm-ff-search-library-in-sexp t)
  (csetq helm-scroll-amount 8)
  (csetq helm-ff-file-name-history-use-recentf t)
  ;; (csetq helm-echo-input-in-header-line t)

  (csetq helm-autoresize-max-height 25)
  (csetq helm-autoresize-min-height 10)

  (helm-mode t)
  (helm-autoresize-mode t)

  (csetq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (csetq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))

  (use-package helm-smex
    :bind (("M-x" . helm-smex)
           ("M-X" . helm-smex-major-mode-commands))
    :ensure t)

  (use-package wgrep-helm
    :ensure t))

;; (use-package ivy
;;   :ensure t
;;   :diminish ivy-mode
;;   :bind (("C-c C-r" . ivy-resume)
;;          :map ivy-mode-map
;;          ([escape] . user-minibuffer-keyboard-quit))
;;   :init
;;   (csetq ivy-on-del-error-function nil) ; don't exit with backspace
;;   (csetq ivy-display-style 'fancy)
;;   (csetq ivy-use-virtual-buffers t)
;;   (csetq ivy-count-format "(%d/%d) ")
;;   (csetq ivy-height 11)
;;   (csetq ivy-wrap t)
;;   ;; I still prefer space as separator
;;   ;; (csetq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
;;   :config
;;   (ivy-mode t))

;; (use-package ivy-hydra
;;   :ensure t
;;   :after ivy
;;   :bind ((:map ivy-minibuffer-map
;;           ("C-o" hydra-ivy/body))))

;; (use-package counsel
;;   :ensure t
;;   :diminish counsel-mode
;;   :bind (("C-c f r" . counsel-recentf)
;;          ("C-c f f" . counsel-find-file)
;;          ("C-c s c" . counsel-rg))
;;   :init
;;   ;; on windows it doesn't work without the '--vimgrep' part
;;   (csetq counsel-ag-base-command "ag --ignore tags --ignore TAGS --ignore elpa --vimgrep %s")
;;   (counsel-mode t))

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
  (csetq comint-ignore-dups t))

(defun user-programming-setup ()
  "Settings for all programming modes."
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'user-programming-setup)

(use-package which-func
  :init
  (which-function-mode t))

(use-package hexl
  :init
  (csetq hexl-bits 8))

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :config
  (csetq highlight-symbol-idle-delay 0.3)
  (csetq highlight-symbol-highlight-single-occurrence nil)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))

(use-package compile
  :diminish compilation-in-progress
  :bind (("C-c c" . compile))
  :config
  (csetq compilation-scroll-output 'first-error)
  (csetq compilation-always-kill t)
  (csetq compilation-disable-input t)
  (csetq compilation-context-lines 3)

  (defvar user-compile-process nil
    "The current compilation process or nil if none.")

  (defun user-delete-window (window)
    "Kill the WINDOW, ignoring errors."
    (ignore-errors
      (delete-window window)))

  (defun user-compile-start (proc)
    (setq user-compile-process proc))

  (defun user-compile-done (buffer _msg)
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

    (setq user-compile-process nil))

  (add-hook 'compilation-start-hook #'user-compile-start)
  (add-to-list 'compilation-finish-functions #'user-compile-done)

  (evil-define-key 'normal compilation-mode-map (kbd "q") #'delete-window)

  (require 'ansi-color)

  (defun user-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer.
Taken from http://stackoverflow.com/a/3072831/355252."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook #'user-colorize-compilation-buffer)
  (add-hook 'compilation-mode-hook #'user-results-buffer-hook))

(when (eq system-type 'gnu/linux)
  (use-package multi-term
    :ensure t
    :bind (("C-; C-;" . multi-term-next)
           ("C-; c" . multi-term)
           ("C-; d" . multi-term-dedicated-toggle)
           ("C-; n" . multi-term-next)
           ("C-; p" . multi-term-prev))
    :init
    (csetq multi-term-dedicated-select-after-open-p t)

    (defun user-term-mode-hook ()
      (company-mode -1)
      (hl-line-mode -1)
      (setq-local scroll-margin 0))

    (add-hook 'term-mode-hook #'user-term-mode-hook)
    :config
    (evil-define-key 'insert term-mode-map (kbd "C-a") #'term-send-raw)
    (evil-define-key 'insert term-mode-map (kbd "C-d") #'term-send-raw)))

(use-package eshell
  :defer t
  :config
  (defun user-eshell-hook ()
    (company-mode -1)
    (setq-local scroll-margin 0))

  (add-hook 'eshell-mode-hook #'user-eshell-hook))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

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
  (csetq flycheck-check-syntax-automatically '(save mode-enabled))

  (global-flycheck-mode t)
  (csetq flycheck-standard-error-navigation nil)
  (csetq flycheck-display-errors-function
         #'flycheck-display-error-messages-unless-error-list))

;; C/C++
(use-package user-c)

(use-package nasm-mode
  :ensure t
  :defer t)

;; cmake
(use-package cmake-mode
  :ensure t
  :defer t)

;; Debugging
(use-package gud
  :defer t
  :config
  (csetq gdb-many-windows t)

  (defun user-gud ()
    "Hook to run when GUD mode is activated."
    (company-mode -1))

  (add-hook 'gud-mode-hook #'user-gud))

(use-package realgud
  :commands (realgud:bashdb realgud:gdb realgud:gub realgud:ipdb
             realgud:jdb realgud:kshdb realgud:nodejs realgud:pdb
             realgud:perldb realgud:zshdb)
  :ensure t)

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

(use-package web-mode
  :ensure t
  :defer t
  :init
  (defun user-web-mode-hook ()
    "Hook to run when `web-mode' is active."
    (smartparens-mode -1))

  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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
         '(("django" . "\\.html\\'")))

  (add-hook 'web-mode-hook #'user-web-mode-hook))

;; Python
(use-package cython-mode
  :ensure t
  :defer t)

;; imenu
(use-package imenu
  :bind ("M-i" . imenu)         ;; allow ivy to override it
  :init
  (csetq imenu-auto-rescan t)
  (csetq imenu-auto-rescan-maxout (* 1024 1024))

  (add-hook 'imenu-after-jump-hook (lambda () (recenter-top-bottom))))

(use-package imenu-anywhere
  :ensure t
  :bind (("M-I" . imenu-anywhere)))

;; TAGS
(use-package ectags
  :init
  (evil-define-key 'normal c-mode-map (kbd "C-]") #'ectags-find-tag-at-point))

(use-package xref
  :init
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "C-]") #'xref-find-definitions))

;; Git
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init
  (csetq magit-display-buffer-function
         #'magit-display-buffer-fullframe-status-v1)
  (csetq magit-completing-read-function #'magit-builtin-completing-read)

  :config
  (use-package evil-magit
    :ensure t)

  (use-package magit-gitflow
    :ensure t
    :defer t
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

;; Keybindings
(bind-key "C-c t d" #'toggle-debug-on-error)
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key "M-g" #'goto-line)
(bind-key "C-o" #'isearch-occur isearch-mode-map)
(bind-key "C-8" #'repeat-complex-command)

(evil-leader/set-key
  "SPC" #'helm-smex

  "; ;" #'multi-term-next
  "; c" #'multi-term
  "; d" #'multi-term-dedicated-toggle
  "; n" #'multi-term-next
  "; p" #'multi-term-prev

  "a" #'projectile-find-other-file

  ;; Buffers
  "b" #'helm-mini

  ;; Compile
  "c c" #'projectile-compile-project

  ;; Errors
  "e" #'user-flycheck-errors/body

  ;; Files
  "f f" #'helm-find-files
  "f r" #'helm-recentf
  "f D" #'user-delete-file-and-buffer
  "f R" #'user-rename-file-and-buffer

  "g g" #'magit-status

  ;; Search
  "s a" #'ag
  "s c" #'helm-do-grep-ag
  "s o" #'occur
  "s r" #'anzu-query-replace-at-cursor-thing
  "s s" #'user-ripgrep)

(use-package pdf-tools
  :init
  (pdf-tools-install t t t))

(unbind-key "C-z")
(unbind-key "C-x C-z")
(unbind-key "C-x f")
(unbind-key "C-x m")
(unbind-key "<insert>")
(unbind-key "M-o")

(provide 'init)

;;; init.el ends here

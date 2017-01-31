(defvar user-sane-syntax
  `(
    ;; statements
    ("\\<goto\\>\\|\\<break\\>\\|\\<return\\>\\|\\<continue\\>\\|\\<asm\\>" . font-lock-keyword-face)
    ;; label
    ("\\<case\\>\\|\\<default\\>" . font-lock-keyword-face)
    ;; preprocessor (set this before contidionals)
    ("^\\s-*#\\s-*\\(include\\|define\\|if\\|ifdef\\|ifndef\\|endif\\|else\\|elif\\|pragma\\)\\>" . font-lock-preprocessor-face)
    ;; conditional
    ("\\<if\\>\\|\\<else\\>\\|\\<switch\\>" . font-lock-keyword-face)
    ;; repeat
    ("\\<while\\>\\|\\<for\\>\\|\\<do\\>" . font-lock-keyword-face)
    ;; keywords
    ("\\<char\\>\\|\\<const\\>\\|\\<double\\>\\|\\<enum\\>\\|\\<extern\\>\\|\\<float\\>" . font-lock-keyword-face)
    ("\\<int\\>\\|\\<long\\>\\|\\<register\\>\\|\\<short\\>\\|\\<signed\\>" . font-lock-keyword-face)
    ("\\<sizeof\\>\\|\\<static\\>\\|\\<struct\\>\\|\\<typedef\\>" . font-lock-keyword-face)
    ("\\<union\\>\\|\\<unsigned\\>\\|\\<void\\>\\|\\<volatile\\>\\|\\<while\\>" . font-lock-keyword-face)
    ;; c++
    ("\\<class\\>\\|\\<auto\\>\\|\\<new\\>\\|\\<delete\\>\\|\\<bool\\>" . font-lock-keyword-face)
    ;; ansi extensions
    ("\\<typeof\\>\\|\\<inline\\>\\|\\<size_t\\>" . font-lock-keyword-face)
    ;; windows extensions
    ("\\<P?\\(QWORD\\|DWORD\\|WORD\\|BYTE\\|CHAR\\)\\>" . font-lock-type-face)
    ("\\<P?U?\\(INT64\\|INT32\\|INT16\\|INT8\\)\\>" . font-lock-type-face)
    ("\\<P?VOID\\>\\|\\<BOOLEAN\\>\\|\\<NTSTATUS\\>\\|\\<STATUS\\>\\|\\<BOOL\\>" . font-lock-type-face)
    ("\\<NULL\\>\\|\\<TRUE\\>\\|\\<FALSE\\>\\|\\<true\\>\\|\\<false\\>" . font-lock-type-face)
    ;; defines
    ("^\\s-*#\\s-*define\\s-*\\([_a-zA-Z][a-zA-Z0-9_]+\\)" 1 font-lock-type-face)
    ;; annotation-face
    ("@\\([[:word:]]\\)" 1 c-annotation-face)
    ;; labels
    ("^\\s-*\\<\\(protected\\|private\\|public\\)\\>:" 1 font-lock-keyword-face)
    ("^\\s-*\\([_a-zA-Z][a-zA-Z0-9_]+\\):[^:]" 1 font-lock-constant-face)
    ("goto\\s-+\\([_a-zA-Z][a-zA-Z0-9_]+\\)[ \t\n]+;" 1 font-lock-constant-face)
    ;; linux
    ("\\<u?\\(int64\\|int32\\|int16\\|int8\\)_t\\>" . font-lock-type-face))
  "When the default syntax is too slow.")

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

(c-add-style "allman" user-allman-style)

(defun user-c-goto-def ()
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

(defun user-cc-mode-setup ()
  "Hook for C/C++ mode."

  ;; This can be moved inside the cc-mode eval-after-load
  (c-toggle-electric-state t)
  (c-toggle-syntactic-indentation t)

  ;; (setq font-lock-defaults '((user-sane-syntax)
  ;;                            nil ;; do it for strings & comments too
  ;;                            nil ;; case sensitive
  ;;                            ((95 . "w")
  ;;                             (36 . "w"))
  ;;                            c-beginning-of-syntax
  ;;                            (font-lock-mark-block-function . c-mark-function)))

  ;; (if user-is-linux
  ;;     (progn
  ;;       (rtags-start-process-unless-running)

  ;;       (setq rtags-completions-enabled t)
  ;;       (setq rtags-autostart-diagnostics t)

  ;;       (flycheck-select-checker 'rtags)

  ;;       ;; RTags creates more accurate overlays.
  ;;       (setq-local flycheck-highlighting-mode nil)
  ;;       (setq-local flycheck-check-syntax-automatically nil)

  ;;      (setq-local company-backends '(company-rtags company-dabbrev-code company-files)))

  (irony-mode t)
  (irony-cdb-autosetup-compile-options)
  (setq-local company-backends '(company-irony company-dabbrev-code company-files))

  (flycheck-mode t))

;; When it's not fast enough
;; (setq jit-lock-context-time 3)
;; (setq jit-lock-chunk-size 1024)
;; (setq font-lock-maximum-decoration 2)

(add-hook 'c-mode-common-hook #'user-cc-mode-setup) ; this should be the last one called

(defun user-setup-cc-on-linux ()
  "Setups the C/C++ mode on linux using rtags."

  (use-package rtags
    :init
    (add-hook 'rtags-after-find-file-hook #'recenter)
    :config
    (rtags-enable-standard-keybindings))

  (with-eval-after-load 'cc-mode
    ;; This only gets called once. It's enough for some config
    (require 'flycheck-rtags)

    (define-key c-mode-base-map (kbd "M-.") #'rtags-find-symbol-at-point)
    (define-key c-mode-base-map (kbd "M-,") #'rtags-location-stack-back)
    (define-key c-mode-base-map (kbd "C-c r n") #'rtags-next-match)
    (define-key c-mode-base-map (kbd "C-c r p") #'rtags-previous-match)
    (define-key c-mode-base-map (kbd "C-c r d") #'rtags-dependency-tree)
    (define-key c-mode-base-map (kbd "M-m") #'rtags-imenu)

    (setq fast-but-imprecise-scrolling t)))

(defun user-setup-cc ()
  "Setups the C/C++ mode using irony."

  (use-package irony
    :ensure t
    :defer t
    :diminish irony-mode

    :init
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))

    :config
    (use-package company-irony
      :ensure t
      :defer t
      :init
      (csetq company-irony-ignore-case t))

    (use-package flycheck-irony
      :ensure t
      :defer t))

  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)

  (with-eval-after-load 'cc-mode
    ;; This only gets called once. It's enough for some config

    (require 'ectags)
    ;; (ectags-xref-setup t)

    (define-key c-mode-map (kbd "M-.") #'ectags-find-tag-at-point)
    (define-key c-mode-map (kbd "M-o") #'counsel-ectags)
    (define-key c++-mode-map (kbd "M-.") #'ectags-find-tag-at-point)
    (define-key c++-mode-map (kbd "M-o") #'counsel-ectags)

    (setq fast-but-imprecise-scrolling t)))

(user-setup-cc)

(provide 'user-c)
;;; user-c.el ends here

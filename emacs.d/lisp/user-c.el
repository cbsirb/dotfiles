;;; user-c.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Contains configuration for C and derived modes (C++, Obj-C)

;;; Code:

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

(use-package cc-mode
  :defer t
  :bind (:map c-mode-map
         ("M-." . ectags-find-tag-at-point)
         ("M-o" . counsel-ectags)
         :map c++-mode-map
         ("M-." . ectags-find-tag-at-point)
         ("M-o" . counsel-ectags))
  :init
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
    (c-toggle-electric-state t)
    (c-toggle-syntactic-indentation t)

    ;; (irony-mode t)
    ;; (irony-cdb-autosetup-compile-options)
    ;; (setq-local company-backends '(company-irony-c-headers company-irony company-dabbrev-code company-files))
    (setq-local company-backends '(company-dabbrev-code company-files))

    (flycheck-mode t))

  (add-hook 'c-mode-common-hook #'user-cc-mode-setup)

  :config
  (require 'ectags)

  ;; (use-package irony
  ;;   :ensure t
  ;;   :defer t
  ;;   :diminish irony-mode

  ;;   :init
  ;;   (setq irony-server-w32-pipe-buffer-size (* 64 1024))

  ;;   :config
  ;;   (use-package company-irony
  ;;     :ensure t
  ;;     :defer t
  ;;     :init
  ;;     (csetq company-irony-ignore-case t))

  ;;   (use-package irony-eldoc
  ;;     :ensure t
  ;;     :init
  ;;     (add-hook 'irony-mode-hook #'irony-eldoc))

  ;;   (use-package company-irony-c-headers
  ;;     :ensure t
  ;;     :defer t)

  ;;   (use-package flycheck-irony
  ;;     :ensure t
  ;;     :defer t
  ;;     :init
  ;;     (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  )

(provide 'user-c)

;;; user-c.el ends here

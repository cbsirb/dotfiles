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

    ;; Use this as a default, it's pretty sane (compared to gnu)
    (c-set-style "linux")

    (ycmd-mode t)
    (lsp-cquery-enable))

  (add-hook 'c-mode-common-hook #'user-cc-mode-setup)

  :config
  (use-package lsp-mode :ensure t
    :init
    (csetq lsp-highlight-symbol-at-point nil))

  (use-package cquery :ensure t
    :init
    (csetq cquery-project-roots '("compile_commands.json"))
    (csetq cquery-executable "~/src/cquery/build/release/bin/cquery"))
  )

(provide 'user-c)

;;; user-c.el ends here

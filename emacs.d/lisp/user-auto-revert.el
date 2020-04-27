;; Configure auto-revert first, even though we're not activating the mode (just in case)
(setq auto-revert-verbose t)
(setq auto-revert-avoid-polling t)
(setq auto-revert-stop-on-user-input nil)

(autoload #'auto-revert-handler "autorevert")

(defun auto-revert-buffer ()
  "Auto revert current buffer, if necessary."
  (unless (or (bound-and-true-p auto-revert-mode) (active-minibuffer-window))
    (auto-revert-handler)))

(defvar switch-buffer-hooks '()
  "Hooks to run when switching buffers.")

(defvar switch-window-hooks '()
  "Hooks to run when switching windows.")

(defvar user--last-window nil
  "The Last window selected.")

(defun run-switch-buffer-hooks (orig-fn buffer-or-name &rest args)
  (if (or (eq (current-buffer) (get-buffer buffer-or-name))
          (and (eq orig-fn #'switch-to-buffer) (car args)))
      (apply orig-fn buffer-or-name args)
    (let ((inhibit-redisplay t))
      (when-let (buffer (apply orig-fn buffer-or-name args))
        (with-current-buffer (if (windowp buffer)
                                 (window-buffer buffer)
                               buffer)
          (run-hooks 'switch-buffer-hooks))
        buffer))))

(defun run-switch-to-next-prev-buffer-hooks (orig-fn &rest args)
  (let ((inhibit-redisplay t))
    (when-let (buffer (apply orig-fn args))
      (with-current-buffer buffer
        (run-hooks 'user/switch-buffer-hook))
      buffer)))

(defun run-switch-window-hooks ()
  (unless (or (eq user--last-window (selected-window))
              (minibufferp))
    (let ((inhibit-redisplay t))
      (run-hooks 'switch-window-hooks)
      (setq user--last-window (selected-window)))))

(dolist (fn '(switch-to-next-buffer switch-to-prev-buffer))
  (advice-add fn :around #'run-switch-to-next-prev-buffer-hooks))

(dolist (fn '(switch-to-buffer display-buffer))
  (advice-add fn :around #'run-switch-buffer-hooks))

(add-hook 'buffer-list-update-hook #'run-switch-window-hooks)
(add-hook 'focus-in-hook #'auto-revert-buffer)
(add-hook 'switch-buffer-hooks #'auto-revert-buffer)
(add-hook 'switch-window-hooks #'auto-revert-buffer)

(provide 'user-auto-revert)

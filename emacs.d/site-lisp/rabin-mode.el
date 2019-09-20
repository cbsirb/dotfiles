(defvar-local rabin-mode nil)

;;;###autoload
(defun rabin-mode ()
  (interactive)
  (let ((inhibit-read-only t))
    (if rabin-mode
        (progn
          (delete-region (point-min) (point-max))
          (insert-file-contents (buffer-file-name))
          (read-only-mode -1)
          (setq rabin-mode nil))
      (setq rabin-mode t)
      (delete-region (point-min) (point-max))
      (insert (shell-command-to-string
               (format "rabin2 -SMIesl %s" (buffer-file-name)))))
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(provide 'rabin-mode)

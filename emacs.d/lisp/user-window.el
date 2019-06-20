;;; user-window.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Contains functions for manipulating windows

;;; Code:

;;;###autoload
(defun user/switch-to-minibuffer ()
  "Switch to current minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun user/find-side-windows (&optional side)
  "Get all side window if any.
If SIDE is non-nil only get windows on that side."
  (let (windows)
    (walk-window-tree
     (lambda (window)
       (let ((window-side (window-parameter window 'window-side)))
         (when (and window-side (or (not side) (eq window-side side)))
           (push window windows)))))
    windows))

;;;###autoload
(defun user/quit-all-side-windows ()
  "Quit all side windows of the current frame."
  (interactive)
  (dolist (window (user/find-side-windows))
    (when (window-live-p window)
      (quit-window nil window)
      ;; When the window is still live, delete it
      (when (window-live-p window)
        (delete-window window)))))

;;;###autoload
(defun user/toggle-current-window-dedication ()
  "Toggle dedication state of a window.
Taken from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows"
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(provide 'user-window)

;;; user-window.el ends here

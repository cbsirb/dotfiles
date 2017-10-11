;;; user-advices.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Contains advices (such as indenting the yanked region)

;;; Code:

(defvar yank-indent-modes '(LaTeX-mode TeX-mode nxml-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).")

(defvar yank-advised-indent-threshold 10000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large.
BEG is the beginning of region, END is the end."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)
    (message "[YANK] Region size %d too big!" (- end beg))))

(defadvice yank (after yank-indent activate)
  "Indent yanked text (with prefix arg don't indent).
If current mode is one of `yank-indent-modes' or derived from `prog-mode'."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "Indent yanked text (with prefix arg don't indent).
If current mode is one of `yank-indent-modes' or derived from `prog-mode'."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

(defun multi-pop-to-mark (orig-func &rest args)
  "Call ORIG-FUNC until the cursor is moved.
Try the repeated popping up to 10 times.
ARGS is passed to the ORIG-FUNC."
  (let ((p (point)))
    (dotimes (_i 10)
      (when (= p (point))
        (apply orig-func args)))))

(advice-add 'pop-to-mark-command :around #'multi-pop-to-mark)

(when (eq system-type 'windows-nt)
  (defadvice shell-command (around fix-encoding activate)
    (let ((coding-system-for-read 'cp1250))
      ad-do-it)))

(provide 'user-advices)

;;; user-advices.el ends here

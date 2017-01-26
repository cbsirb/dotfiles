(defvar yank-indent-modes '(emacs-lisp-mode
                            lisp-interaction-mode
                            js-mode
                            js2-mode
                            web-mode
                            html-mode
                            css-mode
                            c-mode
                            c++-mode
                            perl-mode
                            ruby-mode
                            python-mode
                            LaTeX-mode
                            TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).")
(make-variable-buffer-local 'yank-indent-modes)

(defvar yank-advised-indent-threshold 10000
  "Threshold (# chars) over which indentation does not automatically occur.")
(make-variable-buffer-local 'yank-advised-indent-threshold)

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large.
BEG is the beginning of region, END is the end."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)
    (message "[YANK] Region size %d too big!" (- end beg))))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

(defun multi-pop-to-mark (orig-func &rest args)
  "Call ORIG-FUNC until the cursor is moved.
Try the repeated popping up to 10 times.
ARGS is passed to the ORIG-FUNC."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-func args)))))

(advice-add 'pop-to-mark-command :around #'multi-pop-to-mark)

(provide 'user-advices)
;;; user-advices.el ends here

;;; user-utils.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Contains various functions (most of them interactive, to be binded)

;;; Code:

;;;###autoload
(defun user/forward-paragraph (&optional n)
  "Advance just past next blank line.
With N goes forward that many paragraphs."
  (interactive "p")
  (let ((para-commands
         '(user/forward-paragraph user/backward-paragraph)))
    ;; Only push mark if it's not active and we're not repeating.
    (or (use-region-p)
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; The actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\\(\n[[:blank:]]*\\)+" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min))))
      )))

;;;###autoload
(defun user/backward-paragraph (&optional n)
  "Go back up to previous blank line.
With N goes back that many paragraphs."
  (interactive "p")
  (user/forward-paragraph (- n)))

(defmacro user/bol-with-prefix (function)
  "Define a new function which call FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument.  The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "endless/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

;;;###autoload
(defun user/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun user/open-line-above (arg)
  "Same thing as vim 'O' command.  With ARG opens that many lines."
  (interactive "p")

  (unless (bolp)
    (beginning-of-line))

  (open-line arg)
  (indent-according-to-mode))

;;;###autoload
(defun user/backward-kill-word ()
  "Deletes the previous word, respecting:
1. If the cursor is at the beginning of line, delete the '\n'.
2. If there is only whitespace, delete only to beginning of line and exit.
3. If there is whitespace, delete whitespace and check 4-5.
4. If there are other characters instead of words, delete one only char.
5. If it's a word at point, delete it."
  (interactive)

  (if (bolp)
      ;; 1
      (delete-char -1)

    (if (string-match-p "^[[:space:]]+$"
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))
        ;; 2
        (delete-horizontal-space)

      (when (thing-at-point 'whitespace)
        ;; 3
        (delete-horizontal-space))

      (if (thing-at-point 'word)
          ;; 5
          (let ((start (car (bounds-of-thing-at-point 'word)))
                (end (point)))
            (if (> end start)
                (delete-region start end)
              (delete-char -1)))
        ;; 4
        (delete-char -1)))))

;;;###autoload
(defun user/kill-word-or-region ()
  "If the region is active, will call `kill-region'.
Else it will use `user/backward-kill-word'.
Basically simulates `C-w' in bash or vim when no region is active."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (user/backward-kill-word)))

;;;###autoload
(defun user/copy-line-or-region ()
  "If the region is active, will call `kill-ring-save'.
Else it will call `kill-ring-save' on the current line."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))

    (kill-ring-save (line-beginning-position) (1+ (line-end-position)))))

;;;###autoload
(defun user/comment-dwim (arg)
  "If a region is selected, it comments the region like `comment-dwim'.
If we are at the end of line, adds a comment like `comment-dwim'.
If none of the above, comment current line with `comment-line'.
The prefix ARG is given to the comment function."
  (interactive "*P")

  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (if (= (point) (line-end-position))
        (comment-dwim arg)
      (comment-line arg))))

;;;###autoload
(defun user/ispell-word-on-line ()
  "Call `ispell-word'.
If there's nothing wrong with the word at point, keep looking for a typo
until the beginning of line.  You can skip typos you don't want to fix
with `SPC', and you can abort completely with `C-g'."
  (interactive "P")
  (let ((stop (line-beginning-position)))
    (save-excursion
      (while (if (thing-at-point 'word)
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.

                   (forward-word -1)
                   (> (point) stop))

               (forward-word -1)
               (> (point) stop))))))

;;;###autoload
(defun user/ispell-dwim ()
  "Call `ispell-buffer' of `ispell-comments-and-strings'.
Depends if the current major mode is derived from `prog-mode'.
Saves the position before.  You can skip typos you don't want to fix with
`SPC', and you can abort completely with `C-g'."
  (interactive)
  (save-excursion
    (if (derived-mode-p 'prog-mode)
        (ispell-comments-and-strings)
      (ispell-buffer))))

;;;###autoload
(defun user/next-error ()
  "Go to the next compilation error/search item (ignoring any errors)."
  (interactive)
  (ignore-errors
    (next-error)))

;;;###autoload
(defun user/prev-error ()
  "Go to the previous compilation error/search item (ignoring any errors)."
  (interactive)
  (ignore-errors
    (previous-error)))

;;;###autoload
(defun user/join-line ()
  "Join line like VIM."
  (interactive)
  (join-line -1))

;;;###autoload
(defun user/open-terminal ()
  "Opens a terminal in the current directory."
  (interactive)
  (start-process "konsole" nil "setsid" "konsole" default-directory))

(defun user/get-indentation-chars ()
  "Helper function to return the number of spaces at the begining of line."
  (save-mark-and-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

;; maybe skip comments: (nth 4 (syntax-ppss))

(defun user/forward-c-block ()
  "Go to the next C block (the next line after a '{')."
  (re-search-forward "{\n" (point-max) t)
  (back-to-indentation))

(defun user/backward-c-block ()
  "Go to the previous C block (the next line after a '{')."
  (let ((current-line (line-number-at-pos)))
    (when (re-search-backward "{\n")
      (forward-line)
      ;; We were already on the first line of the block and we found
      ;; the begining. Skip that and go to the previous
      (when (= current-line (line-number-at-pos))
        (forward-line -1)
        (if (re-search-backward "{\n")
            (forward-line)))
      (back-to-indentation))))

(defun user/forward-indentation (&optional n)
  "Go to the next line that has a different indentation that the current one, \
and it's not empty.
If N is negative, then go to the previous block."
  (interactive)
  (unless n (setq n 1))
  (let ((curr-indent (user/get-indentation-chars)))
    (forward-line n)
    (while (or (looking-at-p "^\n")
               (and (= curr-indent (user/get-indentation-chars))
                    (/= (point) (point-max))))
      (forward-line n)))
  (back-to-indentation))

;;;###autoload
(defun user/forward-block ()
  "Go to the next block, based on the current major mode."
  (interactive)
  (cond
   ((derived-mode-p 'java-mode 'c-mode 'c++-mode 'objc-mode) (user/forward-c-block))
   (t (user/forward-indentation))))

;;;###autoload
(defun user/backward-block ()
  "Go to the previous block, based on the current major mode."
  (interactive)
  (cond
   ((derived-mode-p 'java-mode 'c-mode 'c++-mode 'objc-mode) (user/backward-c-block))
   (t (user/forward-indentation -1))))

;;;###autoload
(defun user/clean-windows-whitespace ()
  "Clean strays ^M (windows end-lines) and trailing whitepsace."
  (interactive)
  (while (re-search-forward "[[:space:]\|?\r]+" nil t)
    (replace-match " " nil nil)))

;;;###autoload
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;;;###autoload
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;;;###autoload
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun user/scroll-half-page (direction)
  "Scrolls half page down if `DIRECTION' is non-nil, otherwise will scroll half page up."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)     ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1)  ;; Current line becomes last
      (recenter-top-bottom 0))    ;; Current line becomes first
    (move-to-window-line opos)))  ;; Restore cursor/point position

;;;###autoload
(defun user/scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor/point position."
  (interactive)
  (line-move (- (/ (window-body-height) 2)))
  (recenter))

;;;###autoload
(defun user/scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor/point position."
  (interactive)
  (line-move (/ (window-body-height) 2))
  (recenter))

;;;###autoload
(defun user/split-window-right ()
  "`split-window-right' and focus to the window."
  (interactive)
  (split-window-right)
  (other-window 1))

;;;###autoload
(defun user/split-window-below ()
  "`split-window-below' and focus to the window."
  (interactive)
  (split-window-below)
  (other-window 1))

;;;###autoload
(defun user/yank-rectangle-as-newlines+ (&optional pos)
  "Yank the rectangle, inserting new lines (as it should).

If `POS' is not passed, it uses `point'. Thanks to /u/clemera"
  (interactive)
  (let ((pos (or pos (point))))
    (save-restriction
      (narrow-to-region pos pos)
      (yank-rectangle))))

(provide 'user-utils)

;;; user-utils.el ends here

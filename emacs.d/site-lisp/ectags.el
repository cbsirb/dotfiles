;;; ectags.el --- Select from multiple tags -*- lexical-binding: t; -*-

;; Copyright (C) 2007  Scott Frazer and (C) 2008 John Connors
;; Copyright (C) 2017  rompy

;; Author: rompy
;; Author: John Connors <johnc@yagc.ndo.co.uk>
;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: rompy

;; Keywords: exuberant-ctags universal-ctags ectags tag select

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package originated from John Connors etags-select.el

;;; Code:

(eval-when-compile
  (require 'cl-generic))

(require 's)

;;* Customization

(defgroup ectags nil
  "Universal Ctags Support for Emacs"
  :version "24.4"
  :group 'tools)

(defcustom ectags-search-command
  "rg --no-heading --no-line-number \"^%s\\b\" %s"
  "The command used to search the tags file.  \
The first '%s' is for the tag name, the second '%s' for the tag filename.  \
It will use ripgrep by default, but you can change this to grep like \
by setting this to \"grep \"^%s\\b\" %s\"
Will use the following priority (times based on linux kernel's tag \
file of 489 MB):
- rg   0.756s
- grep 1.192s"
  :group 'ectags
  :type 'string)

(defcustom ectags-max-candidates 24
  "How many candidates to select between."
  :group 'ectags
  :type 'number)

(defcustom ectags-buffer-name "*ectags*"
  "The buffer name for the ectags."
  :group 'ectags
  :type 'string)

(defcustom etags-no-select-for-one-match t
  "If set to nil, it will show the tag selection menu even if \
there is only one match."
  :group 'ectags
  :type 'boolean)

(defcustom ectags-mode-hook nil
  "*List of functions to call on entry to ectags-mode mode."
  :group 'ectags
  :type 'hook)

(defcustom ectags-case-sensitive t
  "If non-nil then the tag searching is case-sensitive."
  :group 'ectags
  :type 'boolean)

(defcustom ectags-tag-files '("tags")
  "The filenames to be checked for tags."
  :group 'ectags
  :type 'list)


(defvar ectags--file-name nil
  "The tag file of the current buffer.  This is a buffer-local variable.")
(make-variable-buffer-local 'ectags--file-name)

(defvar ectags--matches nil
  "List of candiate tag matches of the last search.")

(defvar ectags--regexp nil
  "Holds regexp currently being sought in tags.")

(defvar ectags--fn-name nil
  "Holds the current function name (from where the tag search was initiated).")


(defun ectags--verify-tags-file (file)
  "Validate that the FILE is a valid ctags table."
  (string-prefix-p
   "!_TAG_FILE_FORMAT"
   (shell-command-to-string (format "head -n1 %s" file))))

(defun ectags-visit-tags-table (&optional force)
  "Visit an exuberant ctags file and add it to the current list of tags tables.
If FORCE is not nill, it will force the directory rescan and file checking."
  (when (or (not ectags--file-name)
            (not (file-readable-p ectags--file-name))
            force)
    (let* ((tags-file-dir (locate-dominating-file default-directory "tags"))
           (tags-file-path (expand-file-name "tags" tags-file-dir)))
      (if (not tags-file-dir)
          (setq ectags--file-name nil)
        (if (ectags--verify-tags-file tags-file-path)
            (setq ectags--file-name tags-file-path)
          (message "File %s is not a valid tags file!" tags-file-path)
          (setq ectags--file-name nil))))))

(defun ectags-match-tagname (tag-match)
  "Return the tag name from the TAG-MATCH."
  (nth 0 tag-match))

(defun ectags-match-filename (tag-match)
  "Return the file name from the TAG-MATCH."
  (replace-regexp-in-string "\\\\\\\\" "/" (nth 1 tag-match)))

(defun ectags-match-pattern (tag-match)
  "Return the search pattern from the TAG-MATCH."
  (nth 2 tag-match))

(defun ectags-match-kind (tag-match)
  "Return the kind from the TAG-MATCH."
  (nth 3 tag-match))

(defun ectags-match-linenumber (tag-match)
  "Return the line number from the TAG-MATCH."
  (nth 4 tag-match))

(defun ectags-match-tag-info (tag-match)
  "Return the info from the TAG-MATCH."
  (nth 5 tag-match))

(defun ectags-kind-full (kind)
  "Given a tag KIND, it expands to the full name if it's not already."
  ;; TODO: Add for all languages
  (if (> 1 (length kind))
      kind
    (let ((kind-char (string-to-char kind)))
      (cond ((= ?d kind-char) "macro")
            ((= ?e kind-char) "enumerator")
            ((= ?f kind-char) "function")
            ((= ?g kind-char) "enum")
            ((= ?l kind-char) "local variable")
            ((= ?m kind-char) "member")
            ((= ?p kind-char) "prototype")
            ((= ?s kind-char) "structure")
            ((= ?t kind-char) "typedef")
            ((= ?u kind-char) "union")
            ((= ?v kind-char) "global variable")
            ((= ?x kind-char) "external")
            ((= ?z kind-char) "parameter")
            ((= ?L kind-char) "goto label")
            ((= ?c kind-char) "class")
            ((= ?n kind-char) "namespace")
            ((= ?A kind-char) "namespace alias")
            ((= ?N kind-char) "name import")))))

(defun ectags-match (tag-line)
  "Parse the given TAG-LINE and return the the tag as a list."
  (let* ((tag-name (if (string-match "^\\([^\t]+\\)" tag-line)
                       (match-string-no-properties 1 tag-line)))
         (tfname (if (string-match "^[^\t]+\t\\([^\t]+\\)" tag-line)
                     (match-string-no-properties 1 tag-line)))
         (tpattern (progn
                     (if (string-match "^[^\t]+\t[^\t]+\t/\\(.+\\)/;\"\t" tag-line)
                         (match-string-no-properties 1 tag-line)
                       (if (string-match "^[^\t]+\t[^\t]+\t\\([0-9]+\\);\"\t" tag-line)
                           (match-string-no-properties 1 tag-line)
                         nil))))
         (tkind (if (string-match "kind:\\([^\t]+\\)" tag-line)
                    (match-string-no-properties 1 tag-line)))
         (tline (if (string-match "line:\\([0-9]+\\)" tag-line)
                    (match-string-no-properties 1 tag-line)))
         (tinfo (if (string-match "line:[0-9]+\t\\(.+\\)$" tag-line)
                    (match-string-no-properties 1 tag-line)
                  "")))
    (add-to-list 'ectags--matches
                 (list
                  tag-name
                  tfname
                  tpattern
                  (ectags-kind-full tkind)
                  (string-to-number tline)
                  (s-trim tinfo)))))

(defun ectags-scan-external ()
  "Search the `ectags--file-name' for the `ectags--regexp'.
Calls `ectags-match' for each line that matches."
  ;; vlf-re-search-forward - too slow
  ;; binary search - too slow and the tags file has to be sorted
  (if ectags--file-name
      (dolist (tag-line (s-split
                         "\n"
                         (shell-command-to-string
                          (format
                           ectags-search-command
                           ectags--regexp
                           ectags--file-name))
                         t))
        (ectags-match tag-line))
    (message "No `ectags--file-name' for the current buffer!")))

(defun ectags-seek (tag-name)
  "Seek a match for the TAG-NAME in the current tag table."
  (setq ectags--matches nil)
  (setq ectags--regexp tag-name)

  (ectags-visit-tags-table)

  (ectags-scan-external))

;;* Helper functions
(defun ectags-get-tag-signature (&optional tag-name)
  "Will return the signature of the TAG-NAME (if any)."
  (interactive)

  (if (not tag-name)
      (setq tag-name (thing-at-point 'symbol)))

  (ectags-seek tag-name)

  (if (zerop (length ectags--matches))
      nil
    (let ((tag-info (ectags-match-tag-info (car ectags--matches))))
      (if (string-match "signature:\\([^\t]+\\)" tag-info)
          (match-string-no-properties 1 tag-info)
        nil))))

(defun ectags-sanitize-info (tag-info)
  "Sanitize some prefixes fro the TAG-INFO."
  (s-replace-all
   '(("typeref:typename:" . "type:"))
   tag-info))

(defun ectags-insert-matches ()
  "Insert the `ectags--matches' into the ectags select buffer."
  (get-buffer-create ectags-buffer-name)
  (set-buffer ectags-buffer-name)
  (setq buffer-read-only nil)
  (erase-buffer)

  (when ectags--matches
    (set-buffer ectags-buffer-name)
    (loop for index from 0 below (min (length ectags--matches) ectags-max-candidates)
          do
          (let ((match (nth index ectags--matches)))
            (insert "<" (int-to-string (1+ index)) ">: "
                    (ectags-match-tagname match) " in "
                    (file-relative-name (ectags-match-filename match)) " at "
                    (int-to-string (ectags-match-linenumber match)) " "
                    (ectags-match-pattern match)
                    "\n"
                    (or (ectags-match-kind match) "")
                    "\t" (ectags-sanitize-info (ectags-match-tag-info match)) "\n"))))

  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (goto-char (point-min))
  (ectags-next-tag))

(defun ectags-find (tagname)
  "Actually find TAGNAME and show the tags select buffer."
  (setq ectags--fn-name
        (replace-regexp-in-string
         "%" "%%"
         (or
          (gethash (selected-window) which-func-table) which-func-unknown)))

  (ectags-seek tagname)

  ;; Delete 'local variables' that aren't from the current function
  (dolist (match ectags--matches)
    (when (string-equal
           "local variable"
           (ectags-match-kind match))
      (unless (string-match
               (concat "function:" ectags--fn-name)
               (ectags-match-tag-info match))
        (setq ectags--matches (delete match ectags--matches)))))

  (if (> (length ectags--matches) 0)
      (progn
        (ectags-insert-matches)
        (unless (get-buffer-window ectags-buffer-name)
          (goto-char (point-min))
          (select-window (split-window-vertically))
          (switch-to-buffer ectags-buffer-name)
          (ectags-mode))
        (shrink-window-if-larger-than-buffer)
        (if (and etags-no-select-for-one-match
                 (= 1 (length ectags--matches)))
            (ectags-by-tag-number "1")))
    (progn
      (message "Failed to find tag: %s " tagname)
      (ding))))

(defun ectags-quit-buffer ()
  "Quit ectags buffer deleting it's window."
  (interactive)

  (kill-buffer)
  (delete-window))

(defun ectags-goto-tag ()
  "Goto the tag we currently have the point over in \
an ectags select mode window."
  (interactive)
  (let ((case-fold-search (not ectags-case-sensitive)))
    (beginning-of-line)
    (if (not (looking-at "<"))
        (message "Please put the cursor on a line with a tag")
      (setq overlay-arrow-position (point-marker))
      (re-search-forward ": \\([^ ]+\\) in \\(.*\\) at \\([0-9]+\\) \\(.*\\)$")
      (let ((tag (match-string-no-properties 1))
            (fname (match-string-no-properties 2))
            (lnno (string-to-number (match-string-no-properties 3)))
            (pattern (match-string-no-properties 4)))
        (ectags-quit-buffer)

        (xref-push-marker-stack)
        (find-file fname)

        ;; If the pattern is a line number, goto it
        ;; If not search the pattern and if that fails too, then
        ;; goto the line number
        (let ((patline (string-to-number pattern)))
          (if (not (zerop patline))
              (progn
                (goto-char (point-min))
                (forward-line (1- patline)))

            (goto-char (point-min))
            (if (re-search-forward
                 ;; TODO: Proper escape a regexp!
                 (s-replace-all
                  '(("*" . "\\*")
                    ("+" . "\\+")
                    ("[" . "\\[")
                    ("]" . "\\]"))
                  pattern)
                 (point-max)
                 t)
                ;; search the actual tag
                (re-search-backward (concat "\\<" tag "\\>") (line-beginning-position) t)
              (goto-line lnno))))
        (recenter)))))

(defun ectags-next-tag ()
  "Move to next tag in buffer."
  (interactive)
  (let ((s-pos (point)))
    (end-of-line)
    (if (re-search-forward "^<[0-9]+>: " (point-max) t)
        (beginning-of-line)
      (goto-char s-pos))))

(defun ectags-previous-tag ()
  "Move to previous tag in buffer."
  (interactive)
  (let ((s-pos (point)))
    (beginning-of-line)
    (unless (re-search-backward "^<[0-9]+>: " (point-min) t)
      (goto-char s-pos))))

(defun ectags-by-tag-number (first-digit)
  "Go to the tag by the number in the tag select buffer.
FIRST-DIGIT is the first digit of the number.  If there are less than
ten matches will jump directly.  Otherwise it will ask for a complete
number."
  (let ((tag-num (progn
                   (if (> (length ectags--matches) 9)
                       (read-from-minibuffer "Tag number: " first-digit)
                     first-digit)))
        (current-point (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^<" tag-num ">") nil t)
        (ectags-goto-tag)
      (goto-char current-point)
      (message (concat "Couldn't find tag number " tag-num))
      (ding))))

;;;###autoload
(defun ectags-find-tag-at-point ()
  "Do a find-tag-at-point, and display it.  If only one match is \
found, see the `etags-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (ectags-find (find-tag-default)))

(defun counsel-ectags-function (input)
  "Helper function for `counsel-ectags'.

This calls for the external program to search for the
INPUT in the `ectags--file-name'."
  (if (not ectags--file-name)
      (ectags-visit-tags-table))

  (if (< (length input) 3)
      (counsel-more-chars 3)
    ;; escape the start ^ with another ^ (batch is fun)
    (when (and (eq system-type 'windows-nt)
               (string-match-p "^\\^[^\\^]" input))
      (setq input (concat "^" input)))
    (counsel--async-command
     (format "rg -N %s %s" input ectags--file-name))
    '("" "working...")))

(defun counsel-ectags-action (tag-line)
  "Go to the tag given by `counsel-ectags'.

TAG-LINE is the line that the user selected."
  (ectags-match tag-line)

  (when ectags--matches
    (with-ivy-window
      (xref-push-marker-stack)

      (find-file (file-relative-name (ectags-match-filename (car ectags--matches))))
      (goto-char (point-min))
      (forward-line (1- (ectags-match-linenumber (car ectags--matches))))
      (recenter-top-bottom)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

;;;###autoload
(defun counsel-ectags (&optional initial-input)
  "Use an external program to search the tag file for a tag.

If INITIAL-INPUT is not nil, then insert that input in the
minibuffer initially."
  (interactive)
  (ectags-visit-tags-table)
  (ivy-read "Find tag: "
            #'counsel-ectags-function
            :initial-input (concat "^" initial-input)
            :dynamic-collection t
            :action #'counsel-ectags-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-ectags))

;;;###autoload
(defun ectags-find-tag ()
  "Do a find tag.

If only one match is found, see the `etags-no-select-for-one-match'
variable to decide what to do."
  (interactive)
  (let ((tagname (read-from-minibuffer
                  (format "Find tag (default %s): " (find-tag-default)) nil nil
                  nil 'find-tag-history)))
    (when (string= tagname "")
      (setq tagname (find-tag-default)))
    (ectags-find tagname)))

(defun ectags-current-symbol (&optional limit)
  "Finds the current function and position in argument list."
  (let* ((literal-limits (c-literal-limits))
         (literal-type (c-literal-type literal-limits)))
    (save-excursion
      ;; if this is a string, move out to function domain
      (when (eq literal-type 'string)
        (goto-char (car literal-limits))
        (setq literal-type nil))
      (if literal-type
          nil
        (when (c-on-identifier)
          (let* ((id-on (point-marker))
                 (id-start
                  (progn (c-beginning-of-current-token)
                         ;; are we looking at a double colon?
                         (if (and (= (char-before)  ?:)
                                  (= (char-before (1- (point))) ?:))
                             (progn
                               (backward-char 3)
                               (c-beginning-of-current-token)
                               (point-marker))
                           (point-marker))))
                 (id-end
                  (progn
                    (goto-char id-on)
                    (forward-char)
                    (c-end-of-current-token)
                    (point-marker))))
            (buffer-substring-no-properties id-start id-end)))))))

;;;###autoload
(defun ectags-print-current-symbol-info ()
  "Return the ectags info associated with the current symbol.
Non scoped verison for more conservative languages."
  (let* ((eldoc-sym (ectags-current-symbol (- (point) 1000))))
    (ectags-seek eldoc-sym)
    (when (> (length ectags--matches) 0)
      (let ((tag-info (ectags-match-tag-info (car ectags--matches))))
        (setq tag-info (replace-regexp-in-string "end:[0-9]+" "" tag-info))
        (setq tag-info (replace-regexp-in-string "typeref:typename:" "type:" tag-info))
        tag-info))))

(defvar ectags-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<return>") #'ectags-goto-tag)
    (define-key map (kbd "n") #'ectags-next-tag)
    (define-key map (kbd "p")  #'ectags-previous-tag)
    (define-key map (kbd "q") #'ectags-quit-buffer)
    (define-key map (kbd "C-g") #'ectags-quit-buffer)
    (define-key map (kbd "1") (lambda () (interactive) (ectags-by-tag-number "1")))
    (define-key map (kbd "2") (lambda () (interactive) (ectags-by-tag-number "2")))
    (define-key map (kbd "3") (lambda () (interactive) (ectags-by-tag-number "3")))
    (define-key map (kbd "4") (lambda () (interactive) (ectags-by-tag-number "4")))
    (define-key map (kbd "5") (lambda () (interactive) (ectags-by-tag-number "5")))
    (define-key map (kbd "6") (lambda () (interactive) (ectags-by-tag-number "6")))
    (define-key map (kbd "7") (lambda () (interactive) (ectags-by-tag-number "7")))
    (define-key map (kbd "8") (lambda () (interactive) (ectags-by-tag-number "8")))
    (define-key map (kbd "9") (lambda () (interactive) (ectags-by-tag-number "9")))
    map)
  "Keymap for `ectags-mode-map'.")

(defvar ectags-mode-highlights
  '(("^<\\([0-9]+\\)>: \\([^ ]+\\) in \\(.+\\) at \\([0-9]+\\) \\(.*\\)$"
     (1 font-lock-warning-face)
     (2 font-lock-function-name-face)
     (3 font-lock-string-face)
     (4 font-lock-warning-face)
     (5 font-lock-comment-face)))
  "Highlight for ectags `font-lock-keywords'.")

(unless ectags-search-command
  (unless (executable-find "rg")
    (if (executable-find "grep")
        (setq ectags-search-command "grep \"^%s\\b\" %s")
      (message "ERROR: rg or grep not installed! We cannot search tag files")
      (setq ectags-search-command nil))))

;;;###autoload
(define-derived-mode ectags-mode fundamental-mode "Ectags Select"
  "Major mode for browsing through exuberant ctags.
Global bindings:
\\{ectags-mode-map}"
  (setq font-lock-defaults '(ectags-mode-highlights))
  (set-syntax-table text-mode-syntax-table)
  (use-local-map ectags-mode-map)

  (setq-local show-trailing-whitespace nil)
  (setq-local overlay-arrow-position nil)
  (run-hooks 'ectags-mode-hook))

;;* xref integration
(defvar ectags--xref-count 0)

(defun ectags--xref-make (tag)
  "Return a xref object from the TAG."
  (setq ectags--xref-count (1+ ectags--xref-count))
  (xref-make (concat
              "<" (number-to-string ectags--xref-count) "> "
              (ectags-match-tagname tag)
              "\tkind: " (ectags-match-kind tag)
              "\n" (ectags-match-tag-info tag))
             (xref-make-file-location
              (ectags-match-filename tag)
              (ectags-match-linenumber tag)
              0)))

(defun ectags-xref-find-symbol (symbol)
  "Find the given SYMBOL in the tagfile.

Return a list of xref objects."
  (setq ectags--xref-count 0)

  (ectags-seek symbol)
  (if (> (length ectags--matches) 0)
      (mapcar #'ectags--xref-make ectags--matches)))

;;;###autoload
(defun ectags-xref-backend ()
  "Ectags backend for Xref."
  'ectags)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ectags)))
  "Return the current symbol name."
  (let ((current-symbol (symbol-at-point)))
    (when current-symbol
      (symbol-name current-symbol))))

(cl-defmethod xref-backend-definitions ((_backend (eql ectags)) symbol)
  "Define the ectags backend for finding definitions."
  (ectags-xref-find-symbol symbol))

(cl-defmethod xref-backend-references ((_backend (eql ectags)) symbol)
  "Define the ectags backend for finding references."
  (ectags-xref-find-symbol symbol))

(cl-defmethod xref-backend-apropos ((_backend (eql ectags)) symbol)
  "Define the ectags backend for finding apropos."
  (ectags-xref-find-symbol symbol))

(defun ectags--xref-goto-by-number (number)
  "Same as `ectags-by-tag-number', but for the xref buffer.

NUMBER is the tag number to jump to."
  (interactive)

  (goto-char (point-min))
  (forward-line)
  (beginning-of-line)

  (if (<= number 0)
      nil
    (let (succeded)
      (if (= number 1)
          (setq succeded t)
        (dotimes (_i number)
          (setq succeded (re-search-forward "^[0-9]+" (point-max) t))))

      (when succeded
        (with-slots (file line column) (xref-item-location (xref--item-at-point))
          (delete-window (get-buffer-window xref-buffer-name))
          (find-file file)
          (goto-line line)
          (forward-char column)
          (recenter))))))

;;;###autoload
(defun ectags-xref-setup (&optional window-behaviour)
  "Use the `ectags-xref-backend' as the xref backed.

If WINDOW-BEHAVIOUR is non nil it will also call the
`ectags-xref-set-window-behaviour'."
  (add-to-list 'xref-backend-functions 'ectags-xref-backend)

  (if window-behaviour
      (ectags-xref-set-window-behaviour)))

(defun ectags-xref-set-window-behaviour ()
  "Will setup the window appearance and keymaps as follows:

1. The window will be displayed at the bottom of the screen, \
taking 0.2 of the Emacs height and never taking over another window \
2. After jumping to a tag, the window will close itself.
3. keys 0-9 jumps to the coresponding tag

WARNING: this will not run `xref-after-jump-hook'"
  (add-hook 'xref-after-jump-hook
            (lambda ()
              (when (get-buffer-window xref-buffer-name)
                (delete-window (get-buffer-window xref-buffer-name)))))

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*xref*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2)))

  (define-key xref--xref-buffer-mode-map (kbd "1") (lambda () (interactive) (ectags--xref-goto-by-number 1)))
  (define-key xref--xref-buffer-mode-map (kbd "2") (lambda () (interactive) (ectags--xref-goto-by-number 2)))
  (define-key xref--xref-buffer-mode-map (kbd "3") (lambda () (interactive) (ectags--xref-goto-by-number 3)))
  (define-key xref--xref-buffer-mode-map (kbd "4") (lambda () (interactive) (ectags--xref-goto-by-number 4)))
  (define-key xref--xref-buffer-mode-map (kbd "5") (lambda () (interactive) (ectags--xref-goto-by-number 5)))
  (define-key xref--xref-buffer-mode-map (kbd "6") (lambda () (interactive) (ectags--xref-goto-by-number 6)))
  (define-key xref--xref-buffer-mode-map (kbd "7") (lambda () (interactive) (ectags--xref-goto-by-number 7)))
  (define-key xref--xref-buffer-mode-map (kbd "8") (lambda () (interactive) (ectags--xref-goto-by-number 8)))
  (define-key xref--xref-buffer-mode-map (kbd "9") (lambda () (interactive) (ectags--xref-goto-by-number 9))))

(provide 'ectags)

;;; ectags.el ends here

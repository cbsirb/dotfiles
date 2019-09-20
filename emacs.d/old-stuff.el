;;
;; Things that I used once, maybe will use again
;;

(require 'ansi-color)

(defun user/colorize-compilation-buffer ()
  "Colorize a compilation mode buffer.
Taken from http://stackoverflow.com/a/3072831/355252."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defvar user/compile-process nil
  "The current compilation process or nil if none.")

(defun user/bury-buffer (window buffer)
  "Bury the BUFFER and switch to the previous buffer in WINDOW, ignoring
errors.

If the previous buffer cannot be found for the WINDOW, then it will simply
delete the WINDOW."
  (ignore-errors
    (unless (switch-to-prev-buffer window)
      (delete-window window))
    (bury-buffer buffer)))

(defun user/compile-start (proc)
  (when (string-equal (buffer-name (current-buffer)) "*compilation*")
    (setq user/compile-process proc)))

(defun user/compile-done (buffer _msg)
  (when (string-equal "*compilation*" (buffer-name buffer))
    (let* ((exit-status (process-exit-status user/compile-process))
           (has-errors  (/= 0 exit-status))
           (window (get-buffer-window buffer)))

      (when (and window (not has-errors))
        (run-at-time "1 sec" nil #'user/bury-buffer window buffer)))

    (setq user/compile-process nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra user/hydra-smerge
  (:color pink
          :hint nil
          :pre (smerge-mode 1)
          ;; Disable `smerge-mode' when quitting hydra if
          ;; no merge conflicts remain.
          :post (smerge-auto-leave))

  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("q" nil "cancel" :color blue))

(bind-key "C-c C-m" #'user/hydra-smerge/body)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mouse-copy
  :ensure nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eldoc-box
  :disabled
  :hook ((prog-mode . eldoc-box-hover-at-point-mode)
         (prog-mode . eldoc-box-hover-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package selected
  :disabled
  :demand t
  :config
  (selected-global-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package objed
  :disabled
  :config
  (add-to-list 'objed-keeper-commands 'undo-tree-undo)
  (add-to-list 'objed-keeper-commands 'undo-tree-redo)

  (define-key objed-op-map "x" 'counsel-M-x)

  (objed-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package user-window
  :load-path "lisp"
  :bind (("C-c w" . #'user/windows-hydra/body)
         ([remap exchange-point-and-mark] . #'exchange-point-and-mark-no-activate))
  :preface
  (defhydra user/windows-hydra (:color pink)
    "
^
^Movement^           ^Window^            ^Zoom^
^────────^───────────^──────^───────────^────^──────────────
_q_ quit             _b_ balance        _-_ out
_3_ split right      _i_ heighten       _+_ in
_2_ split below      _j_ narrow         _=_ reset
_1_ delete others    _k_ lower          ^^
_d_ dedicated        _l_ widen          ^^
_m_ minibuffer       _f_ fullscreen     ^^
_o_ other            ^^                 ^^
^^                   ^^                 ^^
"
    ("q" nil)
    ("b" balance-windows :color blue)
    ("d" user/toggle-current-window-dedication :color blue)
    ("f" toggle-frame-fullscreen :color blue)
    ("i" enlarge-window)
    ("j" shrink-window-horizontally)
    ("k" shrink-window)
    ("l" enlarge-window-horizontally)
    ("o" other-window)
    ("m" user/switch-to-minibuffer :color blue)
    ("1" delete-other-windows :color blue)
    ("2" split-window-right :color blue)
    ("3" split-window-below :color blue)
    ("-" text-scale-decrease)
    ("+" text-scale-increase)
    ("=" (text-scale-increase 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package powerline
  :disabled
  :if (image-type-available-p 'xpm)
  :preface
  (defun powerline-info-theme ()
    "Setup the default mode-line."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" face0 'l)
                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size face0 'l))
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face0 'l))
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       (when (and (boundp 'which-func-mode) which-func-mode)
                                         (powerline-raw which-func-format face0 'l))
                                       (powerline-raw " " face0)
                                       (funcall separator-left face0 face1)
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face1 'l))
                                       (powerline-raw minions-mode-line-modes face1 'l)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (powerline-vc face2 'r)
                                       (when (and (bound-and-true-p nyan-mode)
                                                  (not (eq major-mode 'exwm-mode)))
                                         (powerline-raw (list (nyan-create)) face2 'l))))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (funcall separator-right face2 face1)
                                       (unless window-system
                                         (powerline-raw (char-to-string #xe0a1) face1 'l))
                                       (unless (eq major-mode 'exwm-mode)
                                         (powerline-raw "%4l" face1 'l))
                                       (unless (eq major-mode 'exwm-mode)
                                         (powerline-raw ":" face1 'l))
                                       (unless (eq major-mode 'exwm-mode)
                                         (powerline-raw "%3c" face1 'r))
                                       (funcall separator-right face1 face0)
                                       (unless (eq major-mode 'exwm-mode)
                                         (powerline-raw "%p" face0 'r))
                                       (when powerline-display-hud
                                         (powerline-hud face0 face2))
                                       (when display-battery-mode
                                         (funcall separator-right face0 face1))
                                       (when display-battery-mode
                                         (powerline-raw battery-mode-line-string face1 'r))
                                       (when display-time-mode
                                         (funcall separator-right face1 face0))
                                       (when display-time-mode
                                         (powerline-raw display-time-string face0 'r))
                                       (powerline-fill face0 0)
                                       )))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))
  :custom
  (powerline-display-buffer-size nil)
  (powerline-display-mule-info nil)
  (powerline-display-hud nil)
  (powerline-gui-use-vcs-glyph t)
  :config
  (when (display-graphic-p)
    (powerline-info-theme)
    (remove-hook 'focus-out-hook 'powerline-unset-selected-window)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

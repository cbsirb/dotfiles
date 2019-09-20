(require 'cl-macs)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(csetq window-divider-default-bottom-width 3)
(csetq window-divider-default-right-width 3)
(window-divider-mode t)

(csetq exwm-workspace-show-all-buffers t)
(csetq exwm-layout-show-all-buffers t)

(csetq exwm-floating-border-width 3)
(csetq exwm-floating-border-color "orange")

(defvar user-volume-cmd "pactl set-sink-volume @DEFAULT_SINK@")
(defvar user-volume-up-cmd (concat user-volume-cmd " +3%"))
(defvar user-volume-down-cmd (concat user-volume-cmd " -3%"))
(defvar user-volume-toggle-cmd (concat user-volume-cmd " toggle"))

(defvar user-player-cmd "playerctl")
(defvar user-player-next-cmd (concat user-player-cmd " next"))
(defvar user-player-prev-cmd (concat user-player-cmd " previous"))
(defvar user-player-toggle-cmd (concat user-player-cmd " play-pause"))

(defvar user-backlight-cmd "xbacklight")
(defvar user-backlight-inc-cmd (concat user-backlight-cmd " -inc 10"))
(defvar user-backlight-dec-cmd (concat user-backlight-cmd " -dec 10"))

(defmacro defcmd (cmd)
  `(lambda ()
     (interactive)
     (start-process-shell-command "hotkey" nil ,cmd)))

(defun exwm-get-buffer-by-class (class)
  (cl-find-if (lambda (buf-alist)
                (string-match-p class (buffer-local-value 'exwm-class-name (cdr buf-alist))))
              exwm--id-buffer-alist))

(defun exwm-jump-to-window-by-class (class)
  (interactive "s")
  (let* ((buffer (cdr (exwm-get-buffer-by-class class)))
         (bufwin (get-buffer-window buffer t)))
    (when buffer
      (if bufwin
          (select-window bufwin)
        (select-frame-set-input-focus (buffer-local-value 'exwm--frame buffer))
        (display-buffer-same-window buffer nil)))
    buffer))

(defmacro exwm-jump-to-window (class)
  `(lambda () (interactive) (exwm-jump-to-window-by-class ,class)))

(defun exwm-jump-to-firefox ()
  (interactive)
  (exwm-jump-to-window-by-class "^Nightly$"))

(defun exwm-jump-to-spotify ()
  (interactive)
  (unless (exwm-jump-to-window-by-class "^Spotify")
    (when (exwm-jump-to-firefox)
      (exwm-input--fake-key ?\M-2)
      t)))

(defun exwm-jump-to-slack ()
  (interactive)
  (unless (exwm-jump-to-window-by-class "^Slack")
    (when (exwm-jump-to-firefox)
      (exwm-input--fake-key ?\M-1)
      t)))

(defun other-window-all-frames ()
  (interactive)
  (other-window 1 t))

(defun alternate-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun exwm-move-window-other-display ()
  (interactive)
  (when exwm--id
    (let ((buffer (current-buffer)))
      (exwm-workspace-move-window
       (exwm-workspace--workspace-from-frame-or-index
        (mod (1+ exwm-workspace-current-index) (length (exwm-randr--get-monitors)))))
      (select-frame-set-input-focus (buffer-local-value 'exwm--frame buffer)))))

(defun exwm-swap-displays ()
  (interactive)
  (exwm-workspace-swap
   (exwm-workspace--workspace-from-frame-or-index
    exwm-workspace-current-index)
   (exwm-workspace--workspace-from-frame-or-index
    (mod (1+ exwm-workspace-current-index) (length (exwm-randr--get-monitors))))))

(defun suggest-google-search-terms (input)
  (when (not (string-blank-p input))
    (with-current-buffer (url-retrieve-synchronously
                          (url-encode-url
                           (format "https://suggestqueries.google.com/complete/search?client=firefox&q=%s" input))
                          t t 1)
      (goto-char (1+ url-http-end-of-headers))
      (cl-loop for term in `(,input ,@(cadr (json-parse-buffer :array-type 'list)))
               collect term))))

(defun suggest-ddg-search-terms (input)
  (or
   (ivy-more-chars)

   (when (not (string-blank-p input))
     (with-current-buffer (url-retrieve-synchronously
                           (format "https://duckduckgo.com/ac/?q=%s" input)
                           t t 1)
       (goto-char (1+ url-http-end-of-headers))
       (cl-loop for phrase in `(((phrase . ,input)) ,@(json-parse-buffer :array-type 'list :object-type 'alist))
                collect (cdar phrase))))))

(defun exwm-net-search ()
  (interactive)

  (let ((ivy-use-selectable-prompt t))
    (ivy-read
     "Search: "
     #'suggest-google-search-terms
     :require-match nil
     :history 'exwm-net-search-history
     :dynamic-collection t
     :sort nil
     :action (lambda (term)
               (browse-url (url-encode-url (concat "https://duckduckgo.com/?q=" term)))
               (exwm-jump-to-firefox)))))

;; Jumping to specific applications
(exwm-input-set-key (kbd "s-w") #'exwm-jump-to-firefox)
(exwm-input-set-key (kbd "s-s") #'exwm-jump-to-spotify)
(exwm-input-set-key (kbd "s-d") #'exwm-jump-to-slack)

(exwm-input-set-key (kbd "s--") #'alternate-buffer)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)
(exwm-input-set-key (kbd "s-q") #'kill-buffer-and-window)
(exwm-input-set-key (kbd "s-o") #'other-window-all-frames)
(exwm-input-set-key (kbd "s-i") #'exwm-workspace-switch-to-buffer)
(exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-u") #'exwm-net-search)
(exwm-input-set-key (kbd "s-R") (defcmd "autorandr --change"))
(exwm-input-set-key (kbd "s-X") #'exwm-move-window-other-display)
(exwm-input-set-key (kbd "s-x") #'exwm-swap-displays)

(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") (defcmd user-volume-up-cmd))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") (defcmd user-volume-down-cmd))
(exwm-input-set-key (kbd "<XF86AudioMute>") (defcmd user-volume-toggle-cmd))
(exwm-input-set-key (kbd "<C-s-up>") (defcmd user-volume-up-cmd))
(exwm-input-set-key (kbd "<C-s-down>") (defcmd user-volume-down-cmd))

(exwm-input-set-key (kbd "<XF86AudioPlay>") (defcmd user-player-toggle-cmd))
(exwm-input-set-key (kbd "<XF86AudioNext>") (defcmd user-player-next-cmd))
(exwm-input-set-key (kbd "<XF86AudioPrev>") (defcmd user-player-prev-cmd))
(exwm-input-set-key (kbd "s-m") (defcmd user-player-toggle-cmd))
(exwm-input-set-key (kbd "s-.") (defcmd user-player-next-cmd))
(exwm-input-set-key (kbd "s-,") (defcmd user-player-prev-cmd))

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") (defcmd user-backlight-inc-cmd))
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (defcmd user-backlight-dec-cmd))

(exwm-input-set-key (kbd "s-p") #'counsel-linux-app)
(exwm-input-set-key (kbd "<s-return>") (defcmd "alacritty"))

;; i3lock doesn't work if it doesn't have a buffer
(exwm-input-set-key (kbd "M-s-l") (lambda ()
                                    (interactive)
                                    (async-start-process
                                     "i3lock" "i3lock"
                                     (lambda (process) (run-with-timer 1 nil #'kill-buffer (process-buffer process))))))

(csetq exwm-input-simulation-keys
       `((,(kbd "M-w") . ,(kbd "C-c"))
         (,(kbd "C-y") . ,(kbd "C-v"))
         (,(kbd "C-c C-c") . ,(kbd "C-c"))
         (,(kbd "C-x C-x") . ,(kbd "C-x"))
         (,(kbd "C-a") . ,(kbd "<home>"))
         (,(kbd "C-e") . ,(kbd "<end>"))
         (,(kbd "C-x h") . ,(kbd "C-a"))
         ))

(defmacro exwm-switch-to-workspace-key (ws-num)
  `(exwm-input-set-key (kbd (concat "s-" (number-to-string ,ws-num)))
                       (lambda ()
                         (interactive)
                         (exwm-workspace-switch (1- ,ws-num))))
  ;; (push (kbd (concat "s-" (number-to-string ws-num))) exwm-input-prefix-keys)
  )

(csetq exwm-input-prefix-keys '(?\C-x ?\C-h ?\M-x ?\M-& ?\M-:))

;; Set 1 workspace per monitor
(csetq exwm-workspace-number 2)

;; Switch to monitor workspace with s-N (1-based) (TODO: maybe drop this ?!)
(exwm-switch-to-workspace-key 1)
(exwm-switch-to-workspace-key 2)

(require 'exwm)

(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ": "
           (if (<= (length exwm-title) 50) exwm-title
             (concat (substring exwm-title 0 49) "...")))))

(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

(require 'exwm-xim)
(exwm-xim-enable)

(require 'exwm-randr)

(csetq exwm-randr-workspace-monitor-plist
       '(0 "DP-1-2-8" 1 "DP-1-2-1-8"))

(exwm-randr-enable)

(defun user-randr-changed ()
  (message "RandR changed..."))

(add-hook 'exwm-randr-screen-change-hook #'user-randr-changed)

(display-time-mode t)
(display-battery-mode t)

(csetq battery-mode-line-format "%b%p")
(csetq display-time-24hr-format t)
(csetq display-time-load-average-threshold 100.0)

;; will be displayed worngly with powerline
(csetq global-mode-string '())

;; (csetq mode-line-end-spaces
;;        (list (propertize " " 'display '(space :align-to (- right 20)))
;;              'battery-mode-line-string " "
;;              'display-time-string))

;; (csetq mode-line-format
;;        '("%e" mode-line-front-space mode-line-mule-info mode-line-client
;;          mode-line-modified mode-line-remote mode-line-frame-identification
;;          mode-line-position
;;          minions-mode-line-modes mode-line-misc-info
;;          mode-line-buffer-identification
;;          mode-line-end-spaces))

;; autostart some things (maybe another program to handle all these, but I'm lazy for now...)
(start-process-shell-command "autorandr" nil "autorandr --change")
(start-process-shell-command "mouse" nil "xsetroot -cursor_name left_ptr")
(start-process-shell-command "keyboard" nil "xset r rate 250 44")
(start-process-shell-command "compositor" nil "compton -b")
(start-process-shell-command "notifications" nil "dunst")
(start-process-shell-command "network" nil "nm-applet")
(start-process-shell-command "touchpad" nil "xinput set-prop \"DLL07D1:01 044E:120B\" \"libinput Tapping Enabled\" 1")

(exwm-enable)

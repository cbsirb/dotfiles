;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:

;; The first thing to run

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

;; Do not resize the frame while starting up
(setq frame-inhibit-implied-resize t)

(setq default-frame-alist
      '((vertical-scroll-bars)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (scroll-bar-mode . -1)
        (menu-bar-mode . -1)
        (tool-bar-mode . -1)
        (width . 110)
        (height . 50)))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ;; ("elpa-devel" . "https://elpa.gnu.org/devel/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ;; ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ))

(setq package-quickstart t)
(setq package-enable-at-startup t)
(advice-add #'package--ensure-init-file :override #'ignore)
(advice-add #'x-apply-session-resources :override #'ignore)

(setq load-prefer-newer t)

(setq site-run-file nil)

(setq native-comp-async-report-warnings-errors 'silent)

;; startup.el
(setq inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")
(setq inhibit-startup-buffer-menu t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq inhibit-x-resources t)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(set-face-attribute 'default nil
                    :family "Iosevka Extended"
                    :height 105
                    :weight 'semi-bold)

(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka Extended"
                    :height 105
                    :weight 'semi-bold)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :weight 'semi-bold)

(unless after-init-time
  ;; prevent flash of unstyled modeline at startup
  (setq-default mode-line-format nil))


(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-quickstart t)
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)
(advice-add #'x-apply-session-resources :override #'ignore)

(setq default-frame-alist
      '((vertical-scroll-bars)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (scroll-bar-mode . -1)
        (menu-bar-mode . -1)
        (tool-bar-mode . -1)
        (width . 90)
        (height . 45)))

(setq frame-inhibit-implied-resize t)

(set-face-attribute 'default nil
                    :family "Iosevka Fixed SS10 Extended"
                    :height 105
                    :weight 'normal)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 105
                    :weight 'normal)

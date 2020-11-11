(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

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
        (width . 110)
        (height . 50)))

(setq frame-inhibit-implied-resize t)

(set-face-attribute 'default nil
                    :family "Iosevka Fixed Extended"
                    :height 105
                    :weight 'normal)

(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka Fixed Extended"
                    :weight 'normal)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :weight 'normal)

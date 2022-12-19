;;; feeds.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; My feeds. Separate them from the init.el

;;; Code:

(use-package elfeed
  :commands (elfeed elfeed-update)
  :preface
  (defun user/elfeed-visual-line-toggle ()
    (interactive)
    (setq-local fill-column 120)
    (visual-fill-column-mode 'toggle)
    (visual-line-mode 'toggle))

  :general
  ("C-x w" #'elfeed)
  (:keymaps 'elfeed-show-mode-map
            "v" #'user/elfeed-visual-line-toggle
            "w" #'elfeed-show-visit)
  (:keymaps 'elfeed-search-mode-map
            "U" #'elfeed-update)

  :init
  (csetq elfeed-search-title-max-width 100)
  (csetq elfeed-search-title-min-width 30)
  (csetq elfeed-search-filter "+unread")
  (csetq elfeed-feeds
         '(("https://www.youtube.com/feeds/videos.xml?channel_id=UC3ts8coMP645hZw9JSD3pqQ" tech)        ;; Andreas Kling
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCgWip0vxtqu34rZrFeCpUow" tech)        ;; Tim Morgan
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-_ZS52_S34Spofk4-lAedA" tech)        ;; Valtteri Koskivuori
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3KNgU1jlleVcn8f7DY9bdg" tech)        ;; Dimitriy Kubyshkin
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7S6EpMQ5QNGRg7uJmJWXNw" tech)        ;; QueueQueueHack
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBa659QWEk1AI4Tg--mrJ2A" tech)        ;; Tom Scott
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHC4G4X-OR5WkY-IquRGa3Q" tech)        ;; Tom Scott 2nd
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKTehwyGCKF-b2wo0RKwrcg" tech)        ;; Bisqwit
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbfYPyITQ-7l4upoX8nvctg" tech)        ;; Two Minute Papers
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCS0N5baNlQWJCUrhCEo8WlA" tech)        ;; Ben Eater
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxHAlbZQNFU2LgEtiqd2Maw" tech)        ;; Jason Turner
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCG7yIWtVwcENg_ZS-nahg5g" tech)        ;; CNLohr
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRDQEDxAVuxcsyeEoOpSoRA" tech)        ;; Mark Furneaux
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkf4VIqu3Acnfzuk3kRIFwA" tech)        ;; gotbletu
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUQo7nzH1sXVpzL92VesANw" tech)        ;; DIY Perks
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCy0tKL1T7wFoYcxCe0xjN6Q" tech)        ;; Technology Connections
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCS4FAVeYW_IaZqAbqhlvxlA" tech)        ;; ContextFree
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBLr7ISa_YDy5qeATupf26w" tech)        ;; AlgorithmsLive!
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" tech)        ;; 3blue1brown
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1kBxkk2bcG78YBX7LMl9pQ" tech)        ;; codereport
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_iD0xppBwwsrM9DegC5cQQ" tech)        ;; Jon Gjengset (rust)
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCguWV1bZg1QiWbY32vGnOLw" tech)        ;; Bitwise
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJ0-OtVpF0wOKEqT2Z1HEtA" tech)        ;; ElectroBOOM
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3azLjQuz9s5qk76KEXaTvA" tech)        ;; suckerpinch
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCq7dxy_qYNEBcHqQVCbc20w" tech)        ;; WhatsACreel
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UClcE-kVhqyiHCcjYwcpfj9w" tech)        ;; LiveOverflow
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" tech)        ;; Protesilaos Stavrou
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUzQJ3JBuQ9w-po4TXRJHiA" tech)        ;; jdh
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCq-PF3nIPg5KO8po0dHcXsQ" tech)        ;; Marko Tasic
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-yuWVUplUJZvieEligKBkA" tech)        ;; javidx9
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrW38UKhlPoApXiuKNghuig" tech)        ;; Systems with JT
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBLthWZ0AdU-tTpKCBs8PGQ" tech)        ;; Kofybrek
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxwcmRAmBRzZMNS37dCgmHA" tech)        ;; RoboNuggie
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCov_51F0betb6hJ6Gumxg3Q" tech)        ;; AI and Games
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC6mIxFTvXkWQVEHPsEdflzQ" tech)        ;; GreatScott!
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCE5dIscvDxrb7CD5uiJJOiw" tech)        ;; Adrian's digital basement
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCV0t1y4h_6-2SqEpXBXgwFQ" tech)        ;; AngetheGreat
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrqM0Ym_NbK1fqeQG2VIohg" tech)        ;; Tsoding Daily

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCNf56PUyMI0wUyZ8KRhg2AQ" cinema)      ;; Cinema Nippon
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7GV-3hrA9kDKrren0QMKMg" cinema)      ;; CinemaTyler
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAGkSxTlleqxRauEqupyVPw" cinema)      ;; The Cinema Cartography
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCErSSa3CaP_GJxmFpdjG9Jw" cinema)      ;; Lessons from the Screenplay
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKlnkrjfJKGpuZ3-kkhiaXQ" cinema)      ;; Film Histories
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbphDfwSJmxk1Ny_3Oicrng" cinema)      ;; Storytellers
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCx0L2ZdYfiq-tsAXb8IXpQg" cinema)      ;; JustWrite
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxO_ya-RmAXCXJCU54AxYFw" cinema)      ;; New Frame Plus
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCVmAQ9b63_oxitZ9Zgh8nA" cinema)      ;; Lucas Gravey
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZ7g7HfH1gWmhgxW47IcW7Q" cinema)      ;; Beyond the Frame
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCI9DUIgtRGHNH_HmSTcfUbA" cinema)      ;; The Closer Look
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCL5kBJmBUVFLYBDiSiK1VDw" cinema)      ;; Criswell
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWTFGPpNQ0Ms6afXhaWDiRw" cinema)      ;; Now You See It
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UChcSRqitXAm2BXtxTzP1azQ" cinema)      ;; kubricklynch

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQD-0MjUbDBwm2UTVYr0Dag" history)     ;; Suibhne
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKMnl27hDMKvch--noWe5CA" history)     ;; Cogito
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqBiWcuTF8IaLH7wBqnihsQ" history)     ;; Told in Stone
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCytq3DBifaIwY376ARqdxWA" history)     ;; Scenic Routes to the Past

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCO6nDCimkF79NZRRb8YiDcA" art)         ;; Storied (Monstrum)
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmQThz1OLYt8mb2PU540LOA" art)         ;; The Art Assignment
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3R-xanNgtoa8b7gpVexVlA" art)         ;; Smarthistory

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoW-z7WLeh9IAcr85tnDG6Q" literature)  ;; Malazan

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q" science)     ;; Kurzgesagt
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-3SbfTPJsL8fJAPKiVqBLg" science)     ;; Deep Look
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCotwjyJnb-4KW7bmsOoLfkg" science)     ;; Art of the Problem
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCzR-rom72PHN9Zg7RML9EbA" science)     ;; PBS Eons
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9Lp_AA5M2cMGrlvnnIns-g" science)     ;; Bizarre Beasts
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSIvk78tK2TiviLQn4fSHaw" science)     ;; Up and Atom
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMk_WSPy3EE16aK5HLzCJzw" science)     ;; NativLang

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8XjmAEDVZSCQjI150cb4QA" pop)         ;; Knowing Better
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2C_jShtL725hvbm1arSV9w" pop)         ;; GCP Grey
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCuPgdqQKpq4T4zeqmTelnFg" pop)         ;; kaptainkristian
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-gjznzViwMols6dz89qLbg" pop)         ;; Entertain The Elk
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHiwtz2tCEfS17N9A-WoSSw" pop)         ;; Pop Culture Detective
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqJ-Xo29CKyLTjn6z2XwYAw" pop)         ;; Game Maker's Toolkit
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCaPRCWnFAzeI3_tr--Qw5qg" pop)         ;; Human Interests
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCs_bV79AmugXVvjaASibPnw" pop)         ;; KhAnubis
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXkNod_JcH7PleOjwK_8rYQ" pop)         ;; Polyphonic
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9RM-iSvTu1uPJb8X5yp3EQ" pop)         ;; Wendover
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTWKe1zATFV6d0o6oLS9sgw" pop)         ;; Extremities
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCi7l9chXMljpUft67vw78qw" pop)         ;; Sideways
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyNtlmLB73-7gtlBz00XOQQ" pop)         ;; Folding Ideas
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKzJFdi57J53Vr_BkTfN3uQ" pop)         ;; Primer

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdPPmAd9qlG80qeSm74-eww" weird)       ;; In Praise of Shadows
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCF9LcCkPbnCxiTQhbKa-xvw" weird)       ;; kirstenlepore

           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCct9aR7HC79Cv2g-9oDOTLw" religion)    ;; ReligionForBreakfast

           ("https://andreyorst.gitlab.io/feed.xml" tech)
           ("https://fabiensanglard.net/rss.xml" tech)
           ("https://danluu.com/atom.xml" tech)
           ("https://rachelbythebay.com/w/atom.xml" tech)
           ("https://trofi.github.io/feed/atom.xml" tech)
           ("https://virtuallyfun.com/wordpress/feed" tech)
           ("https://stopa.io/feed.rss" tech)
           ("https://kliu.io/index.xml" tech)
           ("https://drewdevault.com/blog/index.xml" tech)
           ("https://protesilaos.com/codelog.xml" tech)
           ("https://www.murilopereira.com/index.xml" tech)
           ("https://venam.nixers.net/blog/feed.xml" tech)
           ("https://vermaden.wordpress.com/feed.xml" tech)
           ("https://www.gridbugs.org/feed.xml" tech)
           ("https://blog.benjojo.co.uk/rss.xml" tech)
           ("https://lemire.me/blog/feed/" tech)
           ("https://blog.royalsloth.eu/posts/index.xml" tech)
           ("https://maskray.me/blog/atom.xml" tech)
           ("https://nullprogram.com/feed/" tech)
           ("https://blog.yiningkarlli.com/feeds/posts/default/index.xml" tech)
           ("http://blog.pkh.me/rss.xml" tech)
           ("https://www.yet-another-blog.com/index.xml" tech)
           ("https://xeiaso.net/blog.rss" tech)
           ("https://crpgaddict.blogspot.com/feeds/posts/default" pop)
           ("https://obscuritory.com/feed/" pop)
           ("https://planet.emacslife.com/atom.xml" emacs)
           ("https://www.phoronix.com/rss.php" linux phoronix)
           ("https://xkcd.com/atom.xml" comic)
           ))
  :config
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "1 months ago"
                                :remove 'unread)))

(provide 'feeds)
;;; feeds.el ends here

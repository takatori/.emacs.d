(require 'elscreen)
(elscreen-set-prefix-key "\C-z")

(setq elscreen-display-tab 15) ; タブの幅（６以上じゃないとダメ）
(setq elscreen-tab-display-kill-screen nil) ; タブの左端の×を非表示
(setq elscreen-tab-display-control nil) ; header-lineの先頭に[<->]を表示しない

;; elscreen-server
(require 'elscreen-server)

;; elscreen-dired
(require 'elscreen-dired)

;; elscreen-color-theme
(require 'elscreen-color-theme)

(elscreen-start)

(require 'neotree)

;; all-the-icons
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; 隠しファイルをデフォルトで表示
(setq neo-show-hidden-files t)

;; neotree でファイルを新規作成した後、自動的にファイルを開く
(setq neo-create-file-auto-open t)

;; delete-other-window で neotree ウィンドウを消さない
(setq neo-persist-show t)

;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
(setq neo-smart-open t)

;; デフォルトのウィンドウサイズ
(setq neo-window-width 50)

;; 表示を右側に
;; elscreenのタブと干渉するため
(setq neo-window-position 'right)

;; オートリフレッシュ
;;(setq neo-autorefresh t)



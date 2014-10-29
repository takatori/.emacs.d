;;Chrome検索のように、検索文字列が現在のバッファでいくつマッチするのかという情報と現在の位置をモードラインに表示するマイナーモードを提供する

;; anzuを常に有効にする
(global-anzu-mode +1)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000))


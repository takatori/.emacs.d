(require 'bind-key)

;; window の移動
(bind-key* "C-t" 'other-window-or-split)
(bind-key "C-S-t" 'delete-window)


;; C-hをBackSpaceに
(bind-key "C-h" 'backward-delete-char)

;; 行番号を指定して移動する機能をM-zに割り当て
(bind-key "M-z" 'goto-line)

;; コピーをC-lに割り当て
(bind-key "C-l" 'kill-ring-save)

;; コメントアウトをC-;に割り当て
(bind-key "C-;" 'one-line-comment)

;; ESC 連打で無効化
(bind-key "M-ESC ESC" 'keyboard-quit)

;; to LARGE C-x C-u と to small C-x C-l
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; expand-region                           
(bind-key "C-," 'er/expand-region)      
(bind-key "C-M-," 'er/contract-region)  

;; multiple-cursors
(bind-key "<C-M-return>" 'mc/edit-lines)
(smartrep-define-key
    global-map "C-." '(("C-n" . 'mc/mark-next-like-this)
                       ("C-p" . 'mc/mark-previous-like-this)
                       ("*"   . 'mc/mark-all-like-this)))
;; open-junk
(bind-key "C-x j" 'open-junk-file)

;; Google Translate
;;(bind-key (kbd "C-x t") 'google-translate-at-point)
;;(bind-key (kbd "C-x T") 'google-translate-query-translate)

;; C-x C-b をつぶす
;;(bind-key (kbd "C-x C-b") 'buffer-menu)

;; Helm
;; recentf
(bind-key "C-x C-r" 'helm-recentf)

;; show-kill-ring
(bind-key "M-y" 'helm-show-kill-ring)




;; dash (cocoa-emacs only)
(bind-key "C-c d" 'dash-at-point)
(bind-key "C-c e" 'dash-at-point-with-docset)



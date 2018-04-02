(require 'elscreen)
(elscreen-set-prefix-key "\C-z")

(setq elscreen-display-tab 30) ; タブの幅（６以上じゃないとダメ）
(setq elscreen-tab-display-kill-screen nil) ; タブの左端の×を非表示
(setq elscreen-tab-display-control nil) ; header-lineの先頭に[<->]を表示しない

;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; elscreen-server
(require 'elscreen-server)

;; elscreen-dired
(require 'elscreen-dired)

;; 色の変更
(set-face-foreground 'elscreen-tab-current-screen-face "coral")
(set-face-background 'elscreen-tab-current-screen-face ' "#2e333b")
(set-face-underline-p 'elscreen-tab-current-screen-face t)
(set-face-foreground 'elscreen-tab-other-screen-face "#aeb5bd")
(set-face-background 'elscreen-tab-other-screen-face "#2e333b")

(elscreen-start)

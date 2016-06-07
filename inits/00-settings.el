;; create backup file in ~/.emacs.d/backup
(setq make-backup-files t)
(setq backup-directory-alist
  (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
    backup-directory-alist))

;; create auto-save file in ~/.emacs.d/backup
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;; コメントアウトの形式変更
(setq comment-style 'multi-line)

;; ￥をバックスラッシュと交換
;; 165が¥（円マーク） , 92が\（バックスラッシュ）を表す
(define-key global-map [165] [92])

;; 改行コードを表示
;;(setq eol-mnemonic-dos "(CRLF)")
;;(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; インデントをスペースに
(defun my-c-mode-hook ()
  (c-set-style "linux")
  (setq c-basic-offset tab-width))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(setq-default tab-width 4 indent-tabs-mode nil)

;; 対応する括弧をハイライト
(setq show-paren-delay 0)
(show-paren-mode t)

;; 行数表示
(global-linum-mode t)

;; 閉じ括弧自動挿入
(electric-pair-mode 1)

;; 編集時 buffer 再読み込み
(global-auto-revert-mode 1)

;; save-buffer 時，buffer 末尾に空行が常にあるように
(setq require-final-newline t)

;; Emacs の質問を y/n に
(fset 'yes-or-no-p 'y-or-n-p)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; シンボリックリンクを開くときの質問省略
(setq vc-follow-symlinks t)

;; linum-mode をいじって Emacs を高速化
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;; スクロール時の移動量を1に
(setq scroll-step 1)

;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;; 通常のウィンドウで行を折り返さない
(setq-default truncate-lines t)

;; ウィンドウを左右に分割したときに行を折り返さない
(setq-default truncate-partial-width-windows t)

;; ツールバーを非表示 メニューバーを非表示 スクロールバーを非表示
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;;現在行に色を付ける
(global-hl-line-mode 1)

;; ビープ音を消す
(setq visible-bell t)

;; 起動時のウィンドウサイズを設定
(if (boundp 'window-system)
    (setq initial-frame-alist
          (append (list
                   '(width . 180)
                   '(height . 50))
                  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

;; for mac
;; meta-keyをcommadに割り当て
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; diredの表示順を変更する
;; (setq dired-listing-switches "-aBhl  --group-directories-first")

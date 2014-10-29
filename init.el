;; -----------------------------------------------------------------------------
;; ~/.emacs.d/site-lisp 以下全部読み込み
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
;; -----------------------------------------------------------------------------
;; パッケージの取得先追加
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; -----------------------------------------------------------------------------
;; パッケージ自動インストール設定

(require 'cl-lib)

(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    init-loader
    recentf-ext
    magit
    helm
    helm-projectile
    auto-complete
    flycheck
    web-mode
    bind-key
    markdown-mode
    multiple-cursors
    zenburn-theme
    expand-region
    open-junk-file
    anzu
    scss-mode
    google-c-style
    yaml-mode
    open-junk-file
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

;; -----------------------------------------------------------------------------
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")


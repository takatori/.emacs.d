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

(eval-when-compile
  (require 'cl))


(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    init-loader
    recentf-ext
    magit
    helm
    helm-projectile
    helm-ag
    auto-complete
    auto-save-buffers-enhanced
    flycheck
    web-mode
    bind-key
    js2-mode
    coffee-mode
    projectile
    volatile-highlights
    markdown-mode
    multiple-cursors
    zenburn-theme
    expand-region
    anzu
    yascroll
    scss-mode
    google-c-style
    yaml-mode
    open-junk-file
    arduino-mode
    scala-mode2
    ensime
    exec-path-from-shell
    powerline
    rainbow-mode
    rainbow-delimiters
    monokai-theme
    elscreen
    tern
    tern-auto-complete
    terraform-mode    
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







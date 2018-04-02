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
(add-to-list 'exec-path (expand-file-name "~/homebrew/bin")) ;; homebrewで入れたツールを使う
(add-to-list 'exec-path (expand-file-name "~/dev/go-workspace/bin")) ;; go getでインスールしたツールを使う
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
    scala-mode
    ensime
    exec-path-from-shell
    powerline
    rainbow-mode
    rainbow-delimiters
    monokai-theme
    elscreen
    terraform-mode
    go
    groovy-mode
    editorconfig
    org-redmine
    rust-mode
    flycheck-rust    
    quickrun
    flycheck-pos-tip
    toml-mode
    racer
    dockerfile-mode
    php-mode
    nginx-mode
    go-mode
    emojify
    docker
    highlight-symbol
    all-the-icons
    neotree
    restclient
    vlf
    company
    helm-rg
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point (quote symbol))
 '(package-selected-packages
   (quote
    (helm-rg go-eldoc company-go vlf restclient protobuf-mode esa neotree all-the-icons highlight-symbol package-utils docker emojify go-mode apib-mode zenburn-theme yascroll yaml-mode web-mode volatile-highlights toml-mode terraform-mode tern-auto-complete scss-mode recentf-ext rainbow-mode rainbow-delimiters racer quickrun powerline php-mode org-redmine open-junk-file nginx-mode multiple-cursors monokai-theme markdown-mode magit js2-mode init-loader helm-projectile helm-ag groovy-mode google-c-style go flycheck-rust flycheck-pos-tip expand-region exec-path-from-shell ensime elscreen editorconfig dockerfile-mode coffee-mode bind-key auto-save-buffers-enhanced arduino-mode anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

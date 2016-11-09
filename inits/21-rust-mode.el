(require 'rust-mode)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;; racer
(setq racer-rust-src-path "/Users/takatorisatoshi/.rust/rustc-1.12.1/src/")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)


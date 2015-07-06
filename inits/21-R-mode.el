;; R-mode
(require 'ess-site)

(autoload 'R-mode "R-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))


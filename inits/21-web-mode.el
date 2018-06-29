;; web-mode
(require 'web-mode)
; load company mode html backend
(require 'company-web-html)

(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.ect?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs?$"     . web-mode))


(add-hook 'web-mode-hook 'web-mode-hook)
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))

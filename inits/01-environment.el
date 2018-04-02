;; 環境変数をシェルからコピーする
;; http://emacs-jp.github.io/tips/environment-variable
(exec-path-from-shell-copy-envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH"))

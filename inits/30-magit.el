(add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
(if run-windows
    (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe"))
(require 'magit)
(setenv "GIT_ASKPASS" "git-gui--askpass")

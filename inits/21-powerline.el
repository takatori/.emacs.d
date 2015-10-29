;;; Powerline Setting
(require 'powerline)

;; Helper function
;; ディレクトリパスの表示を短くする
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defun powerline-my-theme ()
  "Setup the my mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))

                          (lhs (list (powerline-raw "[" mode-line 'l)
                                     (powerline-major-mode mode-line) ;; majar-mode表示
                                     (powerline-process mode-line) 
                                     (powerline-raw "]" mode-line)
                                     (when (buffer-modified-p)
                                       (powerline-raw "[+]" mode-line))
                                     (when buffer-read-only
                                       (powerline-raw "[RO]" mode-line)) 
                                     (powerline-raw "[%Z]" mode-line) ;; emacs標準のモードラインで[%Z]は文字コードと改行コードを表す
                                     (funcall separator-left mode-line face1) ;; mode-lineとface1の分割 三角形表示
                                     (powerline-vc face1 'r)
                                     (funcall separator-left face1 face2) ;; face1とface2の分割 三角形表示
                                     (powerline-raw
                                      (shorten-directory default-directory 15) ;; ディレクトリ名 'lは左寄せ 'rは右寄せ
                                      face2 'l)
                                     (powerline-buffer-id face2 'r) ;; ファイル名
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face2 'l))
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face2 'l))
                                     )) ;; version control
                          
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(setq ns-use-srgb-colorspace nil)
(setq powerline-color1 "#073642")
(setq powerline-color2 "#002b36")

(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#3366cc"
                    :box nil)
(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "#FFD464"
                    :bold t)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)
(set-face-attribute 'powerline-active1 nil
                    :foreground "#333333"
                    :background "#FFD464"
                    :box nil)
(set-face-attribute 'powerline-inactive1 nil
                    :box nil)


(set-face-attribute 'powerline-active2 nil
                    :foreground "#fff"
                    :background "#333333"
                    :box nil)
(set-face-attribute 'powerline-inactive2 nil
                    :box nil)


(powerline-my-theme)

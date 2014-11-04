;; 一行コメントアウト
;; 参考: http://ganmacs.hatenablog.com/entry/2013/12/01/140306
(defun one-line-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (comment-or-uncomment-region (region-beginning) (region-end))))


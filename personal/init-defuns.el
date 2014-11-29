;;; init-defuns.el --- Custom Functions

;; global replace-regexp
(defun replace-regexp-g ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (call-interactively 'replace-regexp)))

;; Convert DOS newlines (\r\n) to Unix newlines (\n) (and the reverse).
;;
;; SOURCE:
;; https://github.com/redguardtoo/emacs.d/blob/7cbd20004ac7d231274df04165e4b424999165b8/lisp/init-misc.el#L350
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; show ascii table
;;
;; SOURCE: https://github.com/redguardtoo/emacs.d/blob/7cbd20004ac7d231274df04165e4b424999165b8/lisp/init-misc.el#L362
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
           (setq i (+ i 1))
           (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

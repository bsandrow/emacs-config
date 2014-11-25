;;; init-defuns.el --- Custom Functions

;; global replace-regexp
(defun replace-regexp-g ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (call-interactively 'replace-regexp)))

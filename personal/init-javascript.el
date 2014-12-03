;;; init-javascript.el --- Initialize JavaScript

(require-package 'js2-mode)

;; ;; TODO What is the point of this:
;; (autoload 'js2-mode "js2" nil t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)

;; TODO What does this do:
(setq js2-mode-toggle-warnings-and-errors t)

;; TODO Why would I want this?
;; (add-hook 'js2-mode '(lambda ()
;;                        (highlight-symbol-mode t)))

;;; init-javascript.el ends here

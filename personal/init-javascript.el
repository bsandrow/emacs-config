;;; init-javascript.el --- Initialize JavaScript


;; Initial Config
;; --------------
;; Note: Some of these must be set *before* js2-mode is loaded.
;;
(setq js-basic-indent 2)
(setq-default js2-basic-indent 2)
(setq-default js2-basic-offset 2)
(setq-default js2-auto-indent-p t)
(setq-default js2-cleanup-whitespace t)
(setq-default js2-enter-indents-newline t)
(setq-default js2-indent-on-enter-key t)
;(setq-default js2-mode-indent-ignore-first-tab t)

(setq-default js2-global-externs '("assert" "refute" "setTimeout"
                                   "clearTimeout" "setInterval"
                                   "clearInterval" "console" "JSON"
                                   "jQuery" "$" "angular" "Ember"))

;; Let flymake do the error-parsing
;(setq-default js2-show-parse-errors nil)

;; TODO What does this do:
(setq js2-mode-toggle-warnings-and-errors t)

;; Loading...
;; ----------
(require-package 'js2-mode)
;; (autoload 'js2-mode "js2" nil t)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Misc
;; ----
(font-lock-add-keywords 'js2-mode
                        `(("\\(function *\\)("
                           (0 (progn (compose-region (match-beginning 1) (match-end 1) "ƒ")
                                     nil)))))

(font-lock-add-keywords 'js2-mode
                        '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|XXX\\):"
                           1 font-lock-warning-face t)))

;;; init-javascript.el ends here

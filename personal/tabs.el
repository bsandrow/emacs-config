;;; tabs.el --- configuration of tabs

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) ; or any other preferred value
(setq indent-line-function 'insert-tab)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default py-indent-offset 4)

;;; tabs.el ends here

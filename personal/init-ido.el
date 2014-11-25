;;; init-ido.el --- Initialize ido-mode

(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(setq ido-enable-flex-matching t)
(ido-vertical-mode t)

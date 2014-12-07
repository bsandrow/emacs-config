;;; init-ibuffer.el -- Configure iBuffer mode

(require-package 'ibuffer)
(require-package 'ibuffer-vc)
(require 'ibuffer)
(require 'ibuffer-vc)

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;;; init-ibuffer.el ends here

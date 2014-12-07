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

;; Source: http://emacs-fu.blogspot.ca/2010/02/dealing-with-many-buffers-ibuffer.html
;;
;; (setq ibuffer-saved-filter-groups
;;       (quote (("default"
;;                ("Org" (mode . org-mode))
;;              ))
;;       )
;; )

;; Future:
;;    - Interface to switch between saved filter groups
;;      - Special-case for ibuffer-vc generated groups
;;      - Use ido completion
;;      - Default to ibuffer-vc (since it's auto-generated, it will
;;        probably apply in more cases).
;;    - Maybe a way to cycle through all of the saved filter groups (+
;;      ibuffer-vc). I'm less bullish on this than I am on the ido
;;      inferface.

;;; init-ibuffer.el ends here

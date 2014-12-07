;;; init-column-marker.el --- Initialize column-wrap indicator

;; column-marker mode
;; ==================
;; This doesn't work so well. It highlights the character that crosses
;; the boundary, doesn't show a vertical line indicating where the
;; boundary is on all lines. (This is what I want.)
;;
;; (require 'column-marker)
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (interactive)
;;             (column-marker-1 80)))

;; fill-column-indicator mode
;; ==========================
;; This does what I want. It displays a line where the fill-column
;; is. As per the display, it displays a thin line, I personally
;; perfer what Vim's python-mode does, which is to set the background
;; for the character position (rectangular block). Maybe I'll
;; configure this later. Just need to add hooks for the modes where I
;; want this to show up. Not rocket science... :)
;;
;;   (package-install "fill-column-indicator")
;;
(require 'fill-column-indicator)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)

;;; init-column-marker.el ends here

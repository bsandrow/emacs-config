;;; init-fonts.el --- Setup default fonts depending on OS

(when window-system
  (let ((font
	 (case window-system
	   (ns "Consolas-11") ; MacOSX Cocoa / NeXTStep
	   (otherwise "DejaVu Sans Mono-11"))))
    (set-face-attribute 'default nil :font font)))

;; -!- Old Code -!-
;;
;; (when (eq system-type 'darwin)
;;   ;; default font
;;   (set-face-attribute 'default nil :font "Consolas-11")
;; 
;;   ;; use specific font for Korean charset
;;   ;; if you want to use differnt font size for specific charset,
;;   ;; add :size POINT-SIZE in the font-spec
;;   ;(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
;;   )

;;; init-colors.el --- initialize color theme

;; Resources
;; ---------
;; - http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; - http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters
;; - http://www.emacswiki.org/emacs/?action=browse;oldid=ColorTheme;id=ColorAndCustomThemes
;; - https://github.com/sellout/emacs-color-theme-solarized/

(let ((paths '("vendor/color-theme-ports"
               "vendor/base16-themes")))
     (dolist (element paths)
       (add-to-list 'custom-theme-load-path
                    (file-name-as-directory (concat dotfiles-dir element)))))

;; (add-to-list 'custom-theme-load-path
;;              (file-name-as-directory (concat dotfiles-dir "vendor/color-theme-ports")))

;; (add-to-list 'custom-theme-load-path
;;              (file-name-as-directory (concat dotfiles-dir "vendor/base16-themes")))

(load-theme 'base16-flat-dark t)

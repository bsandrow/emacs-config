;;; init-colors.el --- initialize color theme

;; Resources
;; ---------
;; - http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; - http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters
;; - http://www.emacswiki.org/emacs/?action=browse;oldid=ColorTheme;id=ColorAndCustomThemes
;; - https://github.com/sellout/emacs-color-theme-solarized/

(defun my-add-theme-load-path (path)
  "Add a path to the custom-theme-load-path list."
  (add-to-list 'custom-theme-load-path (file-name-as-directory path)))

(defun my-add-vendor-theme (name)
  "Add a theme path under the vendor/ directory to custom-theme-load-path."
  (my-add-theme-load-path (concat dotfiles-dir "vendor/" name)))

(mapc 'my-add-vendor-theme
      '("color-theme-ports" "base16-themes"))

(load-theme 'base16-ocean-dark t)

;; Source: https://github.com/drewfrank/dotfiles/blob/master/.emacs
;; Source: http://www.emacswiki.org/Evil
;; Source: http://changelog.complete.org/archives/661-so-long-vim-im-returning-to-emacs
;; Source: http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/

;; TODO Get 'gc' 'gcu' working for comment/uncomment (maybe need to port vim-commentary=https://github.com/tpope/vim-commentary)
;; TODO Get 'TODO' 'XXX' 'FIXME' Highlighting in comments working
;; TODO ido-mode working with ':e <tab>' in evil-mode?
;; TODO visual selection of a bunch of lines, then sort them

;; The Basics
;; ----------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; Disable the menu
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; Disable the toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable the scrollbar
(global-linum-mode 1) ; line numbers on the left
(setq inhibit-startup-message t) ; no splash screen

(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))

;; TODO Figure out what's wrong with this. It's setup these way in the
;; github setup that I was based this off of, but my Emacs complains
;; that ~/.emacs.d is in the load path, and that this could cause
;; issues.
;(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'cl)
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; ---- Loading Packages ----

(defvar required-packages
  '(color-theme
    color-theme-solarized
    evil
    magit
    evil-surround
    evil-leader
    ido-vertical-mode)
  "A list of packages to ensure are installed at launch.")

(defun packages-installed-p ()
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  
  ; install missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; ---- END Loading Packages ----

(setq personal-dir (concat dotfiles-dir "personal"))
(add-to-list 'load-path personal-dir)
(mapc #'load (directory-files personal-dir nil ".*.el$"))

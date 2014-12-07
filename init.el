;; Source: https://github.com/drewfrank/dotfiles/blob/master/.emacs
;; Source: http://www.emacswiki.org/Evil
;; Source: http://changelog.complete.org/archives/661-so-long-vim-im-returning-to-emacs
;; Source: http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/

;; Vim Conversion:
;; ---------------
;; - Get gc/gcu working for dealing with comments...
;;   - Short term fix: bind 'gc' to 'comment-dwim'.
;;   - Long term fix: Move to Evil port of NERD-Commenter?

;; ---------
;; exec-path
;; ---------
(defun add-to-exec-path (new-path)
  "Add NEW-PATH to EXEC-PATH"
  (when (and (file-accessible-directory-p new-path)
             (not (member new-path exec-path)))
    (setenv "PATH" (concat (getenv "PATH") ":" new-path))
    (setq exec-path (append exec-path '(new-path)))))

(add-to-exec-path "/usr/local/bin")

;; ----------
;; The Basics
;; ----------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; Disable the menu
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; Disable the toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable the scrollbar
(setq inhibit-startup-message t) ; no splash screen
(setq initial-scratch-message nil)
(setq frame-title-format "%b - Emacs") ; set frame (window) title
(setq icon-title-format "%b - Emacs") ; set minimized title
(setq require-final-newline t) ; add a newline at end-of-file
(setq use-file-dialog nil) ; use minibuffer rather then file dialog
(defalias 'yes-or-no-p 'y-or-n-p) ; let the user type y or n instead of the full yes or no
(setq-default truncate-lines t) ; truncate, rather than wrap, lines by default
(savehist-mode 1) ; save minibuffer history

(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 174))

;; -- Encoding Settings --
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; -------------
;; "after" macro
;; -------------
;; A useful feature to deal with transition from `eval-after-load` to
;; new and improved `with-eval-after-load`
;;
;; Sources:
;; - http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
;; - https://github.com/juanjux/emacs-dotfiles/blob/master/init.el

(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;; ------------
;; Line Numbers
;; ------------
;; TODO Limit this to particular modes rather than everywhere
(global-linum-mode 1) ; line numbers on the left

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; TODO Figure out what's wrong with this. It's setup these way in the
;; github setup that I was based this off of, but my Emacs complains
;; that ~/.emacs.d is in the load path, and that this could cause
;; issues.
;(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/python-mode.el-6.2.0"))

;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))


;; --------------
;; Emacs Packages
;; --------------
(require 'cl)
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
        ;("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;(require-package 'vc-darcs) <= ibuffer-vc uses this for darcs
(require-package 'ibuffer)
(require-package 'ibuffer-vc)
(require-package 'magit)
(require-package 'ido-vertical-mode)
(require-package 'projectile)
(require-package 'flx)
(require-package 'flx-ido)
(require-package 'git-commit-mode)
(require-package 'git-rebase-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)
(require-package 'gitattributes-mode)

;; ---------------
;; Personal Config
;; ---------------
(setq personal-dir (concat dotfiles-dir "personal"))
(add-to-list 'load-path personal-dir)
(mapc #'load (directory-files personal-dir nil ".*.el$"))

;;; Animated Welcome Message
;; (defconst animate-n-steps 4)
;; (defun emacs-reloaded () "animated welcome message" (interactive)
;;   (animate-string (concat ";; Initialization successful, welcome to "
;;                        (substring (emacs-version) 0 16)
;;                        "."
;;                        "\n"
;;                        ";; Tip of the Day:\n;;   "
;;                        "\n"
;;                        ; (cookie "~/.emacs.d/tip-of-the-day.fortune" "s" "e")
;;                        "\n"
;;                        ";; type C-x C-e for more tips\n"
;;                        "(emacs-reloaded)"
;;                        "\n")
;;                0 0)
;;   (end-of-buffer) (newline-and-indent)
;;   ;; (newline-and-indent)  (newline-and-indent)
;; )
;; (add-hook 'after-init-hook (lambda ()
;;                              (maximize-frame)
;;                              (emacs-reloaded)))

(require-package 'ace-jump-mode)
(require 'ace-jump-mode)

;;; init-persistence.el --- Initialization of configuration for persistence between sessions

;; ---------------------------
;; Remember last edit position
;; ---------------------------
(require-package 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)


;; ------------------
;; minibuffer history
;; ------------------
(require-package 'savehist)
(setq savehist-file "~/.emacs.d/savehist"
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60)
(setq-default history-length 1000)
(savehist-mode +1)

;; ---------------
;; Backup Handling
;; ---------------
(setq backup-by-copying t) ; don't clobber symlinks
(setq backup-directory-alist '(("." . "~/.saves"))) ; don't litter my fs tree
(setq kept-new-versions 6)
(setq kept-old-version 2)
(setq version-control t) ; use versioned backups

;;; disable backup / auto-save
;; (setq backup-by-copying t)
;; (setq make-backup-files nil)
;; (setq auto-save-default nil)


;;; init-persistence.el ends here

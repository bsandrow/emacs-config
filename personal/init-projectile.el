;;; init-projectile.el --- Initialize projectile

(require-package 'projectile)
(require-package 'flx-ido)

(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-user-faces nil)
(projectile-global-mode)

(defun my-find-file ()
  "Open file using projectile or ido"
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

;;; init-projectile.el ends here

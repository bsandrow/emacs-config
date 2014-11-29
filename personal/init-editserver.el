;;; init-editserver.el --- Configure the Edit with Emacs server

(when (require 'edit-server nil t)
  (setq edit-server-new-frame t)
  (edit-server-start))

;;; init-editserver.el ends here

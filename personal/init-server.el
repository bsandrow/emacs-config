;;; init-server.el -- Start the Emacs server

(require 'warnings)

(when window-system
  (let ((warning-minimum-level :error))
    (server-start)))

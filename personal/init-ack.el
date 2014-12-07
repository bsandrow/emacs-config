;;; init-ack.el -- Configure ack

(require-package 'ack-and-a-half)

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(defalias 'ack-with-args 'ack-and-a-half-with-args)

;;; init-ack.el ends here

;;; init.el -- Initialize Emacs...

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  (require 'org-install)
  (require 'ob-tangle))

(org-babel-load-file (expand-file-name "emacs.org" dotfiles-dir))

(setq org-src-fontify-natively t)

;;; init.el ends here

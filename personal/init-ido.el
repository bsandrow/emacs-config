;;; init-ido.el --- Initialize ido-mode

(require 'ido)
(require 'ido-vertical-mode)
(require 'flx-ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(ido-vertical-mode t)

;; Fix ido-completion to allow me to use C-w instead of S-M-DEL to
;; delete backward by a word. It's better to use
;; ido-delete-backward-word-updir because it does what I want in this.
;; situation.
;;
(define-key ido-file-completion-map "\C-w" 'ido-delete-backward-word-updir)

;; Other Keymap Changes:
;;  C-j ido-select-text          => ido-next-match
;;  C-k ido-delete-file-at-head  => ido-prev-match
;;  C-l ido-reread-directory     => ido-select-text
;;  C-r ido-prev-match           => ido-reread-directory
;;  C-s ido-next-match           => nil

;; TODO Write my own ido-prev-match that deletes to end of input if
;; the cursor is not at the end of the user input (like the
;; delete-file-at-head does).

(define-key ido-file-completion-map "\C-j" 'ido-next-match)
(define-key ido-file-completion-map "\C-k" 'ido-prev-match)
(define-key ido-file-completion-map "\C-l" 'ido-select-text)
(define-key ido-file-completion-map "\C-r" 'ido-reread-directory)
(define-key ido-file-completion-map "\C-s" nil)

;;; init-ido.el ends here

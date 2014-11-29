;;; init-evil.el --- initialize evil-mode and friends...

(defun enable-evil-mode ()
  (require 'evil)
  (evil-mode 1)

  (require 'evil-surround)
  (global-evil-surround-mode 1)

  (defun wrap ()
    "Enable line wrapping"
    (interactive)
    (setq truncate-lines nil))

  (defun nowrap ()
    "Disable line wrapping"
    (interactive)
    (setq truncate-lines t))

  ; use C-6 to swap to a previous buffer
  (define-key evil-normal-state-map (kbd "C-6") 'evil-buffer)

  ;; make Esc quit everything
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

  ;; -!- Replace tpope's vim-commentary -!-
  ;;
  ;; TODO missing the 'gcu' binding to uncomment a region without a visual selection
  (defun evil-comment-dwim ()
    (interactive)
    "Like 'comment-dwim', but switches to Insert state when inserting a comment and not operating on a region."
    (unless (and mark-active transient-mark-mode)
      (unless (evil-insert-state-p)
	(evil-insert-state)))
    (call-interactively #'comment-dwim))
  (define-key evil-normal-state-map (kbd "gc") #'evil-comment-dwim)

  ; ending paren
  )

(defun disable-evil-mode ()
  (global-evil-surround-mode 0)
  (evil-mode 0))

(enable-evil-mode)

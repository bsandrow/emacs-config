;; init-evil.el --- initialize evil-mode and friends...

;; Resources
;; ---------
;; - https://gist.github.com/gcr/3962719
;; - https://lists.gnu.org/archive/html/emacs-orgmode/2012-02/msg01000.html
;; - https://github.com/mixandgo/emacs.d/blob/master/my-evil.el
;; - https://github.com/jubos/dotfiles/blob/master/emacs.d/config/curtis-evil.el
;; - http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/

;; Note: evil-leader-mode *MUST* be enabled before evil-mode
(require-package 'evil)
(require-package 'evil-leader)

(global-evil-leader-mode)
(evil-mode 1)

(require-package 'evil-surround)
(global-evil-surround-mode 1)

;; I'm used to using :sort all of the time in Vim, so let's alias
;; :sort to :sort-lines for convenience. Huzzah!
(evil-ex-define-cmd "sort" 'sort-lines)

;; Set up evil-mode <Leader>.
;; Use "," as the leader. "," is already mapped to something useful,
;; so let's remap that to ",," so as not to use it.
(evil-leader/set-leader ",")
(evil-leader/set-key "," 'evil-repeat-find-char-reverse)

;; I have this bound in Vim, so here it is to. mneumonic: Remove Whitespace
(evil-leader/set-key "rw" 'delete-trailing-whitespace)

;; My natural tendency for buffer switching is to hit ,be which I
;; have bound to BufferExplorer in Vim. The functionality of
;; switch-to-buffer isn't the same, but the general idea that I
;; automatically hit ,be when I want to switch a buffer remains.
;;
;; That said, switch-to-buffer (with ido-mode) is probably better
;; than BufferExplorer, though the ability to see a _complete_ list
;; of all buffers is missing.
;(evil-leader/set-key "be" 'switch-to-buffer)
(evil-leader/set-key "be" 'ibuffer)

;; Having a M-x binding that allows for some auto-completion is always
;; good. I can just use evil-ex-mode for the times when I don't care
;; about auto-completion.
(require 'helm-config)
(evil-leader/set-key "xm" 'helm-M-x)

;; Nothing emulates Vim's CtrlP plugin yet, but binding file-file to
;; C-p will help me with my muscle memory. I may just need to wrap
;; find-file with something that acts more CtrlP-like when I'm in a
;; repository, otherwise it will just do the regular find-file (with
;; ido-mode).
;(define-key evil-normal-state-map "\C-p" 'ido-find-file)
(define-key evil-normal-state-map "\C-p" 'my-find-file)

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

;; Misc <Leader> bindings to regular Emacs stuff
(evil-leader/set-key "d" 'dired-jump)
(evil-leader/set-key "k" 'ido-kill-buffer)

;; Eval Bindings
(evil-leader/set-key "ee" 'eval-last-sexp)
(evil-leader/set-key "er" 'eval-region)
(evil-leader/set-key "ef" 'eval-defun)

;; make Esc quit everything
;; source: https://github.com/davvil/.emacs.d/blob/64367f2/init.el#L19

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

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

(evil-set-initial-state 'ibuffer-mode 'normal)

;; iBuffer setup
(eval-after-load 'ibuffer
  '(progn
     ;; use the standard ibuffer bindings as a base
     (set-keymap-parent (evil-get-auxiliary-keymap ibuffer-mode-map 'normal t)
                        (assq-delete-all 'menu-bar (copy-keymap ibuffer-mode-map)))
     (evil-define-key 'normal ibuffer-mode-map "j" 'ibuffer-forward-line)
     (evil-define-key 'normal ibuffer-mode-map "k" 'ibuffer-backward-line)
     (evil-define-key 'normal ibuffer-mode-map "J" 'ibuffer-jump-to-buffer) ; "j"
     (evil-define-key 'normal ibuffer-mode-map "/" 'evil-search-forward)
     (evil-define-key 'normal ibuffer-mode-map "n" 'evil-search-next)
     (evil-define-key 'normal ibuffer-mode-map "N" 'evil-search-previous)
     (evil-define-key 'normal ibuffer-mode-map "?" 'evil-search-backward)
   ))

;; --------------
;; org-mode stuff
;; --------------

;; Note: I don't like these bindings, but I'll deal with them. My
;; preferred bindings would be:
;;
;;  zo => Open the fold at the current level. All sublevels of folds
;;        retain their state. The body counts as part of the current
;;        fold instead of this weird idea that show-children keeps
;;        the body hidden whole showing immediate sub-headings.
;;
;;  zO => Does what show-subtree does right now. Opens all folds
;;        from the current level downwards.
;;
;;  zC => Works like hide-subtree right now.
;;
;;  zc => Hide at the current level. All sub-levels retain their
;;        state (i.e. if I hit 'zo' to show the fold again, all
;;        sub-levels remember what expanded/collapsed state they are
;;        in.
;;
;; zR => Open all folds (e.g. show-all)
;;
;; zM => Close all folds (e.g. hide-all)
;;
;; zj => Move downwards to the next fold. (downwards in relation to
;;       the file, not the fold level)
;;
;; zk => Move upwards to the next fold. (upwards in relation to the
;;       file, not the fold level)

(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-open-at-point
  "za"        'org-cycle
  "zA"        'org-shifttab
  "zm"        'hide-body
  "zr"        'show-all
  "zo"        'show-subtree
  "zO"        'show-all
  "zc"        'hide-subtree
  "zC"        'hide-all
  )

(evil-define-key 'normal orgstruct-mode-map
  (kbd "RET") 'org-open-at-point
  "za"        'org-cycle
  "zA"        'org-shifttab
  "zm"        'hide-body
  "zr"        'show-all
  "zo"        'show-subtree
  "zO"        'show-all
  "zc"        'hide-subtree
  "zC"        'hide-all
  )

;; init-evil.el ends here

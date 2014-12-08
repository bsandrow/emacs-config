;;; init-pomodoro.el --- Configure pomodoro.el

;; == Configure ==
;;
(setq pomodoro-work-time 25)
(setq pomodoro-short-break 5)
(setq pomodoro-long-break 15)
(setq pomodoro-set-number 4)

;; == Initialize ==
;;
(require 'pomodoro)
(pomodoro-add-to-mode-line)

;; Sources
;; -------
;; https://github.com/rodw/.dotfiles/blob/master/emacs/.rods-dot-emacs.org#pomodoro
;; http://ivan.kanis.fr/pomodoro.el
;; https://github.com/baudtack/pomodoro.el/blob/master/pomodoro.el
;;
;;; init-pomodoro.el ends here

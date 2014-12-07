;;; init-scrolling.el --- configure mouse scrolling

;;; Sources:
;;; - http://www.emacswiki.org/emacs/SmoothScrolling

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; scroll one line at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll the window under the mouse
(setq scroll-step 1) ; keyboard scroll one line at a time
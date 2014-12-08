;;; init-frame.el --- Configure Frame Settings

;; OSX Native Fullscreen
;; =====================
;; Controls behaviour of `toggle-frame-fullscreen` on OSX.
;;
;; This controls whether or not to use the new 'native' fullscreen in
;; OSX that creates a separate workspace for the fullscreen'd
;; app. Setting to false disables use of this.
;;
;; source: http://crypt.codemancers.com/posts/2013-07-05-non-native-fullscreen-for-osx-on-emacs-24-dot-3/
;(setq ns-use-native-fullscreen nil)

(require 'maximize)

(defun maximize-toggle-frame-max ()
  "Maximize the window (horizontally and vertically).

Note: If one of the dimensions is already maxed, it will be toggled
      off instead of on. Would have to take a deeper look at the
      internals of the functions to check for that or not.
  (interactive)
  (maximize-toggle-frame-vmax)
  (maximize-toggle-frame-hmax))

;;; init-frame.el ends here

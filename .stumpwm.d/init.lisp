
;; Quicklisp - Load
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)


(defconstant +guix-system-path+ "/run/current-system/profile/share"
  "Define Guix System profile PATH")

(defconstant +guix-home-path+ "/home/davy/.guix-home/profile/share"
  "Define Guix Home profile PATH")

(setf *startup-message* nil)

;; Set up X11 Environment
(load "~/.stumpwm.d/modules/auto-start.lisp")

;; _ModeLine
(when *initializing*
  (mode-line))

(setf *mouse-focus-policy* :click
      *float-window-modifier* :SUPER)

(setf *startup-message*
      (concatenate 'string "^2*Welcome ^BDavy^b! "
		   "Your ^BStumpWM^b session is ready..."))

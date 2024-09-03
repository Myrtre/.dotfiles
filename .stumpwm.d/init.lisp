
;; Quicklisp - Load
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)

(declaim (optimize (speed 3) (safety 3)))

;; Define guix profiles
(defconstant +guix-system-path+ "/run/current-system/profile/share/"
  "Define Guix System profile PATH")
(defconstant +guix-home-path+ "/home/davy/.guix-home/profile/share/"
  "Define Guix Home profile PATH")
(defconstant +guix-profile+ "/home/davy/.guix-profile/share/"
  "Define Guix Profile PATH")

;; Set PATHs for guix-modules
(set-module-dir (concat +guix-system-path+
			"common-lisp/sbcl/"))

(setf *default-package* :stumpwm)
(setf *data-dir* (concat (getenv "HOME")
			 "~/.stumpwm.d/data/"))

(setf *startup-message* nil)

(setf *altgr-offset* 4)      ;; Set up AltGr key to work
(register-altgr-as-modifier)


;; Set up X11 Environment
(load "~/.stumpwm.d/modules/auto-start.lisp")

;; Load ./modules
;; [ bluetooth commands utilites frame keybindings theme modeline (auto-start) ]
;; - (stumpwm:add-to-load-path "~/.stumpwm.d/modules")

(load "~/.stumpwm.d/modules/colors.lisp")
(load "~/.stumpwm.d/modules/theme.lisp")
(load "~/.stumpwm.d/modules/frames.lisp")
(load "~/.stumpwm.d/modules/keybindings.lisp")
(load "~/.stumpwm.d/modules/modeline.lisp")

;; Start Mode-line
(when *initializing*
  (mode-line))

(setf *mouse-focus-policy*    :click
      *float-window-modifier* :SUPER)

(load-module "globalwindows")

;; Additional XOrg Resource + Runs
(run-shell-command "xrdb -merge ~/.Xresources")

(require :slynk)
(sb-thread:make-thread
 (lambda () (slynk:create-server :port 4005 :dont-close t)))


;; Welcome
(setf *startup-message*
      (concatenate 'string "^2Welcome ^BDavy^b! "
		   "Your ^BStumpWM^b session is ready."))

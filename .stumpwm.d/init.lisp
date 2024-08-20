
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

(setf *altgr-offset* 4)      ;; Set up AltGr key to work
(register-altgr-as-modifier)


;; Set up X11 Environment
(load "~/.stumpwm.d/modules/auto-start.lisp")

;; Load ./modules
;; [ bluetooth commands utilites placement keybindings theme modeline (auto-start) ]
(stumpwm:add-to-load-path "~/.stumpwm.d/modules")


(setf *mouse-focus-policy* :click
      *float-window-modifier* :SUPER)

;; Navigate between windows from all workspaces
(load-module "globalwindows")

;; Additional XOrg Resource + Runs
(run-shell-command "xrdb -merge ~/.Xresources")

(when *initializing*
  (mode-line))

(setf *startup-message*
      (concatenate 'string "^2*Welcome ^BDavy^b! "
		   "Your ^BStumpWM^b session is ready..."))

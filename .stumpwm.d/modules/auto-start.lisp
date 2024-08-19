;; auto-start.lisp --> Start X11 Environment for StumpWM

(in-package :stumpwm)

;; Cursor
;;(run-shell-command "xsetroot -xcf")

;; Turn offf systemm bell & screen-saver control
(run-shell-command "xset b off")
(run-shell-command "xset s off")


;; UI
;; Wallpaper?

;; Screen Compositor
(run-shell-command "picom")

;; Enable screen locking on suspenk
(run-shell-command "xss-lock -- slock")

(in-package :stumpwm)

(setf *mode-line-timeout* 2)

(setf *time-modeline-string* "%F %H:%M")

(setf *group-format* "%t")

(defvar *mode-line-formatter-list*
  '(("%g")  ;; Groups
    ("%W")  ;; Windows
    ("^>")  ;; StumpWM modeline separator
    ("%P")  ;; Audio info
    ("%d"))  ;; Date/Time
  "List of formatters for the modeline.")

(defun generate-mode-line-component (out-color in-color component #optional right-alignment)
  "Generate a component of given color, by deafault component is LeftAlligned. set right-alignent"
  (if right-alignment
      (list "^>" out-color "[" in-color component outcolor "]")
      (list out-color "[" in-color component out-color "]")))


(defparameter pipe " | ")
(defparameter bracket-color "^8")
(defparameter content-color "^7")

(defvar group-fmt "%g")
(defvar audio-fmt "%P")
(defvar time-fmt "%d")

(defun generate-mode-line ()
  "build a modeline"
  (setf *screen-mode-line-format*
	(list
	 (generate-mode-line-component bracket-color content-color group-fmt)
	 (generate-mode-line-component bracket-color content-color audio-fmt)
	 (generate-mode-line-component bracket-color content-color time-fmt))))

(generate-mode-line)

(setf *mode-line-background-color* "#282828"
      *mode-line-border-color* "#EBDBE2"
      *mode-line-border-width* 2
      *mode-line-pad-x* 1
      *mode-line-pad-y* 1)

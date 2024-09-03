(in-package :stumpwm)

(setf *mode-line-timeout* 2)
;; Time format
(setf *time-modeline-string* "%F %I:%M%p")

;; Indicate groupname
(setf *group-format* "%t")

;; The Window format should display first its window number, then title
;; Limited to 30 chars
(setf *window-format* " %n: %25t")


(setf *mode-line-background-color* gruvbox-dark00
      *mode-line-foreground-color* gruvbox-dark07)

(setf *mode-line-border-color* gruvbox-dark01
      *mode-line-border-width* 1)

(setf *mode-line-pad-x* 0
      *mode-line-pad-y* 1
      *mode-line-timeout* 1)

(load-module "cpu")
(load-module "mem")

;; Costum Module Settings
;; f0 = font-hermit, f1 = font-hetbrains-mono, f3 font awesome
(setf cpu::*cpu-modeline-fmt*          "%c %t"
      cpu::*cpu-usage-modeline-fmt*    "\~ ^[~A~0D%^]"
      mem::*mem-modeline-fmt*          "= %a%p"
      *hidden-window-color*            "^**"
      *mode-line-highlight-template*   "<~A>")

(defvar *mode-line-formatter-list*
  '(("%g")  ;; Group
    ("%W")  ;; Window
    ("^>")  ;; StumpWM modeline separator
    ("%M")  ;; Memory usage
    ("%d")) ;; Date/Time
  "List of formatters for the modeline")

(defun generate-modeline (elements &optional not-invertedp rightp)
  "Generate a modeline for StumpWM"
  (when elements
    (cons (format nil " ^[~A^]^(:bg \"~A\") "
		  (format nil "^(:fg \"~A\")^(:bg \"~A\")~A"
			  (if (xor not-invertedp rightp)
			      gruvbox-dark00 gruvbox-dark03)
			  (if (xor not-invertedp rightp)
			      gruvbox-dark03 gruvbox-dark00)
			  (if rightp "" ""))
		  (if not-invertedp gruvbox-dark03 gruvbox-dark00))
	  (let* ((current-element (first elements))
		 (formatter       (first current-element))
		 (commandp        (rest current-element)))
	    (cons (if commandp
		      `(:eval (run-shell-command ,formatter t))
		    (format nil "~A" formatter))
		  (generate-modeline (rest elements)
				     (not not-invertedp)
				     (if (string= "^>" (first (first elements)))
					 t rightp)))))))

(defcommand reload-modeline () ()
	    "Reload modeline."
	    (sb-thread:make-thread
	     (lambda ()
	       (setf *screen-mode-line-format*
		     (rest (generate-modeline *mode-line-formatter-list*))))))
(reload-modeline)



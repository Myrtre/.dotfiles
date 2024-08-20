(in-package :stumpwm)

(load-module "kbd-layouts")

(kdb-layouts:keyboard-layouts-list "hu")
(define-key *top-map*
    (kbd "s-k") "switch-keyboard-layout")


(which-key-mode)

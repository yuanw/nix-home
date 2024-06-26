(require 'hydra)
(require 'ace-window)
(require 'winner)

(defhydra my-window-movement ()
  "window movement"
    ("o" other-window "next")
    ("*" enlarge-window "h+" )
    ("@" shrink-window "h-" )
    ("$" enlarge-window-horizontally "w+" )
    ("^" shrink-window-horizontally "w-" )
    ("f" find-file-other-window "other file")

    ("d" delete-other-windows :color blue)
    ("j" ace-window "ace-window")
    ("v" (lambda ()
       (interactive)
       (split-window-right)
       (windmove-right)) "split right")
    ("s" (lambda ()
       (interactive)
       (split-window-below)
       (windmove-down)) "below")
    ("k" delete-window "delete")
    ("r" winner-redo "redo")
    ("u" winner-undo "undo")
    ("D" ace-delete-window "ace delete") ;; TODO not working
    ("m" ace-maximize-window "maximize" :color blue) ;; TODO not working
    ("q" nil "cancel"))

(winner-mode 1)
(global-set-key (kbd "C-c w") 'my-window-movement/body)
(provide 'wm)

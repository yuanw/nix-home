;;; my-meow.el --- My Meow configuration package  -*- lexical-binding: t; -*-
;; Thin wrapper package so that (require 'my-meow) succeeds.
;; Actual config lives in configs/meow.el and is loaded by hm--load-external.
(require 'meow)

(provide 'my-meow)
;;; my-meow.el ends here

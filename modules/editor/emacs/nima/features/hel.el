;;; hel.el --- Hel modal editing configuration -*- lexical-binding: t; -*-

(use-package hel
  :demand t
  :config
  (require 'hel)
  (hel-mode 1))

(use-package hel-leader
  :after (hel which-key)
  :demand t
  :config
  (require 'hel-leader))

(use-package hel-consult
  :no-require t
  :after (hel hel-leader consult embark-consult)
  :config
  (load-file (expand-file-name "hel-consult.el" (file-name-directory load-file-name))))

(provide 'nima-feature-hel)
;;; hel.el ends here

;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu
  :recipe (:files (:defaults "extensions/*.el")))
(when (modulep! +icons)
  (package! kind-icon))
(when (modulep! +orderless)
  (package! orderless))
(package! corfu-doc
  :recipe (:host github :repo "galeo/corfu-doc"))
(package! cape)
;; (package! cape-yasnippet
;;   :recipe (:host github :repo "elken/cape-yasnippet"))
(package! yasnippet-capf
  :recipe (:host github :repo "elken/yasnippet-capf"))
;; (package! cape-use-package
;;   :recipe (:host github :repo "elken/cape-use-package"))
(package! package-capf
  :recipe (:host github :repo "elken/package-capf"))
(when (modulep! :os tty)
  (package! popon
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon"))
  (package! corfu-terminal
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
  (package! corfu-doc-terminal
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")))

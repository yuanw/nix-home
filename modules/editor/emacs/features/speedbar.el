(use-package speedbar
  :ensure nil
  :if (> emacs-major-version 30)
  :commands (speedbar)
  :config
  (setq speedbar-prefer-window t)
  (setq speedbar-use-images nil)
  )

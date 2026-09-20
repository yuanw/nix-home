{
  elisp = ''
    (use-package ediff
      :config
      ;; don't use a separate Frame for the control panel
      (setq ediff-window-setup-function 'ediff-setup-windows-plain)
      ;; horizontal split is more readable
      (setq ediff-split-window-function 'split-window-horizontally)

      ;; restore window config upon quitting ediff
      (defvar ue-ediff-window-config nil "Window config before ediffing.")
      (add-hook 'ediff-before-setup-hook
                (lambda ()
                  (setq ue-ediff-window-config (current-window-configuration))))
      (dolist (hook '(ediff-suspend-hook ediff-quit-hook))
        (add-hook hook
                  (lambda ()
                    (when (window-configuration-p ue-ediff-window-config)
                      (set-window-configuration ue-ediff-window-config))))))
  '';
}

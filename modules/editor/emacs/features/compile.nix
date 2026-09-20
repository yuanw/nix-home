{
  elisp = ''
    (use-package compile
      :bind (("C-c c" . compile)
             ("M-O" . show-compilation))
      :hook ((compilation-filter . compilation-ansi-color-process-output))
      :preface
      (defun show-compilation ()
        (interactive)
        (let ((it
               (catch 'found
                 (dolist (buf (buffer-list))
                   (when (string-match "\\*compilation\\*" (buffer-name buf))
                     (throw 'found buf))))))
          (if it
              (display-buffer it)
            (call-interactively 'compile))))

      (defun compilation-ansi-color-process-output ()
        (ansi-color-process-output nil)
        (set (make-local-variable 'comint-last-output-start)
             (point-marker)))
      :config
      (setq compilation-always-kill t)
      (setq compilation-ask-about-save nil)
      (setq compilation-context-lines 10)
      (setq compilation-scroll-output 'first-error)
      (setq compilation-skip-threshold 2)
      (setq compilation-window-height 100)
      (with-eval-after-load 'compile
        (keymap-set compilation-mode-map "z" #'delete-window)))
  '';
}

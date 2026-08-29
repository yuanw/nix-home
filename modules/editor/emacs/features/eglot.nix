{
  lspStyle,
  pkgs,
  ...
}:

{
  enable = lspStyle == "eglot";

  epkgs = epkgs: [
    epkgs.eglot
  ];

  elisp = ''
    (use-package eglot
      :preface
      (defun my/eglot-enable-command-provider (orig-fn server)
        "Unconditionally add :executeCommandProvider to Eglot client capabilities."
        (let ((original-capabilities (funcall orig-fn server)))
          (plist-put original-capabilities
                     :executeCommandProvider '(:commands (:dynamicRegistration :json-false)))))
      (advice-add 'eglot-client-capabilities :around #'my/eglot-enable-command-provider)

      (defun my/eglot-execute-command (command)
        "Interactively execute a COMMAND supported by the current Eglot LSP server."
        (interactive
         (let* ((server (eglot-current-server))
                (caps (eglot--capabilities server))
                (provider (plist-get caps :executeCommandProvider))
                (commands (and provider (plist-get provider :commands))))
           (list (completing-read "LSP Command: "
                                  (or (cl-coerce commands 'list) '())
                                  nil nil))))
        (eglot-execute (eglot-current-server) (list :command command)))
      :config
      (setq eglot-autoshutdown t)
      ;; jdtls initialize on large Gradle monorepos exceeds the default 30s.
      (setq eglot-connect-timeout 180)
      (add-to-list 'eglot-server-programs
                   '((java-mode java-ts-mode) . ("jdtls"
                                                 "--jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar")))
      (when (executable-find "zls")
        (add-to-list 'eglot-server-programs '((zig-mode zig-ts-mode) . ("zls")))
        (add-hook 'zig-mode-hook #'eglot-ensure)
        (add-hook 'zig-ts-mode-hook #'eglot-ensure)))
  '';
}

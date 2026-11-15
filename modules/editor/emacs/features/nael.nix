{ lspStyle, ... }:

{
  # nael.el loads nael-lsp when lsp-mode loads, so it is only needed with the
  # lsp-style LSP client; with Eglot, nael-eglot ships in the nael package.
  epkgs = epkgs: [ epkgs.nael ] ++ (if lspStyle == "lsp-mode" then [ epkgs.nael-lsp ] else [ ]);

  # nael talks to the Lean server (`lake serve`, or `lean --server` for old
  # lake) through Eglot or lsp-mode, so `lean`/`lake` must be on `exec-path`
  # when the buffer loads.  In a project they come from the flake dev shell,
  # and features/direnv.el only refreshes the environment on an idle timer
  # after a window switch -- which can lose the race against LSP autostart, so
  # direnv is hooked before the LSP client.  For .lean files outside a nix
  # project, turn on `modules.dev.lean` to install a toolchain globally.
  elisp = ''
    (use-package nael
      :mode ("\\.lean\\'" . nael-mode)
      :preface
      ;; Nael has no evaluation command, and the standalone files under book/code
      ;; are in no Lake target, so `project-compile' (C-c C-c) never builds them.
      ;; Run the file through `lean' instead: its stdout carries the `#eval'
      ;; results, which the Lean server otherwise only sends as diagnostics.
      (defun my-nael-run-file ()
        "Save and run the visited file with `lean', showing its `#eval' output."
        (interactive)
        (unless buffer-file-name
          (user-error "Buffer is not visiting a Lean file"))
        (save-buffer)
        (compile (format "lake env lean %s" (shell-quote-argument buffer-file-name))))
      :init
      (add-hook 'nael-mode-hook
                (lambda ()
                  (when (fboundp 'direnv-update-directory-environment)
                    (direnv-update-directory-environment))))
      (add-hook 'nael-mode-hook #'${if lspStyle == "lsp-mode" then "lsp-deferred" else "eglot-ensure"})
      (add-hook 'nael-mode-hook #'abbrev-mode)
      :config
      (keymap-set nael-mode-map "C-c C-r" #'my-nael-run-file))
  '';
}

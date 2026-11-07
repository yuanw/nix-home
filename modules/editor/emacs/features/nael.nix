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
      :init
      (add-hook 'nael-mode-hook
                (lambda ()
                  (when (fboundp 'direnv-update-directory-environment)
                    (direnv-update-directory-environment))))
      (add-hook 'nael-mode-hook #'${if lspStyle == "lsp-mode" then "lsp-deferred" else "eglot-ensure"})
      (add-hook 'nael-mode-hook #'abbrev-mode))
  '';
}

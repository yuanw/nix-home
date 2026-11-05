{ ... }:

{
  epkgs = epkgs: [
    epkgs.lean4-mode
  ];

  # lean4-mode talks to the Lean server (`lake serve`, or `lean --server` for
  # old lake) through lsp-mode, so `lean`/`lake` must be on `exec-path` when
  # the buffer loads.  In a project they come from the flake dev shell, and
  # features/direnv.el only refreshes the environment on an idle timer after a
  # window switch -- which can lose the race against lsp autostart.  Adding to
  # `lean4-mode-hook` with APPEND nil puts direnv ahead of the default `lsp`
  # hook.  For .lean files outside a nix project, turn on `modules.dev.lean`
  # to install a toolchain globally.
  elisp = ''
    (use-package lean4-mode
      :mode ("\\.lean\\'" . lean4-mode)
      :init
      (add-hook 'lean4-mode-hook
                (lambda ()
                  (when (fboundp 'direnv-update-directory-environment)
                    (direnv-update-directory-environment)))))
  '';
}

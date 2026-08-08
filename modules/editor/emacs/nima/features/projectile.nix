{ homeDirectory, workspaceDirectory, ... }:

{
  epkgs = epkgs: [
    epkgs.projectile
  ];

  elisp = ''
    (use-package projectile
      :hook (after-init . projectile-mode)
      :config
      (setq projectile-cache-file (locate-user-emacs-file "projectile.cache"))
      (setq projectile-enable-caching t)
      (setq projectile-project-search-path
            '("${homeDirectory}/org/"
              ("${homeDirectory}/${workspaceDirectory}/" . 1)))
      (setq projectile-cleanup-known-projects t)
      (setq projectile-create-missing-test-files t)
      (setq projectile-file-exists-local-cache-expire 300)
      (setq projectile-remember-projects-between-sessions t)
      (setq projectile-auto-discover t))
  '';
}

{ homeDirectory, workspaceDirectory, ... }:

{
  elisp = ''
    (use-package project
      :config
      (setq project-switch-commands
            '((?f "Find file" project-find-file)
              (?r "Find regexp" project-find-regexp)
              (?d "Dired" project-dired)
              (?b "Buffer" project-switch-to-buffer)
              (?q "Query replace" project-query-replace-regexp)
              (?k "Kill buffers" project-kill-buffers)
              (?! "Shell command" project-shell-command)
              (?e "Eshell" project-eshell)))

      (defun my/project-remember-workspace-projects ()
        "Scan workspace directories and remember all projects found."
        (project-remember-projects-under "${homeDirectory}/org/" nil)
        (project-remember-projects-under "${homeDirectory}/${workspaceDirectory}/" t)
        (project-forget-zombie-projects))

      (with-eval-after-load 'magit
        (defun project-magit-status ()
          "Run magit-status in the current project's root."
          (interactive)
          (magit-status-setup-buffer (project-root (project-current t))))
        (add-to-list 'project-switch-commands '(?g "magit" project-magit-status) t)))
  '';
}

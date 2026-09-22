{
  epkgs = epkgs: [
    epkgs.herdr-el
  ];

  # herdr.el reads the running herdr server over its JSON API socket
  # ($HERDR_SOCKET_PATH, else ~/.config/herdr/herdr.sock); the server keeps
  # owning the PTYs, so nothing here needs herdr's own TUI to be up.  The
  # opt-in libraries are left alone on purpose: `herdr-review` wants the
  # external herdr-meat-review script, `herdr-sound` wants a sound file.
  # `C-c h' is taken by consult-history, hence the `C-c g' prefix, which stays
  # a bare prefix: this keymap dialect refuses a command on a key that also
  # starts a longer sequence.
  elisp = ''
    (use-package herdr-ui
      :commands (herdr-ui herdr-ui-quit herdr-spaces herdr-agents)
      :bind (("C-c g g" . herdr-ui)
             ("C-c g s" . herdr-spaces-visit)
             ("C-c g a" . herdr-agents-visit)
             ("C-c g q" . herdr-ui-quit)))
  '';
}

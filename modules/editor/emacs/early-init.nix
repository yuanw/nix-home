{
  monoFont,
  font,
  isDarwin ? false,
}:

let
  inherit (builtins) toJSON;
in
''
  ;; -*- lexical-binding: t; -*-

  (defvar my-mono-font ${toJSON monoFont}
    "Monospace font family selected from Nix.")
  (defvar my-font ${toJSON font}
    "Proportional font family selected from Nix.")

  ${
    if isDarwin then
      ''
        ;; `undecorated-round' keeps rounded corners via the local patch, but on
        ;; current NS Emacs/macOS it can still leave a visible titlebar area.
        ;; Also set built-in `undecorated' early so the initial NS frame is
        ;; borderless/titlebar-free.
        (dolist (parameter '((undecorated . t)))
          (add-to-list 'default-frame-alist parameter)
          (add-to-list 'initial-frame-alist parameter))
      ''
    else
      ""
  }

  ${builtins.readFile ./early-init.el}
''

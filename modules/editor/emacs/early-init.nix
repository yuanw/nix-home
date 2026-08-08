{
  monoFont,
  font,
}:

''
  (defvar my-mono-font ${builtins.toJSON monoFont}
    "Monospace font family selected from Nix.")
  (defvar my-font ${builtins.toJSON font}
    "Proportional font family selected from Nix.")

  ${builtins.readFile ./early-init.el}
''

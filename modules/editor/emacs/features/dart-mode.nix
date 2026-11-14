{ nixConfig, ... }:

# `nixConfig` is the whole home-manager/darwin config, handed in through
# `_module.args' in ../lib.nix (the special `config' arg is the feature's own).
let
  config = nixConfig;
in
{
  enable = config.modules.dev.dart.enable;

  epkgs = epkgs: [
    epkgs.dart-mode
  ];

  elisp = ''
    (use-package dart-mode
      :mode ("\\.dart\\'"))
  '';
}

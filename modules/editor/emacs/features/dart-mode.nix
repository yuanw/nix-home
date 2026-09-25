{ nixConfig, ... }:

# `nixConfig` is the whole home-manager/darwin config handed in via nima's
# `_module.args` (the special `config` arg is nima's own, so we alias it).
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

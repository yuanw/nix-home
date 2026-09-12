{
  config,
  inputs,
  inputs',
  lib,
  ...
}:

with lib;
let
  cfg = config.modules.neru;
in
{
  imports = [ inputs.neru.darwinModules.default ];

  options.modules.neru = {
    enable = mkEnableOption "neru";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.modules.mouseless.enable;
        message = "modules.neru and modules.mouseless cannot both be enabled on the same host";
      }
    ];

    services.neru = {
      enable = true;
      package = inputs'.neru.packages.default;
    };
  };
}

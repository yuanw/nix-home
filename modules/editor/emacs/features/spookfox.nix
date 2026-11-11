{ lib, nixConfig, ... }:

# Drive the browser from Emacs over the Spookfox add-on.  Gated on the add-on
# being installed in LibreWolf (modules/browsers/librewolf.nix), so Emacs does
# not grow a localhost websocket server on hosts without a browser.
let
  enable = lib.attrByPath [ "modules" "browsers" "librewolf" "spookfox" "enable" ] false nixConfig;
in
{
  inherit enable;

  epkgs = epkgs: [
    epkgs.spookfox
    epkgs.websocket
  ];

  elispFile = ./spookfox.el;
}

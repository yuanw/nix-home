{ melpaBuild, haskellPackages }:
let
  ask = haskellPackages.ask;
in
melpaBuild {
  pname = "ask-mode";
  inherit (ask) src version;
  patches = [ ./ask-mode.patch ];

  files = ''("emacs/*.el")'';

  meta = {
    inherit (ask.meta) homepage license;
    description = "Ask-mode for Emacs extracted from Ask package";
  };
}

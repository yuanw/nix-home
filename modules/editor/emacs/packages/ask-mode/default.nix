{ melpaBuild, haskellPackages }:
let
  Ask = haskellPackages.ask;
in
melpaBuild {
  pname = "ask-mode";
  inherit (Ask) src version;
  patches = [ ./ask-mode.patch ];

  files = ''("emacs/*.el")'';

  meta = {
    inherit (Ask.meta) homepage license;
    description = "Ask-mode for Emacs extracted from Ask package";
  };
}

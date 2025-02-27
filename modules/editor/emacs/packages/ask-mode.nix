{ melpaBuild, haskellPackages }:
let
  Ask = haskellPackages.ask;
in
melpaBuild {
  pname = "ask";
  inherit (Ask) src version;

  files = ''("emacs/*.el")'';

  meta = {
    inherit (Ask.meta) homepage license;
    description = "Ask-mode for Emacs extracted from Ask package";
  };
}

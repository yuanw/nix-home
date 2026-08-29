{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "acp";
  version = "0.14.2-unstable-2026-08-22";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "4d7d58dc39870e9390e94617e13d7ada175d7945";
    sha256 = "sha256-/wG4MSTaYBBHqFWIaOVq94apnro2sIeWOfbWwCcDvBU=";
  };

  packageRequires = [ ];

  recipe = writeText "recipe" ''
    (acp
     :repo "xenodium/acp.el"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "An ACP (Agent Client Protocol) implementation in Emacs lisp";
    homepage = "https://github.com/xenodium/acp.el";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}

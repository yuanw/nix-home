{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  pcre2el,
  dash,
  avy,
  ultra-scroll,
  ...
}:

let
  version = "new_undo_system-unstable-2026-11-02";
  rev = "005dbdf7802737a5e064a97de3b45dfd191d6ba2";
in

melpaBuild {
  pname = "hel";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel";
    inherit rev;
    sha256 = "sha256-GC6eCxtiVYG3qDO0bR4N4CZS/1QKrI7iEfBca3tx/og=";
  };

  packageRequires = [
    dash
    pcre2el
    avy
    ultra-scroll
  ];

  recipe = writeText "recipe" ''
    (hel
     :repo "helheim-emacs/hel"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Helix emulation layer for Emacs";
    homepage = "https://github.com/helheim-emacs/hel";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}

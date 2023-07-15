{ lib,  pkgs }:
let
stdenv = pkgs.clangStdenv;
  # https://stackoverflow.com/questions/51161225/how-can-i-make-macos-frameworks-available-to-clang-in-a-nix-environment
  frameworks = pkgs.darwin.apple_sdk.frameworks;
in
stdenv.mkDerivation rec {
  pname = "cpu-helper";
  version = "0.0.1";

  src = ./helper;

  # buildInputs = [  ];
  # https://github.com/Homebrew/homebrew-core/blob/2f089a6f55a7496248be39f4d883094fd79cbdc4/Formula/ical-buddy.rb#L24
  buildPhase = ''
    make
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp helper $out/bin/sketchybar-cpu-helper
  '';

  meta = with lib; {
    description = "Get events and tasks from the macOS calendar database";
    homepage = "https://hasseg.org/icalBuddy/";
    platforms = platforms.darwin;
    license = licenses.mit;
  };
}

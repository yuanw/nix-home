{
  lib,
  stdenv,
  fetchFromGitHub,
  pkgs,
}:
# https://github.com/Homebrew/homebrew-core/blob/a187304f3f6471f13717893439a39ab152a44a5d/Formula/i/ical-buddy.rb
let

  # https://stackoverflow.com/questions/51161225/how-can-i-make-macos-frameworks-available-to-clang-in-a-nix-environment
  frameworks = pkgs.darwin.apple_sdk.frameworks;
in
stdenv.mkDerivation rec {
  pname = "icallBuddy";
  version = "1.10.1";

  src = fetchFromGitHub {
    owner = "dkaluta";
    repo = "icalBuddy64";
    rev = "v${version}";
    hash = "sha256-ID3U7lAve3DHTHx2kCunwg1LWkIEIIpQrXkvmkvn/Mg=";
  };

  buildInputs = [
    pkgs.perl
    frameworks.AppKit
    frameworks.Carbon
    frameworks.Cocoa
    frameworks.AddressBook
    frameworks.CalendarStore
  ];
  # https://github.com/Homebrew/homebrew-core/blob/2f089a6f55a7496248be39f4d883094fd79cbdc4/Formula/ical-buddy.rb#L24
  buildPhase = ''
    # Allow native builds rather than only x86_64
    sed -i 's/-arch x86_64//g' Makefile
    make  icalBuddy icalBuddy.1 icalBuddyLocalization.1 icalBuddyConfig.1
  '';
  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share/man/man1/
    cp icalBuddy $out/bin/icalBuddy
    mv *.1 $out/share/man/man1/

  '';

  meta = with lib; {
    description = "Get events and tasks from the macOS calendar database";
    homepage = "https://hasseg.org/icalBuddy/";
    platforms = platforms.darwin;
    license = licenses.mit;
  };
}

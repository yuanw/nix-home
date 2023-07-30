{ lib, pkgs, ... }:
# https://github.com/NixOS/nixpkgs/blob/e11142026e2cef35ea52c9205703823df225c947/pkgs/development/tools/language-servers/jdt-language-server/default.nix#L96
#https://github.com/Homebrew/homebrew-core/blob/51e9cd9bd5a35a32f41574df95d43df853df57b5/Formula/jdtls.rb
pkgs.stdenv.mkDerivation {

  pname = "jdt-language-server";
  version = "1.25.0";
  timestamp = "202303161431";
  src = pkgs.fetchurl {
    url = https://download.eclipse.org/jdtls/milestones/1.25.0/jdt-language-server-1.25.0-202306291518.tar.gz;
    sha256 = "";
  };

  nativeBuildInputs = with pkgs; [ makeWrapper ];

  buildPhase = ''
    mkdir -p jdt-language-server
    tar xfz $src -C jdt-language-server
  '';

  installPhase = ''
    mkdir -p $out/bin $out/libexec
    cp -a jdt-language-server $out/libexec
    makeWrapper $out/libexec/jdt-language-server/bin/jdtls $out/bin/jdtls --prefix PATH : ${lib.makeBinPath [ pkgs.python3 ]}
  '';

  dontUnpack = true;
  dontPatch = true;
  dontConfigure = true;
}

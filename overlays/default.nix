final: prev:
# stolen from https://github.com/midchildan/dotfiles/blob/master/overlays/nixpkgs.nix
# FIXME: workaround for wait util it ready for nixpkgs-unstable
# https://nixpk.gs/pr-tracker.html?pr=147289
# prev.lib.optionalAttrs prev.stdenv.isDarwin {
#   clang-tools =
#     prev.clang-tools.override { llvmPackages = prev.llvmPackages_12; };
# } //

{
  installApplication = { name, appname ? name, version, src, description
    , homepage, postInstall ? "", sourceRoot ? ".", ... }:

    with prev;
    stdenv.mkDerivation {
      name = "${name}-${version}";
      version = "${version}";
      inherit src;
      buildInputs = [ undmg unzip ];
      inherit sourceRoot;
      phases = [ "unpackPhase" "installPhase" ];
      installPhase = ''
        mkdir -p "$out/Applications/${appname}.app"
        cp -pR * "$out/Applications/${appname}.app"
      '' + postInstall;
      meta = with lib; {
        inherit description;
        inherit homepage;
        maintainers = [ ];
        platforms = platforms.darwin;
      };
    };

  # https://github.com/montchr/dotfield/blob/main/pkgs/darwin/yabai.nix
  yabai = prev.callPackage ./yabai.nix { };
  alerter = prev.callPackage ./alerter.nix { };
  dart = prev.callPackage ./dart.nix { };
  hosts = prev.callPackage ./hosts.nix { };
  emacsPlusNativeComp = prev.callPackage ./emacs-plus.nix { };
  pragmata-pro = prev.callPackage ./pragmata-pro-font.nix { };

  juliaMac = final.installApplication rec {
    name = "Julia";
    version = "1.7.1";
    sourceRoot = "Julia-1.7.app";
    src = prev.fetchurl {
      url =
        "https://julialang-s3.julialang.org/bin/mac/x64/1.7/julia-1.7.1-mac64.dmg";
      sha256 = "156lcayi6k51ch6wxvw1q9nciy6y4zv51qmxrybz7knnh8kjz14m";
    };
    description = "High Performance";
    homepage = "https://julialang.org/";
    postInstall = ''
      mkdir -p $out/bin
      for file in $out/Applications/Julia.app/Contents/Resources/julia/bin/julia
      do
        ln -s $file $out/bin/julia
        chmod +x $file
      done
    '';
  };
}

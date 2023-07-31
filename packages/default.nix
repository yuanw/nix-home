{ inputs, ... }: {
  #    nixpkgs = {
  #   config = {
  #     allowBroken = true;
  #     allowUnsupportedSystem = true;
  #     allowUnfree = true;
  #   };
  #   overlays = [
  #       inputs.emacs.overlay
  #       inputs.nur.overlay
  #       inputs.agenix.overlays.default

  #       (import ../hs-land/overlay.nix)
  #       (
  #         _final: prev: {
  #   # alpacasay = prev.callPackage ./alpacasay { };
  #   # myvim = prev.callPackage ./myvim { };
  #   stable = inputs.nixpkgs-stable.legacyPackages.${prev.system};
  #   mesa = inputs.nixpkgs-stable.legacyPackages.${prev.system}.mesa;
  #   yabai = prev.callPackage ./yabai.nix { };
  #   alerter = prev.callPackage ./alerter { };
  #   dart = prev.callPackage ./dart.nix { };
  #   hosts = prev.callPackage ./hosts.nix { };
  #   emacsPlusNativeComp = prev.callPackage ./emacs-plus.nix { };
  #   sketchybar-app-font = prev.callPackage ./sketchybar-app-font.nix { };
  #   sf-symbols = prev.callPackage ./sf_symbols.nix { };
  #   font-hack-nerd-font = prev.callPackage ./font-hack-nerd-font.nix { };
  #   ical-buddy = prev.callPackage ./ical-buddy.nix { };
  #   sketchybar-cpu-helper = prev.callPackage ./sketchybar-cpu-helper { };

  #         })

  #     ];
  # };

  perSystem = { system, pkgs, ... }: {
    packages = {
      # re-export our packages
      inherit (pkgs)
        emacsPlusNativeComp;
    };

    # make pkgs available to all `perSystem` functions
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config = {
        allowUnfree = true;
      };
      overlays = [
        # self.overlays.default
        inputs.emacs.overlay
        inputs.nur.overlay
        inputs.agenix.overlays.default

        (import ../hs-land/overlay.nix)
        (
          _final: prev: {
            # alpacasay = prev.callPackage ./alpacasay { };
            # myvim = prev.callPackage ./myvim { };
            stable = inputs.nixpkgs-stable.legacyPackages.${prev.system};
            mesa = inputs.nixpkgs-stable.legacyPackages.${prev.system}.mesa;
            yabai = prev.callPackage ./yabai.nix { };
            alerter = prev.callPackage ./alerter { };
            dart = prev.callPackage ./dart.nix { };
            hosts = prev.callPackage ./hosts.nix { };
            emacsPlusNativeComp = prev.callPackage ./emacs-plus.nix { };
            sketchybar-app-font = prev.callPackage ./sketchybar-app-font.nix { };
            sf-symbols = prev.callPackage ./sf_symbols.nix { };
            font-hack-nerd-font = prev.callPackage ./font-hack-nerd-font.nix { };
            ical-buddy = prev.callPackage ./ical-buddy.nix { };
            sketchybar-cpu-helper = prev.callPackage ./sketchybar-cpu-helper { };

          }
        )
      ];
    };
  };

  flake.overlays.default = _final: prev: {
    # alpacasay = prev.callPackage ./alpacasay { };
    # myvim = prev.callPackage ./myvim { };
    stable = inputs.nixpkgs-stable.legacyPackages.${prev.system};
    mesa = inputs.nixpkgs-stable.legacyPackages.${prev.system}.mesa;
    yabai = prev.callPackage ./yabai.nix { };
    alerter = prev.callPackage ./alerter { };
    dart = prev.callPackage ./dart.nix { };
    hosts = prev.callPackage ./hosts.nix { };
    emacsPlusNativeComp = prev.callPackage ./emacs-plus.nix { };
    sketchybar-app-font = prev.callPackage ./sketchybar-app-font.nix { };
    sf-symbols = prev.callPackage ./sf_symbols.nix { };
    font-hack-nerd-font = prev.callPackage ./font-hack-nerd-font.nix { };
    ical-buddy = prev.callPackage ./ical-buddy.nix { };
    sketchybar-cpu-helper = prev.callPackage ./sketchybar-cpu-helper { };

  };
}

# juliaMac = final.installApplication rec {
#   name = "Julia";
#   version = "1.7.1";
#   sourceRoot = "Julia-1.7.app";
#   src = prev.fetchurl {
#     url =
#       "https://julialang-s3.julialang.org/bin/mac/x64/1.7/julia-1.7.1-mac64.dmg";
#     sha256 = "156lcayi6k51ch6wxvw1q9nciy6y4zv51qmxrybz7knnh8kjz14m";
#   };
#   description = "High Performance";
#   homepage = "https://julialang.org/";
#   postInstall = ''
#     mkdir -p $out/bin
#     for file in $out/Applications/Julia.app/Contents/Resources/julia/bin/julia
#     do
#       ln -s $file $out/bin/julia
#       chmod +x $file
#     done
#   '';
# };
# }

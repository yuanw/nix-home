{ self, inputs, withSystem, ... }:
{
  flake = {
    nixosConfigurations = {
      aws = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { nixpkgs.overlays = [ inputs.agenix.overlays.default ]; }
          inputs.agenix.nixosModules.age
          ../modules/aws.nix
          ../modules/agenix.nix
        ];
      };

      adguard = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { nixpkgs.overlays = [ inputs.agenix.overlays.default ]; }
          inputs.agenix.nixosModules.age
          ../modules/aws.nix
          ../modules/adguradhome-with-user.nix
          ../modules/adguard.nix
          ../modules/agenix.nix
        ];
      };

      asche =
        withSystem "x86_64-linux" ({ config, inputs', system, ... }:
          inputs.nixpkgs.lib.nixosSystem {
            specialArgs = {
              isDarwin = false;
              isNixOS = true;
              packages = config.packages;
              nurNoPkg = import inputs.nur {
                nurpkgs = import inputs.nixpkgs { system = system; };
              };
              inherit inputs inputs';
            };

            modules = [
              {
                nixpkgs.hostPlatform = system;
              }

              ./asche
            ];
          });
    };
    darwinConfigurations = {
      ci = withSystem "aarch64-darwin" ({ config, inputs', system, ... }:
        inputs.nix-darwin.lib.darwinSystem {
          specialArgs = {
            isDarwin = true;
            isNixOS = false;
            nurNoPkg = import inputs.nur {
              nurpkgs = import inputs.nixpkgs { system = system; };
            };
            packages = config.packages;
            inherit inputs inputs';
          };
          modules = [
            {
              nixpkgs.hostPlatform = system;
            }
            ./yuan-mac.nix
          ];
        });
      yuanw =
        withSystem "x86_64-darwin" ({ config, inputs', system, ... }:
          inputs.nix-darwin.lib.darwinSystem {
            specialArgs = {
              isDarwin = true;
              isNixOS = false;
              nurNoPkg = import inputs.nur {
                nurpkgs = import inputs.nixpkgs { system = system; };
              };
              packages = config.packages;
              inherit inputs inputs';
            };

            modules = [
              {
                nixpkgs.hostPlatform = system;
              }
              ./yuan-mac.nix
            ];
          });
      WK01174 = withSystem "aarch64-darwin" ({ config, inputs', system, ... }:
        inputs.nix-darwin.lib.darwinSystem {
          specialArgs = {
            isDarwin = true;
            isNixOS = false;
            nurNoPkg = import inputs.nur {
              nurpkgs = import inputs.nixpkgs { system = system; };
            };
            packages = config.packages;
            inherit inputs inputs';
          };
          modules = [
            {
              nixpkgs.hostPlatform = system;
            }
            ./wk01174.nix
          ];
        });
    };
  };
  perSystem = { system, ... }: {
    packages.asche = self.nixosConfigurations.asche.config.system.build.toplevel;
    packages.yuanw = self.darwinConfigurations.yuanw.system;
    packages.ci = self.darwinConfigurations.ci.system;
    packages.wk01174 = self.darwinConfigurations.WK01174.system;
    # packages.activate = pkgs.writeShellApplication
    #   {
    #     name = "activate";
    #     text =
    #       # TODO: Replace with deploy-rs or (new) nixinate
    #       if system == "aarch64-darwin" || system == "x86_64-darwin" then
    #         let
    #           # This is used just to pull out the `darwin-rebuild` script.
    #           # See also: https://github.com/LnL7/nix-darwin/issues/613
    #           emptyConfiguration = darwinSystem { nixpkgs.hostPlatform = system; };
    #         in
    #         ''
    #           HOSTNAME=$(hostname -s)
    #           set -x
    #           ${emptyConfiguration.system}/sw/bin/darwin-rebuild \
    #             switch \
    #             --flake .#"''${HOSTNAME}" \
    #             "$@"
    #           doom sync
    #         ''
    #       else
    #         ''
    #           HOSTNAME=$(hostname -s)
    #           set -x
    #           ${lib.getExe pkgs.nixos-rebuild} \
    #             switch \
    #             --flake .#"''${HOSTNAME}" \
    #             --use-remote-sudo \
    #             "$@"
    #         '';

    #   };

  };

}

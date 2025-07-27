{
  self,
  inputs,
  withSystem,
  ...
}:
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

      # adguard = inputs.nixpkgs.lib.nixosSystem {
      #   system = "x86_64-linux";
      #   modules = [
      #     { nixpkgs.overlays = [ inputs.agenix.overlays.default ]; }
      #     inputs.agenix.nixosModules.age
      #     ../modules/aws.nix
      #     ../modules/adguradhome-with-user.nix
      #     ../modules/adguard.nix
      #     ../modules/agenix.nix
      #   ];
      # };

      asche = withSystem "x86_64-linux" (
        {
          config,
          inputs',
          system,
          ...
        }:
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
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                sharedModules = [
                  inputs.betterfox.homeManagerModules.betterfox
                  inputs.catppuccin.homeModules.catppuccin
                ];
                # users.johnw = import ./config/home.nix;
                backupFileExtension = "hm-bak";
                extraSpecialArgs = { inherit inputs; };
              };
            }
            ./asche
          ];
        }
      );
      misfit = withSystem "x86_64-linux" (
        {
          config,
          inputs',
          system,
          ...
        }:
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
            ./misfit
          ];
        }
      );
    };
    darwinConfigurations =
      let
        configure =
          hostname: sys: loadPrivate: addtionsModule:
          withSystem sys (
            {
              config,
              inputs',
              system,
              ...
            }:
            inputs.nix-darwin.lib.darwinSystem {
              specialArgs = {
                isDarwin = true;
                isNixOS = false;
                loadPrivate = loadPrivate;
                nurNoPkg = import inputs.nur {
                  nurpkgs = import inputs.nixpkgs { system = system; };
                };
                packages = config.packages;
                inherit hostname inputs inputs';
              };
              modules = [
                {
                  nixpkgs.hostPlatform = system;
                }
                inputs.home-manager.darwinModules.home-manager
                {
                  home-manager = {
                    useGlobalPkgs = true;
                    useUserPackages = true;
                    sharedModules = [
                      inputs.betterfox.homeManagerModules.betterfox
                      inputs.catppuccin.homeModules.catppuccin
                    ];

                    backupFileExtension = "hm-bak";
                    extraSpecialArgs = { inherit inputs; };
                  };
                }
                addtionsModule
              ];
            }
          );
      in
      {
        ci = configure "ci" "aarch64-darwin" false ./yuan-mac.nix;
        yuanw = configure "yuanw" "x86_64-darwin" false ./yuan-mac.nix;
        mist = configure "mist" "aarch64-darwin" true ./mist.nix;
        WK01174 = configure "WK01174" "aarch64-darwin" true ./wk01174.nix;
      };

    perSystem =
      { system, ... }:
      {
        packages.asche = self.nixosConfigurations.asche.config.system.build.toplevel;
        packages.misfit = self.nixosConfigurations.misfit.config.system.build.toplevel;
        packages.yuanw = self.darwinConfigurations.yuanw.system;
        packages.ci = self.darwinConfigurations.ci.system;
        packages.wk01174 = self.darwinConfigurations.WK01174.system;
        packages.mist = self.darwinConfigurations.mist.system;
      };
  };
}

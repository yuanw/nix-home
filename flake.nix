{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    mac-emacs.url = "github:cmacrae/emacs";
    spacebar.url = "github:cmacrae/spacebar";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    resource-id.url = "github:yuanwang-wf/resource-id";
    ws-access-token.url = "github:yuanwang-wf/ws-access-token";
    spacebar.inputs.nixpkgs.follows = "nixpkgs";

    nix-script = {
      url = "github:BrianHicks/nix-script";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, darwin, home-manager, nur, emacs, spacebar
    , mac-emacs, resource-id, ws-access-token, devshell, flake-utils, nix-script
    , ... }:
    let
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      # copied from https://github.com/cmacrae/config
      mailAddr = name: domain: "${name}@${domain}";
      # idea borrowed from https://github.com/hardselius/dotfiles
      mkDarwinSystem = { localConfig, modules }:
        darwin.lib.darwinSystem {
          inputs = inputs;
          system = "x86_64-darwin";
          modules = modules ++ [
            ({ lib, ... }: {
              _module.args.localConfig = localConfig;
              imports = import ./modules/modules.nix {
                inherit lib;
                isDarwin = true;
              };
            })
            home-manager.darwinModules.home-manager
            ./macintosh.nix
          ];
        };
      mkNixSystem = { localConfig, modules }:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = modules ++ [
            ./nixos_system.nix
            home-manager.nixosModules.home-manager
            {
              nixpkgs.overlays = [
                (se: su: { myInputs = inputs; })
                emacs.overlay
                nur.overlay
                (import ./overlays)
              ];
            }
            ({ lib, pkgs, ... }: {
              _module.args.localConfig = localConfig;
              imports = import ./modules/modules.nix { inherit lib; };
            })
          ];
        };
    in {
      nixosConfigurations.asche = mkNixSystem {
        localConfig = {
          username = "yuanwang";
          name = "Yuan Wang";
          email = mailAddr "me" "yuanwang.ca";
          hostname = "asche";
          gpgKey = "BF2ADAA2A98F45E7";
          homeDirectory = "/home/yuanwang";
        };
        modules = [ ./hosts/asche.nix ];
      };

      darwinConfigurations = {
        yuan-mac = mkDarwinSystem {
          localConfig = {
            username = "yuanwang";
            name = "Yuan Wang";
            email = mailAddr "me" "yuanwang.ca";
            hostname = "yuan-mac";
            gpgKey = "BF2ADAA2A98F45E7";
            homeDirectory = "/Users/yuanwang";
          };
          modules = [ ./hosts/yuan-mac.nix ];
        };

        wf17084 = mkDarwinSystem {
          localConfig = {
            username = "yuanwang";
            name = "Yuan Wang";
            email = mailAddr "yuan.wang" "workiva.com";
            hostname = "wf17084";
            gpgKey = "19AD3F6B1A5BF3BF";
            homeDirectory = "/Users/yuanwang";
          };
          modules = [ ./hosts/wf17084.nix ];
        };
      };
      yuan-mac = self.darwinConfigurations.yuan-mac.system;
      wf17084 = self.darwinConfigurations.wf17084.system;

    } // eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlay nix-script.overlay ];
        };

        # use nix-script-haskell to write executable helper
        #implemement dep packages, and check maybe
        mkHsScript = name: text:
          pkgs.writeTextFile {
            inherit name;
            executable = true;
            destination = "/bin/${name}";
            text = ''
              #!/usr/bin/env nix-script-haskell
              #!haskellPackages turtle
              ${text}
            '';
          };

        myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle
          (p: with p; [ cabal-install ormolu hlint hpack brittany turtle ]));

      in {
        devShell = pkgs.devshell.mkShell {
          name = "nix-home";
          imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
          git.hooks.enable = true;
          git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
          packages = [
            myHaskellEnv
            (mkHsScript "home" (builtins.readFile ./bin/home.hs))
            pkgs.haskellPackages.hnix
            pkgs.treefmt
            pkgs.nixfmt
            pkgs.nix-script
            pkgs.nix-script-haskell
            pkgs.nix-script-bash
          ];
        };
      });
}

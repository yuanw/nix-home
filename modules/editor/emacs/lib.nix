# Emacs through the nix-emacs-home library.
#
# The library is a Home Manager module, so everything below lands in the user's
# Home Manager configuration: the features in ./features are read there, and the
# values they need from this config are handed to them through `_module.args'.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.editors.emacs;

  # The packages this config keeps outside nixpkgs.  Long term each of them
  # belongs in the `overlay' of the feature that uses it.
  #
  # Emacs itself is patched in that overlay, not here: nix-emacs-home wraps
  # `cfg.package.pkgs', and `overrideAttrs' keeps the old `passthru', so a patch
  # added to `cfg.pkg' never reaches `withPackages' -- it still wraps the
  # unpatched Emacs the stale package set points at.
  emacsOverlay = import ./overrides.nix {
    inherit pkgs lib;
    packagePath = ../../../packages/emacs;
  };
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} =
      hm@{ ... }:
      let
        # The library's configured Emacs, wrapped with every feature package.
        emacsBin = hm.config.emacs.finalPackage;
      in
      {
        imports = [ inputs.nix-emacs-home.hmModules.default ];

        # The feature files ask for these.
        _module.args = {
          nixConfig = config;
          myConfig = config.my;
          lspStyle = cfg.lspStyle;
          modalEditing = cfg.modalEditing;
          homeDirectory = config.my.homeDirectory;
          workspaceDirectory = config.my.workspaceDirectory;
        };

        emacs = {
          enable = true;
          package = cfg.pkg;
          featureDirs = [ ./features ];
          overlay = emacsOverlay;

          service.enable = cfg.enableService && !pkgs.stdenv.hostPlatform.isDarwin;

          earlyInitFile = ".emacs.d/early-init.el";
          earlyDefvar = {
            my-mono-font = {
              value = config.my.monoFont;
              doc = "Monospace font family selected from Nix.";
            };
            my-font = {
              value = config.my.font;
              doc = "Proportional font family selected from Nix.";
            };
          };
          earlyElisp = lib.optionalString pkgs.stdenv.hostPlatform.isDarwin ''
            ;; `undecorated-round' keeps rounded corners via the local patch, but
            ;; on current NS Emacs/macOS it can still leave a visible titlebar
            ;; area, so also set the built-in `undecorated' early.
            (dolist (parameter '((undecorated . t)))
              (add-to-list 'default-frame-alist parameter)
              (add-to-list 'initial-frame-alist parameter))
          '';
          earlyElispFile = ./early-init.el;
        };

        # The two scripts that poke the running daemon.  The Emacs itself comes
        # from the library, the tools it shells out to from ./default.nix.
        home.packages = [
          (pkgs.writeShellScriptBin "app-launcher" ''
            ${emacsBin}/bin/emacsclient --eval "(consult-omni-app-launcher)"
          '')
          (pkgs.writeShellScriptBin "org-capture" ''
            ${emacsBin}/bin/emacsclient -n -e '(yequake-toggle "org-capture")'
          '')
        ];

        programs.zsh.sessionVariables.EDITOR = "${emacsBin}/bin/emacsclient -c -a '${emacsBin}/bin/emacs'";
      };
  };
}

# `h`, the project jumper (https://github.com/zimbatm/h).
#
# This is upstream's `homeModules.default' (its README's "With home-manager"
# section) restated, because importing that module here fails: it defines
# `programs.zsh.initExtra', which the pinned home-manager has since removed
# ("Use `programs.zsh.initContent' instead."), and a removed-option definition
# aborts the whole evaluation.  Everything else is kept as upstream writes it --
# the package, `codeRoot', the bash hook -- with the zsh hook moved onto the
# option that exists today.  Fish is skipped: no host enables it.
#
# Upstream cannot know about this machine's zsh setup: home-manager's oh-my-zsh
# `history' plugin defines `alias h=history', and an alias both shadows the
# function at the prompt and makes zsh refuse to define it ("defining function
# based on alias"), so the unalias has to come before the eval -- in that order,
# inside the same block.  `history', `hl', `hs' and `hsi' from the plugin stay.
#
# The workspace tree is laid out as <codeRoot>/<host>/<owner>/<repo>, which is
# what `h' resolves and clones into -- and what the rex project picker's typed
# answer and projectRoots depth are built around (modules/terminal-multiplexer/
# rex.nix).
#
# The package is a Ruby script with a `#!/usr/bin/env ruby' shebang and no Ruby
# dependency.  shortcut: it runs on macOS's /usr/bin/ruby, which is on PATH both
# in a login shell and in the Rex popup; if Apple ever drops it, add pkgs.ruby to
# home.packages below.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.h;
  hPkg = inputs.h.packages.${pkgs.stdenv.hostPlatform.system}.default;
  # Upstream's hook, verbatim apart from the explicit binary path.  Double-quoted
  # so the apostrophe in lib.getExe' can never be read as a string delimiter.
  hook = "eval \"$(${lib.getExe' hPkg "h"} --setup ${escapeShellArg cfg.codeRoot})\"";
in
{
  options.modules.h = {
    enable = mkEnableOption "h, the project jumper";

    codeRoot = mkOption {
      type = types.str;
      default = "${config.my.homeDirectory}/${config.my.workspaceDirectory}";
      description = "Directory holding <host>/<owner>/<repo>; h clones missing repos into it. Upstream calls this programs.h.codeRoot.";
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ hPkg ];

      programs.bash.initExtra = hook;

      programs.zsh.initContent = mkAfter ''
        unalias h 2>/dev/null || true
        ${hook}
      '';
    };
  };
}

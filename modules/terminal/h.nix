# `h`, the project jumper (https://github.com/zimbatm/h).
#
# `h <name>` walks the code root for a directory of that name (deepest match wins,
# nothing below depth 3); `h <owner>/<repo>` and `h <url>` name a path
# <codeRoot>/<host>/<owner>/<repo> and clone it when it is missing.  The whole
# ~/workspaces tree is laid out in exactly that shape, which is also why
# `modules.rex.projectRoots` keeps its depth at 3 -- see
# modules/terminal-multiplexer/rex.nix.
#
# Upstream's `homeModules.default` is deliberately not imported.  It installs the
# bare derivation, whose `#!/usr/bin/env ruby` shebang looks Ruby up on PATH, and
# the picker popup launches with a stripped environment; the wrapper below pins
# Ruby and git instead of hoping they are reachable.
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
  hGem = inputs.h.packages.${pkgs.stdenv.hostPlatform.system}.default;
  h = pkgs.runCommand "h" { buildInputs = [ pkgs.makeWrapper ]; } ''
    makeWrapper ${lib.getExe' hGem "h"} $out/bin/h \
      --prefix PATH : ${
        lib.makeBinPath [
          pkgs.ruby
          pkgs.git
        ]
      }
  '';
in
{
  options.modules.h = {
    enable = mkEnableOption "h, the project jumper";

    codeRoot = mkOption {
      type = types.str;
      default = "${config.my.homeDirectory}/${config.my.workspaceDirectory}";
      description = "Directory holding <host>/<owner>/<repo>; h clones missing repos into it.";
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ h ];

      # initContent at mkAfter lands at the very end of .zshrc -- after
      # `source $ZSH/oh-my-zsh.sh', which is where oh-my-zsh's `history' plugin
      # defines `alias h=history'.  Aliases expand before functions are looked up,
      # so dropping this one alias is what lets h's function through; `history',
      # `hl', `hs' and `hsi' from that plugin are untouched.  Bash gets no hook:
      # it is off on darwin.
      programs.zsh.initContent = mkAfter ''
        eval "$(${lib.getExe h} --setup ${escapeShellArg cfg.codeRoot})"
        unalias h 2>/dev/null || true
      '';
    };
  };
}

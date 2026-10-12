{ nixConfig, ... }:

# The Emacs client of agent-workflows.
#
# `nixConfig` is the whole home-manager/darwin config handed in via nima's
# `_module.args`, so the load-path comes from modules.agent-cat.emacs.dir.
# That path is the Emacs directory of the agent-workflows input.
#
# The require is guarded on `noninteractive`, and that is not a hedge. nima
# packs this Elisp into `default.el`, Emacs byte-compiles that file while it
# is being built, and the load-path names a store path that is only a
# reference of the finished output and not an input of that build, so an
# unguarded require fails the build with "Cannot open load file:
# wf-service". Every other feature here defers its loads for the same reason;
# `use-package` does it and nothing else needs `require`.
#
# wf-manager-profiles is left alone. Service mode reads client profile files
# that `agentic-run --manager init` writes, and a generated credential is
# state, not configuration.
let
  cfg = nixConfig.modules.agent-cat or { };
  dir = (cfg.emacs or { }).dir or null;
  enable = (cfg.enable or false) && dir != null;
in
{
  inherit enable;

  elisp =
    if enable then
      ''
        (add-to-list 'load-path "${dir}")
        (unless noninteractive
          (require 'wf-service))
      ''
    else
      "";
}

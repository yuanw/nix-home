{ ... }:

# Structural Lean templates (eval / check / structure) for Nael buffers.
# Tempel itself is installed and bound by features/tempel.nix; this only
# contributes a buffer-local `tempel-template-sources' entry, so it needs no
# packages of its own.  All Elisp lives in lean-templates.el.
{
  elispFile = ./lean-templates.el;
}

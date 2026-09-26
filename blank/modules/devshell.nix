# nix-home-private is not present: clone it beside nix-home, or build the public
# half with --override-input nix-home-private path:$PWD/blank (see
# nix-home-private/README.md).  The Workiva tools are not optional for WK01174,
# so this does not return an empty attrset: that would expose packages that look
# right and cannot do the work.
throw
  "nix-home-private is not present: the WK01174 tools (nix run .#prefetch-work-sources, …) need it; see nix-home-private/README.md"

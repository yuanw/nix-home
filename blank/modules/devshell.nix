# nix-home-private is not present: clone it beside nix-home, or build the public
# half with --override-input nix-home-private path:$PWD/blank (see
# nix-home-private/README.md).  The Workiva dev-shell tools are not optional for
# WK01174, so this does not return an empty list: an empty list would build a
# shell that looks right and cannot do the work.
throw "nix-home-private is not present: the WK01174 dev shell needs the Workiva tools from it; see nix-home-private/README.md"

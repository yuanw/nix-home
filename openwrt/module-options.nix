{ pkgs, lib, ... }:
{
  options.uci.settings = lib.mkOption {
    default = { };
    type = (pkgs.formats.json { }).type;
    description = ''
      UCI configuration as config name -> section name -> section.  A section is
      a set carrying a `_type` (its uci section type); a set whose value is a list
      is an anonymous section, which is addressed as `config.@name[i]`.
      Values containing `@secret@` are filled from $NIX_UCI_SECRETS.
    '';
  };

  options.uci.raw = lib.mkOption {
    default = [ ];
    type = lib.types.listOf lib.types.str;
    description = ''
      Verbatim `uci batch` commands, emitted after the declared config.  The escape
      hatch for what the typed model cannot say: `reorder` to fix section order
      (firewall rules match first to last, so order is precedence) or `rename` to
      give an anonymous section a name.  No `uci` prefix, and only
      set/add/add_list/del_list/delete/rename/reorder are allowed -- commit and
      revert belong to the recipes, not to the config.

      Imperative, so it applies on every run and does not converge the way the rest
      does: for one-off ordering or migrations, removed once the target matches.
    '';
  };
}

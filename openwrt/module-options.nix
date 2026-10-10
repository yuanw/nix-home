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
}

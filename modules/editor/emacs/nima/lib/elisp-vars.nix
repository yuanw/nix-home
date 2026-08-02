{ lib }:

let
  inherit (lib) concatStringsSep mapAttrsToList optionalString;

  raw = expr: { __elispRaw = expr; };

  toElisp =
    value:
    if value == null then
      "nil"
    else if builtins.isBool value then
      if value then "t" else "nil"
    else if builtins.isInt value || builtins.isFloat value then
      toString value
    else if builtins.isString value then
      builtins.toJSON value
    else if builtins.isList value then
      "'(" + concatStringsSep " " (map toElisp value) + ")"
    else if builtins.isAttrs value && value ? __elispRaw then
      value.__elispRaw
    else
      throw "Unsupported Elisp value: ${builtins.toJSON value}";

  mkDefvar =
    name: valueOrAttrs:
    let
      attrs =
        if builtins.isAttrs valueOrAttrs && valueOrAttrs ? value then
          valueOrAttrs
        else
          { value = valueOrAttrs; };
    in
    ''
      (defvar ${name} ${toElisp attrs.value}${
        optionalString (attrs ? doc) " ${builtins.toJSON attrs.doc}"
      })
    '';

  mkSetq =
    attrs:
    concatStringsSep "\n" (mapAttrsToList (name: value: "(setq ${name} ${toElisp value})") attrs);
in
{
  inherit
    raw
    toElisp
    mkDefvar
    mkSetq
    ;

  mkDefvars = attrs: concatStringsSep "\n" (mapAttrsToList mkDefvar attrs);
}

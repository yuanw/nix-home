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

  mkSetForm =
    form: attrs:
    concatStringsSep "\n" (mapAttrsToList (name: value: "(${form} ${name} ${toElisp value})") attrs);

  mkSetq = mkSetForm "setq";

  mkSetopt = mkSetForm "setopt";

  mkDefvars = attrs: concatStringsSep "\n" (mapAttrsToList mkDefvar attrs);

  readElispSource =
    source:
    if source == null then
      ""
    else if builtins.isPath source then
      builtins.readFile source
    else if builtins.isString source then
      source
    else
      throw "Unsupported Elisp source: expected path, string, or null";

  withElisp =
    {
      file ? null,
      elisp ? null,
      defvar ? { },
      setq ? { },
      setopt ? { },
      before ? "",
      after ? "",
    }:
    if file != null && elisp != null then
      throw "withElisp: set either `file` or `elisp`, not both"
    else
      ''
        ${mkDefvars defvar}
        ${mkSetq setq}
        ${mkSetopt setopt}
        ${before}
        ${readElispSource file}
        ${readElispSource elisp}
        ${after}
      '';

  withElispFile = args: withElisp args;
in
{
  inherit
    raw
    toElisp
    mkDefvar
    mkSetq
    mkSetopt
    readElispSource
    withElisp
    withElispFile
    mkDefvars
    ;
}

{ lib }:

let
  inherit (lib)
    concatStringsSep
    mapAttrsToList
    optionalString
    ;

  attrsToPairs = attrs: mapAttrsToList (key: value: ''("${key}" . ${value})'') attrs;
  listForm = values: concatStringsSep "\n        " values;
in
{
  mkUsePackage =
    name:
    {
      enable ? true,
      defer ? false,
      demand ? false,
      after ? [ ],
      hook ? [ ],
      bind ? { },
      mode ? [ ],
      commands ? [ ],
      functions ? [ ],
      preface ? "",
      init ? "",
      config ? "",
      extraConfig ? "",
    }:
    optionalString enable ''
      (use-package ${name}
        ${optionalString defer ":defer t"}
        ${optionalString demand ":demand t"}
        ${optionalString (after != [ ]) ":after (${concatStringsSep " " after})"}
        ${optionalString (commands != [ ]) ":commands (${concatStringsSep " " commands})"}
        ${optionalString (functions != [ ]) ":functions (${concatStringsSep " " functions})"}
        ${optionalString (mode != [ ]) ":mode (${listForm mode})"}
        ${optionalString (hook != [ ]) ":hook (${listForm hook})"}
        ${optionalString (bind != { }) ":bind (${listForm (attrsToPairs bind)})"}
        ${optionalString (preface != "") ''
          :preface
          ${preface}
        ''}
        ${optionalString (init != "") ''
          :init
          ${init}
        ''}
        ${optionalString (config != "") ''
          :config
          ${config}
        ''}
        ${extraConfig})
    '';
}

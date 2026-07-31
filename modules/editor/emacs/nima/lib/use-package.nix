{ lib }:

let
  inherit (lib)
    concatMap
    concatStringsSep
    hasAttr
    isFunction
    isList
    isString
    mapAttrsToList
    optional
    optionalString
    ;

  attrsToPairs = attrs: mapAttrsToList (key: value: ''("${key}" . ${value})'') attrs;
  listForm = values: concatStringsSep "\n        " values;

  packageToList =
    epkgs: package:
    if package == null then
      [ ]
    else if isString package then
      optional (hasAttr package epkgs) epkgs.${package}
    else if isFunction package then
      let
        value = package epkgs;
      in
      if isList value then value else [ value ]
    else if isList package then
      concatMap (packageToList epkgs) package
    else
      [ package ];
in
{
  inherit packageToList;

  mkUsePackage =
    name:
    {
      enable ? true,
      noRequire ? false,
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
        ${optionalString noRequire ":no-require t"}
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

  mkUsePackageFeature =
    name:
    args@{
      enable ? true,
      package ? name,
      extraPackages ? [ ],
      ...
    }:
    {
      inherit enable;

      epkgs = epkgs: packageToList epkgs package ++ extraPackages;

      elisp =
        let
          usePackageArgs = builtins.removeAttrs args [
            "package"
            "extraPackages"
          ];
        in
        (import ./use-package.nix { inherit lib; }).mkUsePackage name usePackageArgs;
    };
}

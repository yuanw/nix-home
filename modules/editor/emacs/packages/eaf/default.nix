# based on https://codeberg.org/heraplem/nix-emacs-extra/src/branch/emacs-application-framework/packages/eaf/default.nix
{
  lib,
  fetchFromGitHub,
  stdenv,
  writeText,
  melpaBuild,
  elpa2nix,
  melpa2nix,

  python3,

  withSwayWMSupport ? stdenv.isLinux,
  jq ? null,

  withUnitySupport ? stdenv.isLinux,
  xdotool ? null,
  withX11Support ? true,
  wmctrl ? null,
  ...
}:
let
  inherit (lib) readFile;
  python = python3.withPackages (pkgs: [
    pkgs.easyocr
    pkgs.epc
    pkgs.lxml
    pkgs.pygetwindow
    pkgs.pyqt6
    pkgs.pyqt6-sip
    pkgs.pyqt6-webengine
    pkgs.qrcode
    pkgs.requests
    pkgs.sexpdata
  ]);
in
(melpaBuild (_finalAttrs: {

  pname = "eaf";
  version = "0-unstable-2025-06-15";

  src = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "emacs-application-framework";
    rev = "3f3a6862fc27919ccdc5cb9335222fae385c8a89";
    sha256 = lib.fakeSha256;
    #sha256 = "sha256-7epTvu9f4Fn7XrEZOVZEieG/Qkt3Vs2M939jJE4EwaA=";
  };

  patchPhase = ''
    runHook prePatch

    sed -i s#'defcustom eaf-python-command .*'#'defcustom eaf-python-command "${python.interpreter}"'# eaf.el
    ${if withSwayWMSupport then "substituteInPlace eaf.el --replace jq ${jq}/bin/jq" else ""}
    ${
      if withUnitySupport then "substituteInPlace eaf.el --replace xdotool ${xdotool}/bin/xdotool" else ""
    }
    ${if withX11Support then "substituteInPlace eaf.el --replace wmctrl ${wmctrl}/bin/wmctrl" else ""}

    mv core/eaf-epc.el .
    mv extension/* .

    runHook postPatch
  '';

  elpa2nix = writeText "elpa2nix.el" ''
    ${readFile elpa2nix}
    (defun byte-recompile-directory (&rest _))
  '';
  melpa2nix = writeText "melpa2nix.el" ''
    ${readFile melpa2nix}
    (defun byte-recompile-directory (&rest _))
  '';
})).overrideAttrs
  {
    # Override genericBuild's postInstall, which tries to native-compile Elisp
    # files.
    postInstall = ''
      DST=$out/share/emacs/site-lisp/elpa/$ename-$melpaVersion/

      mv eaf.py $DST
      mv applications.json $DST
      mv core/* $DST
    '';
  }

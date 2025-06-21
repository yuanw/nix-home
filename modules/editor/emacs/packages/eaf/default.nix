# based on https://codeberg.org/heraplem/nix-emacs-extra/src/branch/emacs-application-framework/packages/eaf/default.nix
{
  fetchFromGitHub,
  stdenv,
  melpaBuild,

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
    pkgs.packaging
    pkgs.pymupdf
  ]);
in
(melpaBuild (_finalAttrs: {

  pname = "eaf";
  version = "0-unstable-2025-06-15";

  src = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "emacs-application-framework";
    rev = "3f3a6862fc27919ccdc5cb9335222fae385c8a89";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-J/98TamZYkugOezuh/e2EQdWxBPv+wWlXFAxkjxmVDY=";
  };

  patchPhase = ''
    runHook prePatch

    sed -i s#'defcustom eaf-python-command .*'#'defcustom eaf-python-command "${python.interpreter}"'# eaf.el
    ${if withSwayWMSupport then "substituteInPlace eaf.el --replace jq ${jq}/bin/jq" else ""}
    ${
      if withUnitySupport then "substituteInPlace eaf.el --replace xdotool ${xdotool}/bin/xdotool" else ""
    }
    ${if withX11Support then "substituteInPlace eaf.el --replace wmctrl ${wmctrl}/bin/wmctrl" else ""}


    runHook postPatch
  '';
})).overrideAttrs
  {
    # Override genericBuild's postInstall, which tries to native-compile Elisp
    # files.
    # TODO: mv only the needed files
    postInstall = ''
      DST=$out/share/emacs/site-lisp/elpa/$ename-$melpaVersion/

      mv * $DST

    '';
  }

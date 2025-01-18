{
  melpaBuild,
  fetchFromGitHub,
  writeText,
  pkgs,

  # Elisp dependencies
  ctable,
  deferred,
  epc,
  s,

  # Native dependencies
  nodejs,
  python3,
  wmctrl,
  xdotool,
}:

let
  # TODO: Package nodejs environment

  pythonEnv = (
    (python3.withPackages (ps: [
      # ps.pyqtwebengine
      ps.pyqt6
      ps.pyqt6-webengine
      ps.qrcode
      ps.qtconsole
      ps.retry
      ps.pymupdf
      ps.sexpdata
      ps.epc
      # Wrap native dependencies in python env $PATH
      pkgs.aria2
    ])).override
      { ignoreCollisions = true; }
  );

  node = "${nodejs}/bin/node";

  pname = "eaf";
  version = "20210309.0";

  eaf-terminal = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "eaf-terminal";
    rev = "b7ddb82baf9604cb52a83d6e0631e6a5edc415b1";
    hash = "sha256-4TRIAcReJs9a91DMKD+puApmSstxYLgLHV300GFDufQ=";
  };

  eaf-browser = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "eaf-browser";
    rev = "9761f7bd22aa69f144a0a032643b0834dde6cb60";
    hash = "sha256-jcCn16lXqcq1UcekekJiTfRBjjgaY0Hkz69ycElSzuA=";
  };

  eaf-pdf-viewer = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "eaf-pdf-viewer";
    rev = "0b79e9000551200b6a0d2206de14b87d9ed3c9c9";
    hash = "sha256-Co5RWWI0OC1w5JrCVpbwafuEEluh48JDzkMQp5QdIbE=";
  };

in
melpaBuild {

  inherit pname version;

  src = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "emacs-application-framework";
    rev = "9bf8caba36e27fafcef5d8e68969a1a8eb5432ed";
    sha256 = "sha256-4CPhEO4d66GzD3vOcNyRl6DmRDYiiIeRhX/9RUPZ+PM=";
  };

  dontConfigure = true;
  dontBuild = true;

  postPatch = ''
    substituteInPlace eaf.el \
      --replace '"xdotool' '"${xdotool}/bin/xdotool' \
      --replace '"wmctrl' '"${wmctrl}'
    sed -i s#'defcustom eaf-python-command .*'#'defcustom eaf-python-command "${pythonEnv.interpreter}"'# eaf.el
    mkdir app
    cp -r ${eaf-terminal} app/terminal
    cp -r ${eaf-pdf-viewer} app/pdf-viewer
    cp -r ${eaf-browser} app/browser
    substituteInPlace app/terminal/buffer.py --replace-warn \
      '"node"' \
      '"${node}"'
  '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/emacs-$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/emacs-$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (eaf
    :repo "emacs-eaf/emacs-application-framework"
    :fetcher github
    :files ("*")
  '';

  packageRequires = [
    ctable
    deferred
    epc
    s
  ];

}

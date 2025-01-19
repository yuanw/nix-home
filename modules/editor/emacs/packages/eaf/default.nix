{
  melpaBuild,
  fetchFromGitHub,
  writeText,
  pkgs,
  qt6Packages,
  # Elisp dependencies
  ctable,
  deferred,
  epc,
  s,
  lndir,
  pkg-config,
  # Native dependencies
  python3,
  wmctrl,
}:

let
  # TODO: Package nodejs environment
  python = python3.override {
    packageOverrides = _final: prev: {
      pyqt6-webengine = prev.pyqt6-webengine.overridePythonAttrs (_oldAttrs: rec {
        nativeBuildInputs = with qt6Packages; [
          pkg-config
          lndir
          qtwebengine
          qmake
          qtwebchannel
        ];
        buildInputs = with qt6Packages; [
          qtwebengine
          qtwebchannel
        ];
        postPatch = ''
          sed -i \
            '/\[tool.sip.project\]/a\
            verbose = true\
            sip-include-dirs = [\"${prev.pyqt6}/${python.sitePackages}/PyQt6/bindings\"]' \
            pyproject.toml
        '';
      });

    };
  };

  pythonEnv = (
    (python.withPackages (ps: [
      ps.pandas
      ps.requests
      ps.sexpdata
      ps.tld
      ps.pyqt6
      ps.pyqt6-sip
      ps.pyqt6-webengine
      ps.qrcode
      ps.qtconsole
      ps.retry
      ps.pymupdf
      ps.markdown
      ps.epc
      # needs for eaf-browserxx
      ps.pysocks
      # Wrap native dependencies in python env $PATH
      pkgs.aria2
    ])).override
      { ignoreCollisions = true; }
  );

  pname = "eaf";
  version = "20210309.0";

  eaf-browser = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "eaf-browser";
    rev = "9761f7bd22aa69f144a0a032643b0834dde6cb60";
    #hash = lib.fakeHash;
    hash = "sha256-jcCn16lXqcq1UcekekJiTfRBjjgaY0Hkz69ycElSzuA=";
  };

in
melpaBuild {

  inherit pname version;

  src = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "emacs-application-framework";
    rev = "73f85b24914227a140e40c5c8bb7eb6efa7a41b2";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-7epTvu9f4Fn7XrEZOVZEieG/Qkt3Vs2M939jJE4EwaA=";
  };

  dontConfigure = true;
  dontBuild = true;

  postPatch = ''
    substituteInPlace eaf.el \
      --replace '"wmctrl' '"${wmctrl}'
    sed -i s#'defcustom eaf-python-command .*'#'defcustom eaf-python-command "${pythonEnv.interpreter}"'# eaf.el
    mkdir app
    cp -r ${eaf-browser} app/browser

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

  __darwinAllowLocalNetworking = true;

  packageRequires = [
    ctable
    deferred
    epc
    s
  ];

}

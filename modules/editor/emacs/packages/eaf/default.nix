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
  nodejs,
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

            cat pyproject.toml
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

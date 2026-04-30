{ lib
, stdenv
, melpaBuild
, fetchFromGitea
, mupdf-headless
, pkg-config
, ...
}:

let
  version = "0.3.0";

  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "MonadicSheep";
    repo = "emacs-reader";
    rev = "0.3.0";
    hash = "sha256-BpuWWGt46BVgQZPHzeLEbzT+ooR4v29R+1Lv0K55kK8=";
  };

  render-core = stdenv.mkDerivation {
    inherit version src;
    pname = "render-core";

    strictDeps = true;

    buildFlags = [
      "CC=cc"
      "USE_PKGCONFIG=yes"
    ];

    nativeBuildInputs = [ pkg-config ];

    buildInputs = [ mupdf-headless ];

    installPhase = ''
      runHook preInstall

      install -Dm444 -t $out/lib/ render-core${stdenv.hostPlatform.extensions.sharedLibrary}

      runHook postInstall
    '';
  };

in
melpaBuild {
  pname = "reader";

  inherit src version;

  files = ''
    (:defaults "${lib.getLib render-core}/lib/render-core.*")
  '';

  meta = {
    homepage = "https://codeberg.org/MonadicSheep/emacs-reader";
    description = "An all-in-one document reader for all formats in Emacs, backed by MuPDF";
    license = lib.licenses.gpl3Plus;
    maintainers = [ ];
  };
}

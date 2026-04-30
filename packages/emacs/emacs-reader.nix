{ lib
, stdenv
, melpaBuild
, fetchFromGitea
, mupdf-headless
, pkg-config
, ...
}:

let
  version = "0-unstable-2025-07-28";

  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "MonadicSheep";
    repo = "emacs-reader";
    rev = "9824fc91eb51bec0edb8c3634a74d73226d26525";
    hash = "sha256-84v8NzAjH0djD98RKElzy3dIkSSh1c3OyjrHXR8cQrY=";
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

    # Only build the shared library; skip autoloads generation and byte-compilation
    buildPhase = ''
      runHook preBuild
      make render-core.so $buildFlags
      runHook postBuild
    '';

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

{
  lib,
  stdenv,
  melpaBuild,
  fetchFromGitea,
  mupdf-headless,
  pkg-config,
  ...
}:

let
  version = "0-unstable-2025-07-28";

  libName = "render-core${stdenv.hostPlatform.extensions.sharedLibrary}";

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

    # Darwin: upstream disables pkg-config and expects Homebrew MuPDF; Nix has mupdf via pkg-config.
    # Also, HAVE_NIX=yes runs before the Darwin branch and forces USE_PKGCONFIG=no, which then
    # incorrectly selects the Homebrew MuPDF branch — skip that assignment on Darwin.
    postPatch = lib.optionalString stdenv.isDarwin ''
            substituteInPlace Makefile \
              --replace-fail 'else ifeq ($(HAVE_NIX),yes)
        $(info Nix detected: skipping pkg-config checks.)
        USE_PKGCONFIG := no' 'else ifeq ($(HAVE_NIX),yes)
      ifneq ($(OS_NAME),Darwin)
        $(info Nix detected: skipping pkg-config checks.)
        USE_PKGCONFIG := no
      endif'
            substituteInPlace Makefile \
              --replace-fail 'else ifeq ($(OS_NAME),Darwin)
        $(info macOS detected: skipping pkg-config and using Homebrew for MuPDF paths.)
        USE_PKGCONFIG := no' 'else ifeq ($(OS_NAME),Darwin)
        $(info macOS (Nix stdenv): use pkg-config for MuPDF)
        USE_PKGCONFIG := yes'
    '';

    # Only build the shared library; skip autoloads generation and byte-compilation
    buildPhase = ''
      runHook preBuild
      make ${libName} $buildFlags
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      install -Dm444 -t $out/lib/ ${libName}

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

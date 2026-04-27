{
  lib,
  stdenv,
  fetchFromGitHub,
}:

let
  soExt = if stdenv.isDarwin then "dylib" else "so";
in
stdenv.mkDerivation {
  pname = "tree-sitter-moonbit";
  version = "unstable-2026-04-07";

  src = fetchFromGitHub {
    owner = "moonbitlang";
    repo = "tree-sitter-moonbit";
    rev = "82237f3f508d09fb09668d9885c99a562a756fe0";
    sha256 = "sha256-xK+v+tc0XRbtYv9e9VNEFo2PITt/0WT2pANtZLerkpc=";
  };

  buildPhase = ''
    runHook preBuild
    make libtree-sitter-moonbit.${soExt}
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib
    cp libtree-sitter-moonbit.${soExt} $out/lib/
    runHook postInstall
  '';

  meta = with lib; {
    description = "Tree-sitter grammar for MoonBit";
    homepage = "https://github.com/moonbitlang/tree-sitter-moonbit";
    license = licenses.mit;
    platforms = platforms.all;
  };
}

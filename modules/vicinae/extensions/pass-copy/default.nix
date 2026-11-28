# Builds the local `pass-copy` Vicinae extension with esbuild directly, matching
# `vici build`'s exact output format (cjs bundle with React & @vicinae/api kept
# external, package.json, and assets/) without needing a vendored npm lockfile.
{
  lib,
  pkgs,
  pass ? pkgs.pass,
}:
pkgs.runCommand "vicinae-extension-pass-copy"
  {
    nativeBuildInputs = [ pkgs.esbuild ];
  }
  ''
    mkdir -p "$out/assets"
    cp ${./package.json} "$out/package.json"
    cp ${./assets/extension_icon.svg} "$out/assets/extension_icon.svg"

    esbuild ${./src/pass-copy.tsx} \
      --bundle \
      --minify \
      --format=cjs \
      --jsx=automatic \
      --platform=node \
      --external:react \
      --external:react/jsx-runtime \
      --external:@vicinae/api \
      --external:@raycast/api \
      --define:__PASS_BIN__=${lib.escapeShellArg (builtins.toJSON (lib.getExe pass))} \
      --outfile="$out/pass-copy.js"
  ''

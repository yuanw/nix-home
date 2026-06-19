# Emacs `ghostel`: Darwin needs pure shims for Zig’s xcode-select/xcrun calls and an
# overridden `ghostel.module` build so the native module compiles in the Nix sandbox.
# Same idea as:
# https://github.com/mzacuna/zix-zonfig/blob/af9690b4b81f5e4329a0776a340fbdda320cf215/modules/darwin/emacs/emacs.nix
{ pkgs, isDarwin }:
let
  appleSdkZigShims =
    if isDarwin then
      pkgs.runCommand "apple-sdk-zig-shims" { } ''
        mkdir -p "$out/bin"

        cat > "$out/bin/xcode-select" <<'EOF'
        #!${pkgs.runtimeShell}
        if [ "$1" = "--print-path" ]; then
          echo "${pkgs.apple-sdk}/Platforms/MacOSX.platform/Developer"
          exit 0
        fi
        echo "unsupported xcode-select invocation: $*" >&2
        exit 1
        EOF

        cat > "$out/bin/xcrun" <<'EOF'
        #!${pkgs.runtimeShell}
        if [ "$1" = "--sdk" ] && [ "$3" = "--show-sdk-path" ]; then
          echo "${pkgs.apple-sdk.sdkroot}"
          exit 0
        fi
        echo "unsupported xcrun invocation: $*" >&2
        exit 1
        EOF

        chmod +x "$out/bin/xcode-select" "$out/bin/xcrun"
      ''
    else
      null;
in
{
  emacsOverrides = _self: super: {
    ghostel =
      if isDarwin then
        let
          base = super.ghostel;
          ghostelModule = base.module.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
              appleSdkZigShims
              pkgs.apple-sdk
            ];
            env = (old.env or { }) // {
              SDKROOT = pkgs.apple-sdk.sdkroot;
            };
          });
        in
        base.overrideAttrs (old: {
          preBuild = ''
            install ${ghostelModule}/lib/libghostel-module${pkgs.stdenv.hostPlatform.extensions.sharedLibrary} \
              ghostel-module${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}
          '';
          passthru = (old.passthru or { }) // {
            module = ghostelModule;
          };
        })
      else
        super.ghostel;
  };

  usePackageGhostel = {
    enable = true;
  };
}

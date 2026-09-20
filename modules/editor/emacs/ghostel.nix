# Emacs `ghostel`: Darwin needs an SDK for libghostty's Zig build.
# Modeled after nixpkgs' ghostel package plus the apple-sdk addition from:
# https://github.com/AndrewBastin/nixos-config/blob/f6fca4e83703816a742955874651b08d3e684114/packages/ghostel/package.nix#L45
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
            nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ appleSdkZigShims ];
            buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.apple-sdk ];
            env = (old.env or { }) // {
              DEVELOPER_DIR = "${pkgs.apple-sdk}/Platforms/MacOSX.platform/Developer";
              SDKROOT = pkgs.apple-sdk.sdkroot;
            };
          });
        in
        base.overrideAttrs (old: {
          preBuild = ''
            install ${ghostelModule}/ghostel-module${pkgs.stdenv.hostPlatform.extensions.sharedLibrary} \
              ghostel-module${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}
            install --mode=444 ${ghostelModule}/ghostel-module.version ghostel-module.version
          '';
          passthru = (old.passthru or { }) // {
            module = ghostelModule;
          };
        })
      else
        super.ghostel;
  };

}

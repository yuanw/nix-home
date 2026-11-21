# Emacs `ghostel`: rexel needs 0.66.0, which is ahead of nixpkgs' 0.59.0, so the
# version is pinned here; Darwin additionally needs an SDK for libghostty's Zig
# build.
# Modeled after nixpkgs' ghostel package plus the apple-sdk addition from:
# https://github.com/AndrewBastin/nixos-config/blob/f6fca4e83703816a742955874651b08d3e684114/packages/ghostel/package.nix#L45
{
  pkgs,
  lib,
  isDarwin,
}:
let
  # Bump together: `nix-prefetch-github dakra ghostel --rev v<version>' for the
  # source, and a build with a wrong hash for zigDeps (nix prints the right one).
  # 0.66.0 is what `rexel' requires (Package-Requires ghostel 0.66.0).
  version = "0.66.0";
  src = pkgs.fetchFromGitHub {
    owner = "dakra";
    repo = "ghostel";
    tag = "v${version}";
    hash = "sha256-0uYcK7qij6gOANQEPt3PQlCFwTFLnm4QENPa+JhxU3E=";
  };
  # 0.66.0's build.zig.zon raises minimum_zig_version from 0.16 to 0.17.
  zig = pkgs.zig_0_17;
  zigDeps = zig.fetchDeps {
    pname = "ghostel";
    inherit version src;
    fetchAll = true;
    hash = "sha256-X16YfBc8i84G2/Dd7/qPhm0lRMDJDLfWCcLeXymb4yg=";
  };

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
  emacsOverrides =
    _self: super:
    let
      base = super.ghostel;
      # nixpkgs builds the Zig module as a separate derivation, from the version
      # and source the package was evaluated with, so the pin above has to be
      # handed to it by hand -- and its `zig' sits in nativeBuildInputs, which
      # an `overrideAttrs' of `zig' alone would leave at 0.16.
      ghostelModule = base.module.overrideAttrs (old: {
        inherit version src zigDeps;
        nativeBuildInputs =
          (lib.filter (dep: dep != old.zig) (old.nativeBuildInputs or [ ]))
          ++ [ zig ]
          ++ lib.optionals isDarwin [ appleSdkZigShims ];
        buildInputs = (old.buildInputs or [ ]) ++ lib.optionals isDarwin [ pkgs.apple-sdk ];
        env =
          (old.env or { })
          // lib.optionalAttrs isDarwin {
            DEVELOPER_DIR = "${pkgs.apple-sdk}/Platforms/MacOSX.platform/Developer";
            SDKROOT = pkgs.apple-sdk.sdkroot;
          };
      });
    in
    {
      ghostel = base.overrideAttrs (old: {
        inherit
          version
          src
          zig
          zigDeps
          ;
        preBuild = ''
          install ${ghostelModule}/ghostel-module${pkgs.stdenv.hostPlatform.extensions.sharedLibrary} \
            ghostel-module${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}
          install --mode=444 ${ghostelModule}/ghostel-module.version ghostel-module.version
        '';
        passthru = (old.passthru or { }) // {
          module = ghostelModule;
        };
      });
    };

}

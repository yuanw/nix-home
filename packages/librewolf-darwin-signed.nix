# macOS .app post-process for Nix-built LibreWolf.
#
# The nixpkgs firefox/librewolf wrapper installs a shell script as
# CFBundleExecutable and leaves Info.plist as a symlink — both prevent
# ad-hoc codesigning and stop the GPU helper from launching (WebGL broken).
{
  stdenv,
  librewolf,
}:
stdenv.mkDerivation {
  pname = "${librewolf.pname}-cask";
  version = librewolf.version;

  dontUnpack = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -R ${librewolf}/Applications $out/
    app="$out/Applications/LibreWolf.app"
    macos="$app/Contents/MacOS"
    chmod -R u+w $out
    xattr -cr $out 2>/dev/null || true

    materialize() {
      local path="$1"
      if [ -L "$path" ]; then
        local target
        target=$(readlink "$path")
        rm "$path"
        cp "$target" "$path"
      fi
    }

    # The nix wrapper symlinks most bundle metadata; codesign rejects those.
    find "$app" -type l | while read -r link; do
      materialize "$link"
    done

    # GPU sandbox needs a signed Mach-O as the bundle executable.
    if [ -f "$macos/.librewolf-old" ]; then
      cp "$macos/.librewolf-old" "$macos/librewolf"
      chmod +x "$macos/librewolf"
      rm -f "$macos/.librewolf-old"
    fi
    rm -f "$macos/librewolf.sh"

    sign() {
      /usr/bin/codesign --force --sign - "$1"
    }

    for helper in gpu-helper.app plugin-container.app media-plugin-helper.app; do
      if [ -d "$macos/$helper" ]; then
        sign "$macos/$helper"
      fi
    done

    if [ -d "$app/Contents/Frameworks/ChannelPrefs.framework" ]; then
      sign "$app/Contents/Frameworks/ChannelPrefs.framework"
    fi

    find "$app" -type f | while read -r file; do
      if file -b "$file" | grep -q Mach-O; then
        sign "$file" 2>/dev/null || true
      fi
    done

    sign "$app"

    runHook postInstall
  '';

  meta = librewolf.meta // {
    description = "LibreWolf macOS application bundle with ad-hoc signing (WebGL/GPU)";
  };
}

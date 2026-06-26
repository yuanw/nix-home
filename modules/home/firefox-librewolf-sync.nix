# Mirror the Firefox HM profile into LibreWolf (macOS) for browser-cli.
# Declarative config lives in modules/browsers/firefox; LibreWolf gets a copy on each switch.
{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:
let
  cfg = config.programs.mics-skills;
  browserCliEnabled = cfg.enable or false && lib.elem "browser-cli" (cfg.skills or [ ]);
  firefoxEnabled = osConfig.modules.browsers.firefox.enable or false;
  syncEnabled = browserCliEnabled && firefoxEnabled && pkgs.stdenv.isDarwin;

  firefoxProfile = "Library/Application Support/Firefox/Profiles/home";
  librewolfRoot = "Library/Application Support/LibreWolf";
  librewolfProfile = "${librewolfRoot}/Profiles/home";
in
{
  config = lib.mkIf syncEnabled {
    home.file."${librewolfRoot}/profiles.ini".text = ''
      [General]
      StartWithLastProfile=1
      Version=2

      [Profile0]
      Default=1
      IsRelative=1
      Name=home
      Path=Profiles/home
    '';

    home.activation.syncFirefoxProfileToLibrewolf = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      src="${config.home.homeDirectory}/${firefoxProfile}"
      dst="${config.home.homeDirectory}/${librewolfProfile}"

      if [ ! -d "$src" ]; then
        echo "firefox-librewolf-sync: no Firefox profile at $src, skipping" >&2
        exit 0
      fi

      mkdir -p "$dst"

      ${lib.getExe pkgs.rsync} -a --delete \
        --exclude '.parentlock' \
        --exclude 'parent.lock' \
        --exclude 'lock' \
        --exclude '.lock' \
        --exclude 'SingletonLock' \
        --exclude 'sessionstore.jsonlz4' \
        --exclude 'sessionstore-backups/' \
        "$src/" "$dst/"
    '';
  };
}

# Home Manager side of browser-cli (config.toml + native messaging host).
# macOS: LibreWolf carries the extension (modules/browsers/browser-cli-darwin.nix).
# LibreWolf HM config mirrors Firefox via modules/browsers/gecko-home.nix.
# https://github.com/Mic92/mics-skills/tree/main/browser-cli#configuring-the-browser-path
{
  config,
  lib,
  pkgs,
  inputs,
  osConfig,
  ...
}:
let
  cfg = config.programs.mics-skills;
  enable = cfg.enable or false && lib.elem "browser-cli" (cfg.skills or [ ]);
  micsSkills = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
  serverBin = "${micsSkills.browser-cli}/bin/browser-cli-server";
  wrapperPath = "${config.home.homeDirectory}/.local/bin/browser-cli-server-wrapper";

  librewolfCfg = osConfig.modules.browsers.librewolf or { enable = false; };
  firefoxPkg = osConfig.modules.browsers.firefox.pkg or null;
  librewolfPkg = librewolfCfg.pkg or pkgs.librewolf;

  # browser-cli uses firefox_path / BROWSER_CLI_FIREFOX_PATH for LibreWolf too.
  browserPath =
    if librewolfCfg.enable or false && pkgs.stdenv.isDarwin then
      "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf"
    else if librewolfCfg.enable or false then
      lib.getExe librewolfPkg
    else
      lib.getExe (if firefoxPkg != null then firefoxPkg else pkgs.firefox);

  nativeMessagingManifest = {
    name = "io.thalheim.browser_cli.bridge";
    description = "Browser CLI Bridge Server";
    path = wrapperPath;
    type = "stdio";
    allowed_extensions = [ "browser-cli-controller@thalheim.io" ];
  };

  nativeMessagingHosts =
    if pkgs.stdenv.isDarwin then
      {
        "Library/Application Support/LibreWolf/NativeMessagingHosts/io.thalheim.browser_cli.bridge.json".text =
          builtins.toJSON nativeMessagingManifest;
        "Library/Application Support/Mozilla/NativeMessagingHosts/io.thalheim.browser_cli.bridge.json".text =
          builtins.toJSON nativeMessagingManifest;
      }
    else
      {
        ".mozilla/native-messaging-hosts/io.thalheim.browser_cli.bridge.json".text =
          builtins.toJSON nativeMessagingManifest;
        ".librewolf/native-messaging-hosts/io.thalheim.browser_cli.bridge.json".text =
          builtins.toJSON nativeMessagingManifest;
      };
in
{
  config = lib.mkIf enable {
    xdg.configFile."browser-cli/config.toml".text = ''
      firefox_path = "${browserPath}"
    '';

    home.sessionVariables.BROWSER_CLI_FIREFOX_PATH = lib.mkDefault browserPath;

    home.file = {
      ".local/bin/browser-cli-server-wrapper" = {
        executable = true;
        text = ''
          #!${pkgs.bash}/bin/bash
          exec ${serverBin} "$@"
        '';
      };
    }
    // nativeMessagingHosts;

    home.activation.clearBrowserCliDefaultsPolicy = lib.hm.dag.entryAfter [ "writeBoundary" ] (
      lib.mkIf pkgs.stdenv.isDarwin ''
        /usr/bin/defaults delete org.mozilla.firefox ExtensionSettings 2>/dev/null || true
      ''
    );

    # Drop stale bridge processes left behind when the extension disconnects.
    home.activation.resetBrowserCliBridge = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ -n "''${DRY_RUN:-}" ]; then
        exit 0
      fi
      pkill -f '[b]rowser-cli-server' 2>/dev/null || true
      if [ -n "''${TMPDIR:-}" ] && [ -d "''${TMPDIR}/browser-cli" ]; then
        rm -f "''${TMPDIR}/browser-cli/browser-cli.sock"
      fi
    '';
  };
}

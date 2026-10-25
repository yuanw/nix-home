# Home Manager side of browser-cli (config.toml + native messaging host).
# macOS: LibreWolf carries the extension (modules/browsers/browser-cli-darwin.nix).
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
  clientBin = "${micsSkills.browser-cli}/bin/browser-cli";
  wrapperPath = "${config.home.homeDirectory}/.local/bin/browser-cli-server-wrapper";

  # browser-cli derives its Unix socket from $TMPDIR, but direnv/`nix develop`
  # rewrite TMPDIR per shell, so the CLI looks in a different directory than
  # the bridge the browser spawned. get_socket_path() prefers XDG_RUNTIME_DIR,
  # so pin it for both ends. The dir must pre-exist mode 0700: only the $TMPDIR
  # branch of get_socket_path() creates it.
  runtimeDir = "${config.home.homeDirectory}/.cache/browser-cli/runtime";
  pinSocketDir = ''
    export XDG_RUNTIME_DIR="${runtimeDir}"
    mkdir -p -m 0700 "$XDG_RUNTIME_DIR"
    chmod 700 "$XDG_RUNTIME_DIR"
  '';

  librewolfCfg = osConfig.modules.browsers.librewolf or { enable = false; };
  librewolfPkg = librewolfCfg.pkg or pkgs.librewolf;

  browserPath =
    if librewolfCfg.enable or false && pkgs.stdenv.hostPlatform.isDarwin then
      librewolfCfg.darwinBrowserExe
        or "${osConfig.my.homeDirectory}/Applications/Home Manager Apps/LibreWolf.app/Contents/MacOS/librewolf"
    else
      lib.getExe librewolfPkg;

  nativeMessagingManifest = {
    name = "io.thalheim.browser_cli.bridge";
    description = "Browser CLI Bridge Server";
    path = wrapperPath;
    type = "stdio";
    allowed_extensions = [ "browser-cli-controller@thalheim.io" ];
  };

  nativeMessagingHosts =
    if pkgs.stdenv.hostPlatform.isDarwin then
      {
        "Library/Application Support/LibreWolf/NativeMessagingHosts/io.thalheim.browser_cli.bridge.json".text =
          builtins.toJSON nativeMessagingManifest;
      }
    else
      {
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
          ${pinSocketDir}
          exec ${serverBin} "$@"
        '';
      };
      # Shadows the profile's browser-cli (~/.local/bin precedes /etc/profiles
      # in $PATH) so the CLI resolves the socket the bridge is listening on.
      ".local/bin/browser-cli" = {
        executable = true;
        text = ''
          #!${pkgs.bash}/bin/bash
          ${pinSocketDir}
          exec ${clientBin} "$@"
        '';
      };
    }
    // nativeMessagingHosts;

    home.activation.clearBrowserCliDefaultsPolicy = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin (
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        /usr/bin/defaults delete org.mozilla.firefox ExtensionSettings 2>/dev/null || true
      ''
    );

    # Drop stale bridge processes left behind when the extension disconnects.
    home.activation.resetBrowserCliBridge = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ -n "''${DRY_RUN:-}" ]; then
        exit 0
      fi
      pkill -f '[b]rowser-cli-server' 2>/dev/null || true
      rm -f "${runtimeDir}/browser-cli.sock"
    '';
  };
}

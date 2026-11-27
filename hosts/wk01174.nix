{
  inputs,
  inputs',
  config,
  pkgs,
  ...
}:
{

  imports = [
    inputs.self.myModules.common
    inputs.self.myModules.darwin
    (inputs.nix-home-private + "/modules/work.nix")
  ];
  users.users.${config.my.username}.uid = 505;
  my = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "WK01174";
    gpgKey = "19AD3F6B1A5BF3BF";
    workspaceDirectory = "workspaces";
    homeDirectory = "/Users/yuanwang";
  };

  # GC / store dedup run on the Sunday schedule set in modules/macintosh.nix.
  # They were disabled here for years because the stock nix-collect-garbage and
  # nix-store --optimise froze this machine under load; if the fast ones do it
  # again, put `launchd.daemons.fast-nix-{gc,optimise}.serviceConfig.Disabled =
  # true;` back.
  #curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- repair sequoia --move-existing-users
  ids.uids.nixbld = 350;
  ids.gids.nixbld = 30000;
  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
  home-manager.users.${config.my.username} = {
    programs.git = {
      settings = {
        github.user = "yuanwang-wf";
        # url."git@github.com:".insteadOf = "https://github.com";
      };
    };
    home.packages = [
      (pkgs.writeShellScriptBin "pi-review" (builtins.readFile ../scripts/pi-review))
    ];
  };
  environment.casks = with inputs'.nix-casks.packages; [
    betterdisplay
    ungoogled-chromium
    slack
    vlc
  ];
  modules = {
    # common = {
    #   enable = true;
    #   supportLocalVirtualBuilder = true;
    # };
    rex.enable = true;
    h.enable = true;
    cursor.enable = true;
    hunk.enable = true;
    open-code-review = {
      enable = true;
      settings = {
        provider = "dgx-spark";
        custom_providers.dgx-spark = {
          url = "http://dgx-spark.local:8888/v1";
          protocol = "openai";
          model = "Qwen3.8-Flash-Next";
          api_key = "not-needed";
          timeout_sec = 600;
        };
      };
    };
    speak2text = {
      enable = false;
      flavor = "parakeet-mlx";
      parakeetServer = true; # ← enables the server
      parakeetServerPort = 5092; # ← default, optional
    };
    transcribe.enable = true; # → transcribe, yt-dlp-librewolf
    mics-skills = {
      enable = true;
      skillDirs = [
        ".pi/agent/skills"
        ".cursor/skills"
      ];
    };
    pi = {
      enable = true;
      defaultProvider = "anthropic";
      defaultModel = "claude-opus-5";
      extensions.cursor-agent.enable = true;
      extensions.subagents.enable = true;
      # Cursor's agent shares Pi's mcpServers through ~/.cursor/mcp.json.
      linkCursorMcp = true;
      rawSkills = [
        pkgs.pi-extensions.pi-interactive-shell
      ];
    };
    browsers = {
      librewolf = {
        enable = true;
        spookfox.enable = true;
      };
      ladybird.enable = true;
      defaultBrowser = "librewolf";
    };
    secrets.agenix = {
      enable = true;
    };
    neru.enable = true;
    brew = {
      enable = true;
      # taps = [ "homebrew/core" "homebrew/cask" ];
      casks = [
        "karabiner-elements"
        "viscosity"
      ];
      brews = [
        "redis"
      ];
    };
    editors.emacs = {
      enable = true;
      enableService = true;
      enableLatex = true;
    };
    # health.enable = true;
    dev = {
      dart.enable = true;
      java.enable = true;
      gcloud.enable = true;
      go.enable = true;
      playwright.enable = true;
      podman.enable = true;
      python.enable = true;
      kotlin.enable = true;
    };
    tmux = {
      enable = false;
      mainWorkspaceDir = "$HOME/workspaces";
      whichKey.enable = true;
    };
    terminal = {
      enable = true;
    };
    wm = {
      # yabai.enable = true;
      # yabai.enableJankyborders = true;
      omniwm = {
        enable = true;
        # Display UUIDs from `omniwmctl query displays --fields name,uuid`,
        # not the shared template's (it was generated on another machine).
        workspaceMonitors = [
          {
            type = "specificDisplay";
            name = "BenQ RD280U";
            uuid = "3172B827-C508-4860-8A0A-CFD93AE75BAA";
          } # 1
          {
            type = "specificDisplay";
            name = "BenQ RD280U";
            uuid = "3172B827-C508-4860-8A0A-CFD93AE75BAA";
          } # 2
          {
            type = "specificDisplay";
            name = "LG HDR 4K";
            uuid = "5DCA26DC-CFAC-4ABF-A744-20D0047BE626";
          } # 3
          {
            type = "specificDisplay";
            name = "LG HDR 4K";
            uuid = "5DCA26DC-CFAC-4ABF-A744-20D0047BE626";
          } # 4
          { type = "main"; } # 5
          { type = "main"; } # 6
          {
            type = "specificDisplay";
            name = "LG HDR 4K";
            uuid = "5DCA26DC-CFAC-4ABF-A744-20D0047BE626";
          } # 7
          {
            type = "specificDisplay";
            name = "BenQ RD280U";
            uuid = "3172B827-C508-4860-8A0A-CFD93AE75BAA";
          } # 8
          {
            type = "specificDisplay";
            name = "Built-in Retina Display";
            uuid = "37D8832A-2D66-02CA-B9F7-8F30A301B230";
          } # 9
        ];
      };
    };

    work = {
      enable = true;
      datadogMcp.enable = true;
      slackMcp.enable = true;
      includeTrio = true;
      atlassianMcp.enable = true;
      atlassianMcp.readOnly = false;
    };
  };
}

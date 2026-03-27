# Home-manager module that extends programs.claude-code with declarative plugin management.
# Plugins are fetched from GitHub at build time, then copied (not symlinked) into
# ~/.claude/plugins/ so Claude Code can write mutable state into them.
# Based on: https://github.com/tupakkatapa/nix-config/blob/76020f65/homeModules/claude-code.nix
{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.programs.claude-code;
  homeDir = config.home.homeDirectory;

  allPlugins = map (p: {
    name = p.passthru.claudePlugin.pname;
    inherit (p.passthru.claudePlugin)
      version
      rev
      id
      runtimeInputs
      activationScript
      ;
    marketplace = p.passthru.claudePlugin.marketplace.name;
    marketplaceSrc = p.passthru.claudePlugin.marketplace.src;
    marketplaceInfo = {
      inherit (p.passthru.claudePlugin.marketplace) owner repo;
    };
    pluginSrc = p;
  }) cfg.plugins;

  allRuntimeInputs = lib.unique (lib.flatten (map (p: p.runtimeInputs) allPlugins));

  allActivationScripts = lib.concatMapStrings (
    p: if p.activationScript != "" then p.activationScript + "\n" else ""
  ) allPlugins;

  uniqueMarketplaces = lib.unique (
    map (p: {
      name = p.marketplace;
      src = p.marketplaceSrc;
    }) allPlugins
  );

  pluginsDir = pkgs.runCommand "claude-plugins" { } ''
    mkdir -p $out/{cache,marketplaces}

    ${lib.concatMapStringsSep "\n" (m: ''
      mkdir -p $out/marketplaces/${m.name}
      cp -rT ${m.src} $out/marketplaces/${m.name}
    '') uniqueMarketplaces}

    ${lib.concatMapStringsSep "\n" (p: ''
      mkdir -p $out/cache/${p.marketplace}/${p.name}/${p.version}
      cp -rT ${p.pluginSrc} $out/cache/${p.marketplace}/${p.name}/${p.version}
    '') allPlugins}

    cat > $out/known_marketplaces.json << 'EOF'
    ${builtins.toJSON (
      lib.listToAttrs (
        map (
          p:
          lib.nameValuePair p.marketplace {
            source = {
              source = "github";
              repo = "${p.marketplaceInfo.owner}/${p.marketplaceInfo.repo}";
            };
            installLocation = "${homeDir}/.claude/plugins/marketplaces/${p.marketplace}";
            lastUpdated = "2025-01-01T00:00:00.000Z";
          }
        ) allPlugins
      )
    )}
    EOF

    cat > $out/installed_plugins.json << 'EOF'
    ${builtins.toJSON {
      version = 2;
      plugins = lib.listToAttrs (
        map (
          p:
          lib.nameValuePair p.id [
            {
              scope = "user";
              installPath = "${homeDir}/.claude/plugins/cache/${p.marketplace}/${p.name}/${p.version}";
              inherit (p) version rev;
              installedAt = "2025-01-01T00:00:00.000Z";
              lastUpdated = "2025-01-01T00:00:00.000Z";
              gitCommitSha = p.rev;
              isLocal = false;
            }
          ]
        ) allPlugins
      );
    }}
    EOF
  '';
in
{
  options.programs.claude-code.skillPackages = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [ ];
    description = "List of skill packages built with mkClaudeSkill. Each is linked recursively into ~/.claude/skills/<pname>/.";
  };

  config = lib.mkMerge [
    (lib.mkIf (cfg.enable && cfg.skillPackages != [ ]) {
      home.file = lib.listToAttrs (
        map (s: {
          name = ".claude/skills/${s.passthru.claudeSkill.pname}";
          value = {
            source = s;
            recursive = true;
          };
        }) cfg.skillPackages
      );
    })
    (lib.mkIf (cfg.enable && allPlugins != [ ]) {
      home.packages = allRuntimeInputs;

      programs.claude-code.settings = lib.mkMerge [
        {
          enabledPlugins = lib.listToAttrs (map (p: lib.nameValuePair p.id true) allPlugins);
        }
      ];

      home.activation.claudePlugins = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        # Remove old symlinks if they exist from a previous approach
        [ -L "${homeDir}/.claude/plugins/cache" ] && rm "${homeDir}/.claude/plugins/cache"
        [ -L "${homeDir}/.claude/plugins/marketplaces" ] && rm "${homeDir}/.claude/plugins/marketplaces"
        [ -L "${homeDir}/.claude/plugins/installed_plugins.json" ] && rm "${homeDir}/.claude/plugins/installed_plugins.json"
        [ -L "${homeDir}/.claude/plugins/known_marketplaces.json" ] && rm "${homeDir}/.claude/plugins/known_marketplaces.json"

        mkdir -p "${homeDir}/.claude/plugins"

        # Copy with write permissions — Claude Code writes state into plugin dirs
        ${pkgs.rsync}/bin/rsync -a --chmod=u+w "${pluginsDir}/cache" "${homeDir}/.claude/plugins/"
        ${pkgs.rsync}/bin/rsync -a --chmod=u+w "${pluginsDir}/marketplaces" "${homeDir}/.claude/plugins/"

        cp -f "${pluginsDir}/installed_plugins.json" "${homeDir}/.claude/plugins/"
        cp -f "${pluginsDir}/known_marketplaces.json" "${homeDir}/.claude/plugins/"
        chmod u+w "${homeDir}/.claude/plugins/installed_plugins.json"
        chmod u+w "${homeDir}/.claude/plugins/known_marketplaces.json"

        ${allActivationScripts}
      '';
    })
  ];
}

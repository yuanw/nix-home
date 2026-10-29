{
  config,
  inputs,
  inputs',
  lib,
  ...
}:

with lib;
let
  cfg = config.modules.neru;
  # Where the signed bundle lives. nix-darwin already mirrors it there: the
  # upstream darwin module puts the package in environment.systemPackages and
  # the `applications` activation script rsyncs every /Applications in it into
  # this directory. The store is read-only, so this is the only copy codesign
  # can write to - and it is the same bundle launchd runs, at a path that
  # never changes.
  signedApp = "/Applications/Nix Apps/Neru.app";
in
{
  imports = [ inputs.neru.darwinModules.default ];

  options.modules.neru = {
    enable = mkEnableOption "neru";

    signingIdentity = mkOption {
      type = types.str;
      default = "Nix Self-Signed Codesign";
      description = ''
        Codesigning identity used to re-sign Neru.app so that its designated
        requirement is bound to a certificate instead of a cdhash. Set to ""
        to install Neru unsigned-and-adhoc, which means macOS drops the
        Accessibility grant on every rebuild and re-prompts. Create the
        identity once with: just neru-selfsign-cert
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.modules.mouseless.enable;
        message = "modules.neru and modules.mouseless cannot both be enabled on the same host";
      }
    ];

    services.neru = {
      enable = true;
      package = inputs'.neru.packages.default;
      configFile = "${config.my.homeDirectory}/.config/neru/config.toml";
    };

    # The shipped bundle is ad-hoc *linker* signed and carries no designated
    # requirement, so `codesign` synthesises `designated => cdhash H"..."`.
    # TCC stores that blob as the grant's csreq, so every rebuild - which also
    # rewrites store paths inside the Mach-O - changes the cdhash, the stored
    # requirement stops matching, and Accessibility has to be re-granted by
    # hand. Re-signing with a stable self-signed identity yields
    # `designated => identifier "com.y3owk1n.neru" and certificate root =
    # H"<cert>"`, which outlives rebuilds. It also fixes the currently broken
    # seal (`codesign --verify` fails on the shipped app: Sealed Resources
    # none) and the signature identifier, which says `neru` while the bundle
    # id is `com.y3owk1n.neru`.
    #
    # postActivation, not a named script of our own: nix-darwin executes only
    # the hard-coded list in modules/system/activation-scripts.nix, so
    # `system.activationScripts.neruCodesign` would be accepted and never run
    # (which is why the yabaiTCCProfile script in modules/wm/yabai.nix is dead
    # code today). It also has to run *after* the `applications` script, which
    # rsyncs --delete over anything we sign here, and after `userLaunchd`, so
    # the kickstart below restarts the agent onto the signed binary.
    #
    # A stable launch path means the plist stops changing between rebuilds and
    # nix-darwin only reloads a user agent when its plist differs
    # (modules/system/launchd.nix: `if ! diff ...`), hence the explicit
    # kickstart: without it a neru version bump would not take effect until
    # the next login.
    system.activationScripts.postActivation.text = mkIf (cfg.signingIdentity != "") (mkAfter ''
      echo "re-signing Neru.app for a stable Accessibility grant..."
      KC=/Library/Keychains/nix-selfsign.keychain-db
      APP='${signedApp}'
      # Both domains: `security find-identity' and codesign consult the invoking
      # user's (here root's) user-domain search list, and -d system alone leaves
      # the identity invisible -- find-identity reports 0 valid identities.
      security list-keychains -d system -s "$KC" /Library/Keychains/System.keychain
      security list-keychains -s "$KC" /Library/Keychains/System.keychain
      # Checked before unlocking: a missing keychain has to report itself with
      # this hint rather than as a SecKeychainUnlock error.
      if ! security find-identity -v -p codesigning | grep -qF '${cfg.signingIdentity}'; then
        echo "ERROR: codesigning identity '${cfg.signingIdentity}' not found in $KC" >&2
        echo "       run 'just neru-selfsign-cert', or set modules.neru.signingIdentity to \"\"" >&2
        exit 1
      fi
      # No `|| true`: a locked keychain makes codesign wait on a GUI prompt
      # that root cannot answer.
      security unlock-keychain -p "" "$KC"
      # nix-darwin rsyncs the bundle with --chmod=-w, and codesign cannot
      # replace Contents/_CodeSignature through a read-only bundle: it leaves
      # the old ad-hoc signature in place and the verify below fails.
      chmod -R u+w "$APP"
      codesign --force --deep --sign '${cfg.signingIdentity}' \
        --entitlements "$APP/Contents/Resources/Neru.entitlements" \
        --options runtime --timestamp=none "$APP"
      codesign --verify --verbose=2 "$APP"
      uid="$(id -u '${config.my.username}')"
      launchctl kickstart -k "gui/$uid/org.nixos.neru" || true
    '');

    # Launch the signed copy, not the store copy, or TCC sees the ad-hoc
    # binary again and the grant is useless. nix-darwin pastes `command` into
    # `sh -c '... exec <command>'`, so the path needs its own quotes: the
    # directory name contains a space.
    launchd.user.agents.neru.command = mkIf (cfg.signingIdentity != "") (
      mkForce ''"${signedApp}/Contents/MacOS/neru" launch --config "${config.services.neru.configFile}"''
    );

    home-manager.users.${config.my.username} =
      hm@{ ... }:
      {
        xdg.configFile."neru/config.toml".source =
          hm.config.lib.file.mkOutOfStoreSymlink "${config.my.homeDirectory}/${config.my.workspaceDirectory}/nix-home/modules/neru/config.toml";
      };
  };
}

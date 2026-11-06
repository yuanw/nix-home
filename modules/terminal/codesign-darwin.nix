{ config, lib, ... }:

with lib;
let
  identity = config.modules.neru.signingIdentity;
in
{
  # Alacritty loses its Local Network grant on rebuilds for the same reason
  # Neru lost Accessibility: the shipped bundle is ad-hoc *linker* signed, so
  # codesign synthesises `designated => cdhash H"..."`, TCC pins the grant to
  # that cdhash, and any rebuild touching alacritty's inputs mints a new one
  # (three store copies of alacritty-0.17.0, three cdhashes, were live at once
  # on mist). Signing binds the requirement to the local certificate instead:
  # `designated => identifier "org.alacritty" and certificate root = H"..."`.
  #
  # Signs the copy LaunchServices actually knows about -- `lsregister -dump`
  # lists only the Home Manager Apps one, and it is the path the yabai keybind
  # opens (modules/wm/yabai.nix). The store copy is read-only and unused.
  #
  # Reuses modules.neru.signingIdentity: one identity, one keychain, and its
  # documented `signingIdentity = ""` opt-out covers this too. Unlike the Neru
  # block, a missing identity warns instead of aborting -- alacritty staying
  # ad-hoc costs a re-prompt, a failed activation costs the whole update.
  #
  # No --options runtime, no --entitlements, no --deep: alacritty ships none of
  # those and the bundle holds a single Mach-O. --identifier because the
  # shipped signature says `alacritty` while the TCC client is the bundle id
  # `org.alacritty`.
  #
  # Darwin-only because NixOS has system.activationScripts too, but there an
  # option merely *declared* without a value breaks the activation script
  # ("accessed but has no value defined") -- asche failed to eval that way.
  #
  # postActivation + mkAfter because nix-darwin runs only the hard-coded script
  # names, and this must be the last writer: Home Manager rsyncs the app copy
  # out of the store during activation and would clobber the signature.
  config = mkIf (config.modules.terminal.enable && identity != "") {
    system.activationScripts.postActivation.text = mkAfter ''
      echo "re-signing Alacritty.app for a stable Local Network grant..."
      KC=/Library/Keychains/nix-selfsign.keychain-db
      APP='${config.my.homeDirectory}/Applications/Home Manager Apps/Alacritty.app'
      # Both domains: codesign and find-identity consult the invoking user's
      # user-domain search list, so -d system alone reports 0 identities.
      security list-keychains -d system -s "$KC" /Library/Keychains/System.keychain
      security list-keychains -s "$KC" /Library/Keychains/System.keychain
      if [ ! -d "$APP" ]; then
        echo "  skipped: $APP not installed"
      elif ! security find-identity -v -p codesigning | grep -qF '${identity}'; then
        echo "WARN: identity '${identity}' not in $KC" >&2
        echo "      run 'just neru-selfsign-cert'; Alacritty stays ad-hoc and its" >&2
        echo "      Local Network grant will keep dying on rebuild." >&2
      else
        # No `|| true`: a locked keychain makes codesign wait on a GUI prompt
        # that root cannot answer.
        security unlock-keychain -p "" "$KC"
        codesign --force --sign '${identity}' \
          --identifier org.alacritty "$APP" \
          || echo "WARN: codesign of Alacritty.app failed" >&2
        codesign --verify --verbose=2 "$APP" \
          || echo "WARN: Alacritty.app does not verify" >&2
      fi
    '';
  };
}

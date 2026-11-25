# skhd chords that launch apps rather than drive a layout. Both window managers
# (yabai, OmniWM) read this, so the launcher keys survive a swap; each WM module
# adds its own reload binding on top.
#
# Not a module: it is imported as a plain function by the WM modules and used
# only while one of them is enabled.
{ config, pkgs }:
let
  emacsEveryWhere = ''emacsclient --eval "(emacs-everywhere)"'';
  emacsClient = "emacsclient -c -a 'emacs'";
  defaultBrowserCmd = config.modules.browsers.darwinLaunchCmd;
  # LaunchServices does not reliably know the Home Manager bundles by name, so
  # `open -a' takes the path (same reason as modules/editor/emacs/features/postlude.el).
  rexApp = "~/Applications/Home\\ Manager\\ Apps/Rex.app";
in
{
  config = ''
    shift + ctrl + alt - e: ${emacsClient}
    shift + ctrl + alt + cmd - e: ${emacsEveryWhere}
    shift + ctrl + alt - o: org-capture
    shift + ctrl + alt - f :${defaultBrowserCmd}
    shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'
    # lock screen
    shift + ctrl + alt - l: pmset displaysleepnow
    # bring up Rex (sessions live in the rex server, so this only needs to wake the app)
    shift + ctrl + alt - h: open -a ${rexApp}
    shift + ctrl + alt - y : choose-pass
    # take screenshot
    shift + ctrl + alt - s: screencapture -ic
    shift + ctrl + alt - i: screencapture -i /tmp/$(date +%s).png
    shift + ctrl + alt - c: screencapture /tmp/$(date +%s).png
    cmd - space: app-launcher
  '';

  # choose-pass reads the store through choose; app-launcher comes from
  # modules/editor/emacs/lib.nix.
  packages = [
    pkgs.choose-mac
    (pkgs.writeShellScriptBin "choose-pass" ''
      find $HOME/.password-store -type f -name '*.gpg' | \
         sed "s|.*/\.password-store/||; s|\.gpg$||" | \
         choose | \
         xargs -r -I{} pass show -c {}
    '')
  ];
}

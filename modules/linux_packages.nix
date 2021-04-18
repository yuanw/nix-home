{ pkgs, ... }:

with pkgs; [
  _1password-gui
  stretchly
  pinentry
  stalonetray
  betterlockscreen
  killall
  haskellPackages.xmobar
  gxmessage
  xmessage
  autorandr
]

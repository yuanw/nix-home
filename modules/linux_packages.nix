{ pkgs, ... }:

with pkgs;
[
  gnumake
  #proton-vpn-cli
  _1password-gui
  _1password-cli
  #protonvpn-gui
  # 'pinentry' has been removed. Pick an appropriate variant like 'pinentry-curses' or 'pinentry-gnome3'
  pinentry-gnome3

  stalonetray
  killall
  gxmessage
  autorandr
  dzen2
  pavucontrol
  scrot
  xdotool
  xorg.xwininfo
  find-cursor
  wifish
  mpv-unwrapped
  brightnessctl
]

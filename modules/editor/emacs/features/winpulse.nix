{
  epkgs = epkgs: [ epkgs.winpulse ];
  elisp = ''
    (use-package winpulse
      :commands (winpulse-momentary-highlight-one-window
                 winpulse-momentary-highlight-other-windows))
  '';
}

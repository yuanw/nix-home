{ lib, pkgs, ... }:

{
  epkgs = epkgs: [ epkgs.auto-dark ];
  elisp = ''
    (use-package auto-dark
      :after ef-themes
      :init
      ;; `modus-themes-to-toggle' is not available unless modus-themes is
      ;; loaded.  We use Ef themes here, so configure Auto Dark explicitly:
      ;; first list is dark themes, second list is light themes.
      (setq auto-dark-themes '((ef-owl) (ef-spring)))
      :config
      ${lib.optionalString pkgs.stdenv.hostPlatform.isDarwin "(setq auto-dark-allow-osascript t)"}
      ;; Faces are global, so Auto Dark can only be gated per Emacs instance,
      ;; not per frame: a terminal-only `emacs -nw' starts unthemed; the daemon
      ;; (which also serves `emacsclient -nw') and GUI frames keep it.
      ;; shortcut: a tty-only instance that later opens a GUI frame stays
      ;; unthemed.
      (when (or (daemonp) (display-graphic-p))
        (auto-dark-mode 1)))
  '';
}

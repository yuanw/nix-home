{ pkgs, ... }: {
  # The activation script needs jq to parse the JXA JSON output.
  environment.systemPackages = [ pkgs.jq ];

  environment.loginItems = {
    enable = true;
    items = [
      "/Applications/Nix Casks/BetterDisplay.app"
      "/Applications/Nix Casks/Mouseless.app"
      "/Applications/Fresh Eyes.app"
    ];
  };
}

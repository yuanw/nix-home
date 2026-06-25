# Enterprise policies — browser-cli extension force-install (Firefox / LibreWolf).
# https://github.com/Mic92/dotfiles/blob/6040591/pkgs/librewolf-policies.nix
{
  browser-cli-extension,
  ...
}:
{
  ExtensionSettings = {
    "browser-cli-controller@thalheim.io" = {
      installation_mode = "force_installed";
      install_url = "file://${browser-cli-extension}/browser-cli-extension.xpi";
    };
  };
}

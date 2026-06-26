# Enterprise policies — browser-cli extension force-install (Firefox / LibreWolf).
# https://github.com/Mic92/dotfiles/blob/6040591/pkgs/librewolf-policies.nix
{
  browser-cli-extension,
  installUrl ? "file://${browser-cli-extension}/browser-cli-extension.xpi",
  installSource ? null,
  ...
}:
let
  sourcePrefix =
    if installSource != null then
      installSource
    else
      let
        parts = builtins.match "(.*)/[^/]*$" installUrl;
      in
      if parts == null then installUrl else builtins.elemAt parts 0;
in
{
  ExtensionSettings = {
    "browser-cli-controller@thalheim.io" = {
      installation_mode = "force_installed";
      install_url = installUrl;
      install_sources = [ "${sourcePrefix}/" ];
    };
  };
  Preferences = {
    # Required for unsigned local XPI on release Firefox (ERROR_SIGNEDSTATE_REQUIRED).
    "xpinstall.signatures.required" = false;
    "extensions.autoDisableScopes" = 0;
  };
}

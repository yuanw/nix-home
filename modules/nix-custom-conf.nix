{
  config,
  lib,
  ...
}:
let
  # silver's LAN cache; my.silver-cache.enable is false on hosts that travel
  # (asche).  Read from the options rather than imported as constants so the
  # url and the key cannot drift apart from what misfit/dgx-spark get.
  silverCache = config.my.silver-cache;
  baseSubstituters = [
    "https://cache.nixos.org"
    "https://nix-community.cachix.org"
    "https://yuanw-nix-home-macos.cachix.org"
    "https://cachix.org/api/v1/cache/yuanwang-wf"
    "https://cachix.org/api/v1/cache/devenv"
    "https://cache.iog.io"
    "https://numtide.cachix.org"
    "https://cache.numtide.com"
    "https://cache.zw3rk.com"
  ];
  # Silver goes first, but its position is cosmetic: harmonia serves at
  # priority 10 and cache.nixos.org at 40, so it wins wherever it is listed.
  cacheSubstituters = lib.optionals silverCache.enable [ silverCache.url ] ++ baseSubstituters;
  trustedPublicKeys = lib.optionals silverCache.enable [ silverCache.publicKey ] ++ [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80="
    "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    "yuanw-nix-home-macos.cachix.org-1:6sDjrV0jQY6kRgXjXe0feuDtsxnoGDnkgvXuKma5JcQ="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
    "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    # https://cache.zw3rk.com
    "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
  ];

  # Silver is a substituter but deliberately NOT a trusted-substituter: every
  # narinfo harmonia emits is signed with silver-1, which is in the list above,
  # so require-sigs passes without it.  Making it trusted would waive signature
  # checking for an unauthenticated HTTP endpoint that anyone on the LAN can
  # stand up in silver's place.
  trustedSubstituters = baseSubstituters;
  trustedUsers = [
    "root"
    config.my.username
  ];

  toLine = key: values: "${key} = ${lib.concatStringsSep " " values}";
in
{
  imports = [ ./services/cache/consumer.nix ];

  # nix-darwin appends https://cache.nixos.org/ via mkAfter by default; mkForce
  # keeps our explicit lists without duplicates.
  nix.settings = {
    trusted-users = lib.mkForce trustedUsers;
    substituters = lib.mkForce cacheSubstituters;
    trusted-substituters = lib.mkForce trustedSubstituters;
    trusted-public-keys = lib.mkForce trustedPublicKeys;
  };

  # Lix/Determinate installs on Darwin include /etc/nix/nix.custom.conf from
  # /etc/nix/nix.conf. Keep the same cache settings there for hosts where the
  # daemon config is not owned by nix-darwin.
  environment.etc."nix/nix.custom.conf".text = ''
    ${toLine "trusted-users" trustedUsers}
    ${toLine "substituters" cacheSubstituters}
    ${toLine "trusted-substituters" trustedSubstituters}
    ${toLine "trusted-public-keys" trustedPublicKeys}
    # A powered-off silver must not stall every missing path for a minute. It is
    # also Nix's default, spelled out because on the Darwin hosts this file,
    # not nix.settings, is what the daemon reads (nix.enable = false there).
    connect-timeout = 5
  '';
}

{
  config,
  lib,
  ...
}:
let
  cacheSubstituters = [
    "https://cache.nixos.org"
    "https://nix-community.cachix.org"
    "https://yuanw-nix-home-macos.cachix.org"
    "https://cachix.org/api/v1/cache/yuanwang-wf"
    "https://cachix.org/api/v1/cache/devenv"
    "https://cache.iog.io"
    "https://numtide.cachix.org"
    "https://cache.numtide.com"
  ];
  extraTrustedSubstituters = [
    "https://cache.garnix.io"
    "https://cache.zw3rk.com"
  ];
  trustedPublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80="
    "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    "yuanw-nix-home-macos.cachix.org-1:6sDjrV0jQY6kRgXjXe0feuDtsxnoGDnkgvXuKma5JcQ="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
    "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
  ];

  toLine = key: values: "${key} = ${lib.concatStringsSep " " values}";
in
{
  environment.etc."nix/nix.custom.conf".text = ''
    trusted-users = root ${config.my.username}
    ${toLine "substituters" cacheSubstituters}
    ${toLine "trusted-substituters" (cacheSubstituters ++ extraTrustedSubstituters)}
    ${toLine "trusted-public-keys" trustedPublicKeys}
  '';
}

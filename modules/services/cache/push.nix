{ lib, pkgs, ... }:
let
  # A Cachix push token for `dgx-spark-nixos`, the cache this host's own CUDA
  # outputs are published to (see `just cache-push`).  The token lives in pass at
  # cachix/dgx-spark-nixos; encrypt it for silver with:
  #     pass show cachix/dgx-spark-nixos | head -1 \
  #         | AGENIX_RULES=secrets/secrets.nix agenix -e secrets/cachix-auth-token.age
  # (the recipient rule is already in secrets/secrets.nix, silver only).
  #
  # Gated on the file existing instead of an option: the repo stays deployable
  # before the token exists, and silver hands it to `just cache-push` the moment
  # it does.
  secret = ../../../secrets/cachix-auth-token.age;
in
{
  config = lib.mkIf (builtins.pathExists secret) {
    # agenix's default 0400 root:root — only the root shell `just cache-push`
    # spawns on silver reads it, and yuanw cannot.
    age.secrets.cachix-auth-token.file = secret;

    # `nix copy --to https://<cache>.cachix.org` answers "Method not supported"
    # (measured): the binary-cache HTTP protocol has no upload path, pushing is
    # the CLI's job.  Installed here so `just cache-push` has it on silver.
    environment.systemPackages = [ pkgs.cachix ];
  };
}

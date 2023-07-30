{
  perSystem = { config, pkgs, ... }: {
    devshells.default.packages = [
      config.treefmt.build.wrapper
    ];
  };
}

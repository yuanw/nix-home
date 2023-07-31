{
  perSystem = { config, ... }: {
    devshells.default.packages = [
      config.treefmt.build.wrapper
    ];
  };
}

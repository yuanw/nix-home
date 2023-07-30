{
  perSystem = { config, ... }: {
    devenv.shells.default = {
      # https://devenv.sh/reference/options/
      packages = [
        config.treefmt.build.wrapper
      ];
    };
  };
}

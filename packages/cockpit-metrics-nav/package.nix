{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "cockpit-metrics-nav";
  version = "1.0.0";

  src = ./.;

  installPhase = ''
    mkdir -p $out/share/cockpit/metrics-nav
    cp manifest.json $out/share/cockpit/metrics-nav/
    cp index.html $out/share/cockpit/metrics-nav/
    cp metrics-icon.svg $out/share/cockpit/metrics-nav/
  '';

  meta = {
    description = "Cockpit plugin to add Performance navigation entry pointing to PCP metrics";
    license = pkgs.lib.licenses.mit;
    platform = "all";
  };
}

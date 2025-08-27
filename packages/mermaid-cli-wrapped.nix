#https://github.com/fred-drake/nix/blob/b921f54f477028814a685374ab10f4248fe59080/apps/mermaid-cli-wrapped.nix
{
  pkgs,
  stdenv,
  ...
}:
let
  # Platform-specific Chrome/Chromium configuration
  browserConfig =
    if stdenv.isDarwin then
      {
        # On macOS, use the system-installed Chrome
        executablePath = "/Applications/Chromium.app/Contents/MacOS/Chromium";
      }
    else
      {
        # On Linux, use Chromium from nixpkgs
        executablePath = "${pkgs.chromium}/bin/chromium";
      };

  puppeteerConfig = pkgs.writeText "puppeteer-config.json" (
    builtins.toJSON {
      inherit (browserConfig) executablePath;
      # Additional args for headless operation
      args = [
        "--no-sandbox"
        "--disable-setuid-sandbox"
      ];
    }
  );
in
pkgs.symlinkJoin {
  name = "mermaid-cli-wrapped";
  paths = [ pkgs.mermaid-cli ];
  buildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/mmdc \
      --add-flags "--puppeteerConfigFile ${puppeteerConfig}" \
      --set PUPPETEER_SKIP_CHROMIUM_DOWNLOAD 1 \
      --set PUPPETEER_EXECUTABLE_PATH "${browserConfig.executablePath}"
  '';
  meta = pkgs.mermaid-cli.meta // {
    description = "Mermaid CLI wrapped with platform-appropriate Chrome/Chromium";
  };
}

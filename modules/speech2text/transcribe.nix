# Media -> text transcription: `transcribe` (yt-dlp captions, whisper-cpp
# fallback) and `yt-dlp-librewolf` (yt-dlp fed with the LibreWolf profile's
# cookies, the workaround from yt-dlp/yt-dlp#14050).
#
# The scripts read their knobs from the environment, so everything below is
# just sessionVariables plumbing plus putting the two scripts on PATH.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.transcribe;

  # Built once, in the packages overlay, so `nix build
  # .#packages.aarch64-darwin.transcribe` and this module cannot drift apart.
  scripts = {
    transcribe = pkgs.transcribe;
    yt-dlp-librewolf = pkgs.yt-dlp-librewolf;
  };

  user = config.my.username;
in
{
  options.modules.transcribe = {
    enable = lib.mkEnableOption "media transcription (transcribe, yt-dlp-librewolf)";

    subLangs = lib.mkOption {
      type = lib.types.str;
      default = "en.,en";
      description = "Value for yt-dlp --sub-langs; 'en.,en' prefers auto-generated English captions.";
    };

    cookieBrowser = lib.mkOption {
      type = lib.types.str;
      default = "librewolf";
      description = ''"librewolf", an explicit profile root path, or "" to run without cookies.'';
    };

    librewolfProfileRoot = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''Absolute path to the LibreWolf profile root (the directory holding profiles.ini). "" means the platform default.'';
    };

    asrCmd = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''Speech-to-text fallback, run as "$asrCmd FILE" and expected to print text. "" means the bundled whisper-cpp with $WHISPER_MODEL.'';
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user}.home = {
      packages = [
        scripts.transcribe
        scripts.yt-dlp-librewolf
      ];

      sessionVariables = {
        TRANSCRIBE_SUB_LANGS = lib.mkDefault cfg.subLangs;
        COOKIE_BROWSER = lib.mkDefault cfg.cookieBrowser;
        LIBREWOLF_PROFILE_ROOT = lib.mkDefault cfg.librewolfProfileRoot;
        TRANSCRIBE_ASR_CMD = lib.mkDefault cfg.asrCmd;
      };
    };
  };
}

# The deployment repo's OpenAI /v1 server, installed as-is from
# server/*.py:
#
#   serve_glm.py     stdlib ThreadingHTTPServer (the image pip-installed
#                    fastapi/uvicorn, but this entry point imports
#                    neither), one generation worker thread.
#   protocol.py      request normalization + jsonschema tool-schema
#                    validation.
#   glm_protocol.py  GLM-5.3 chat format and tool-call parser, new vs
#                    Vic's MiMo recipe (that's the repo's whole point).
#   worker.py        the single native Generator owner thread.
#
# python puts the script's directory on sys.path, so the sibling imports
# resolve from wherever the .py files land (see modules/red-snow.nix,
# which execs serve_glm.py through the CUDA-enabled python313 env).
{
  lib,
  stdenv,
  deploySrc,
}:

stdenv.mkDerivation {
  pname = "redsnow-server";
  version = "b96fac2";

  src = deploySrc;

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -d $out/share/redsnow-server
    install -m 0444 server/*.py $out/share/redsnow-server/
    runHook postInstall
  '';

  meta = {
    description = "RED-SNOW-5.3-Flash 2.49bpw /v1 server (GLM protocol on exllamav3) from the one-DGX-Spark deployment repo";
    homepage = "https://github.com/Weschera/RED-SNOW-5.3-Flash-2.49bpw-1x-DGX-Spark";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}

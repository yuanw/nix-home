# The deployment repo's regression tools (tools/bench.py, tools/needle.py):
# stdlib-only scripts that hit the TensorFold OpenAI-compatible API. They
# measured the container baseline (62/90/107/119 tok/s at 1/2/4/5 streams,
# see the deployment repo's README) and serve as the regression gate for
# the native service. Scripts are copied as-is (bench.py also backs
# needle.py's `from bench import …`), wrapped with the plain python3.
{
  lib,
  stdenv,
  python3,
  makeBinaryWrapper,
  deploySrc,
}:

stdenv.mkDerivation {
  pname = "tensorfold-tools";
  version = "0.3.6.3";

  src = deploySrc;

  nativeBuildInputs = [ makeBinaryWrapper ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -d $out/bin $out/share/tensorfold-tools
    for tool in bench.py needle.py toolcheck.py visioncheck.py; do
      install -m 0555 tools/$tool $out/share/tensorfold-tools/$tool
      binName=$(basename $tool .py)
      makeWrapper ${python3.interpreter} $out/bin/$binName \
        --add-flags "$out/share/tensorfold-tools/$tool"
    done
    runHook postInstall
  '';

  # No PYTHONPATH needed: python puts the script's directory
  # ($out/share/tensorfold-tools) on sys.path, so needle.py/toolcheck.py's
  # `from bench import …` resolves against the installed bench.py.

  meta = with lib; {
    description = "TensorFold regression tools (bench/needle) from the Qwen3.8 DGX Spark deployment repo";
    homepage = "https://github.com/yuanw/Qwen3.8-Flash-Next-Single-DGX-Spark-TensorFold";
    mainProgram = "bench";
    platforms = platforms.unix;
  };
}

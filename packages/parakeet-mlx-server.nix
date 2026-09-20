{
  lib,
  writers,
  runCommand,
  parakeet-mlx,
  ffmpeg,
  makeWrapper,
}:

let
  pythonScript = writers.writePython3Bin "parakeet-mlx-server" {
    libraries = [ parakeet-mlx ];
    flakeIgnore = [
      "E501"
      "E402"
      "E265"
      "W503"
    ];
  } ./parakeet-mlx-server.py;
in

runCommand "parakeet-mlx-server"
  {
    nativeBuildInputs = [ makeWrapper ];
    meta = with lib; {
      description = "Persistent HTTP server for parakeet-mlx speech-to-text";
      platforms = [ "aarch64-darwin" ];
    };
  }
  ''
    mkdir -p $out/bin
    makeWrapper ${pythonScript}/bin/parakeet-mlx-server $out/bin/parakeet-mlx-server \
      --prefix PATH : ${lib.makeBinPath [ ffmpeg ]}
  ''

{
  lib,
  writeShellApplication,
  curl,
  ffmpeg,
  python3,
}:

writeShellApplication {
  name = "parakeet-mlx-server-test";

  runtimeInputs = [
    curl
    ffmpeg
    python3
  ];

  text = builtins.readFile ./parakeet-mlx-server-test.sh;

  meta = with lib; {
    description = "End-to-end test for parakeet-mlx-server";
    platforms = [
      "aarch64-darwin"
      "x86_64-linux"
    ];
  };
}

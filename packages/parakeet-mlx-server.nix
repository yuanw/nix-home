{
  writers,
  parakeet-mlx,
}:

writers.writePython3Bin "parakeet-mlx-server" {
  libraries = [ parakeet-mlx ];
  flakeIgnore = [
    "E501"
    "E402"
    "E265"
    "W503"
  ];
} ./parakeet-mlx-server.py

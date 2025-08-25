{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.4.2-unstable-2025-07-25";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "8c92a17743af05fedc76beeb58da5eab48398035";
    sha256 = "sha256-UzFH+LXZ1ui9Wh9mlRYcZcpLBx0nSzNTtKGB8JI0r9Q=";
  };
}

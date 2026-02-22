{
  melpaBuild,
  fetchFromGitHub, # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "winpulse";
  version = "0-unstable-2025-02-14";
  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "winpulse";
    rev = "7d748b6a0450b60de30d143371f58631670851b9";
    sha256 = "sha256-+J7H4Q8eAhzibDyfnhnKJuYih3etc+Ixfs6og27QP4I=";
  };
}

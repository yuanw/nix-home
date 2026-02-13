{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.5-unstable-2026-01-18";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "08758c6772c5fbce54fb74fb5cce080b6425c6ce";
    sha256 = "sha256-hKgwjs4qZikbvHKjWIJFlkI/4LXR6qovCoTBM5miVr8=";
  };
}

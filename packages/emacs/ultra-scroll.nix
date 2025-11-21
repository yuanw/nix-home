{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.4.2-unstable-2025-11-09";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "19fd73e0ee71558c7770d8daecdbe1793d54c9ae";
    sha256 = "sha256-8Qu7Wgb/LFMWp/8WropgYh40QPG78VxT1HFHc1Kxsj4=";
  };
}

{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.7-unstable-2026-08-10";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "0222f429955f5a2a3810f3c84d59ca441aa16eb2";
    sha256 = "sha256-h9yKFnMrPFF7ZaEEqwDVQWGrrZQX69awG2WsGEwqUdk=";
  };
}

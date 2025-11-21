{
  melpaBuild,
  fetchFromGitHub,
  # Elisp dependencies
  gptel ? null,
  yaml ? null,
  orderless ? null,
  ...
}:
melpaBuild {
  pname = "gptel-agent";
  version = "0-unstable-2025-11-19";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "e8266da2c8923bf270127a757d8d787a0f8e7302";
    sha256 = "sha256-3/xew5u+7i5ipe3UyLNVHrciv0Vh6nbopvxcQFnzjk4=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}

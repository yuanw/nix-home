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
  version = "0-unstable-2026-02-21";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "79803c50efbcbdbf9a5ceba07fb99054da2f9e15";
    sha256 = "sha256-H9sUMbS0YDz3YQeF/qbxX88ifM//1IX56L6WbVwIwq4=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}

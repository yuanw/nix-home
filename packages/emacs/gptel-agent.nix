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
  version = "0-unstable-2026-05-23";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "f8cab0368918672a329ea3caf5cd64b6db1722eb";
    sha256 = "sha256-8FXM0NaYbWxWdAJ2b0drvQQUYSfUiQFLDgWuajB1+Zo=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}

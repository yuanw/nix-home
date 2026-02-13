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
  version = "0-unstable-2026-02-13";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "8ba9056da2341468192e6417d47cb50e26636e97";
    sha256 = "sha256-M2J/K3UHoAbDWQjYPD8ZdL6uHBggvPh+ZvJ+xnbXJuo=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}

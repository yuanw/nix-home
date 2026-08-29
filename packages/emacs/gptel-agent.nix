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
  version = "0-unstable-2026-08-14";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "7a74b695909c0e3304eb8cbd583a1f53e0802c60";
    sha256 = "sha256-yc7zG62lh9qS3LpMSRh+mdrQzI5yMZx7Uv9ZQlzNFg0=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}

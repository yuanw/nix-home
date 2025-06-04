{
  buildPythonApplication,
  fetchFromGitHub,
  callPackage,
  setuptools,
  poetry-core,
  lib,

  demjson3,
  mutagen,
  requests,
  unicode-slugify,
  urllib3,
}:
let
  beautifulsoup4 = callPackage ./beautifulsoup4.nix { };
in
buildPythonApplication rec {
  pname = "bandcamp-dl";
  version = "0.0.17";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "iheanyi";
    repo = pname;
    #rev = "d7b4c4d6e7bfe365ee36514d6c608caf883e4476";
    rev = "v${version}";
    # hash = lib.fakeHash;

    hash = "sha256-PNyVEzwRMXE0AtTTg+JyWw6+FSuxobi3orXuxkG0kxw=";
  };

  build-system = [ poetry-core ];

  propagatedBuildInputs = [
    setuptools
    beautifulsoup4
    demjson3
    mutagen
    requests
    unicode-slugify
    urllib3
  ];

  #  doCheck = false;

  meta = {
    homepage = "https://github.com/iheanyi/bandcamp-dl";
    description = "Download audio from bandcamp.com";
    license = lib.licenses.publicDomain;
  };
}

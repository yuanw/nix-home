{
  buildPythonApplication,
  fetchFromGitHub,
  callPackage,
  setuptools,
  lib,

  beautifulsoup4,
  demjson3,
  mutagen,
  requests,
  requests-ratelimiter,
  urllib3,
}:
let
  # nixpkgs dropped unicode-slugify; see ./unicode-slugify.nix
  unicode-slugify = callPackage ./unicode-slugify.nix { };
in
buildPythonApplication rec {
  pname = "bandcamp-dl";
  # no tag for it -- master of the maintained fork
  version = "1.0.0.dev1";
  pyproject = true;

  src = fetchFromGitHub {
    # iheanyi/bandcamp-dl, this package's original home, has gone stale;
    # Evolution0's fork is where the Bandcamp-scraping fixes land.
    owner = "Evolution0";
    repo = pname;
    rev = "0bf086bc2431c0756630faafff10dee7b64b8b67";

    hash = "sha256-QzZl10YHPvJPH7ufSJxF2xhNya2khgbhN93OFJAEUFg=";
  };

  # nixpkgs has urllib3 2.7.0, the pyproject floor is 2.8.0; nothing here uses
  # what the extra point added.
  pythonRelaxDeps = [ "urllib3" ];

  build-system = [ setuptools ];

  # pyproject asks for setuptools>=84, nixpkgs has 83, and the check inside
  # setuptools' build_meta kills the build on the pin.  The pin is about
  # metadata handling, not about what this build needs, so drop the constraint
  # rather than vendor a newer setuptools.
  postPatch = ''
    sed -i 's/setuptools>=84\.0\.0/setuptools/' pyproject.toml
  '';

  dependencies = [
    beautifulsoup4
    demjson3
    mutagen
    requests
    requests-ratelimiter
    unicode-slugify
    urllib3
  ];

  meta = {
    homepage = "https://github.com/Evolution0/bandcamp-dl";
    description = "Download audio from bandcamp.com";
    license = lib.licenses.unlicense;
  };
}

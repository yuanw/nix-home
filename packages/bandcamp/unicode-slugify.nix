# Vendored: nixpkgs removed unicode-slugify ("broken and unmaintained", 2026)
# and points at python-slugify instead, but bandcamp-dl calls
# `slugify(content, ok=, only_ascii=, spaces=, lower=, space_replacement=)`.
# Those kwargs only exist in unicode-slugify's API, so the suggested swap dies
# with a TypeError on the first download.  Pin the last release (0.1.5, also
# what nixpkgs carried) here instead -- it is what upstream pins too, modulo
# their PR #41 (a Python 3.12 SyntaxWarning fix).
{
  lib,
  buildPythonPackage,
  fetchPypi,
  six,
  unidecode,
}:
buildPythonPackage rec {
  pname = "unicode-slugify";
  version = "0.1.5";
  format = "setuptools";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-JfQkJYMX5MtBCT4pUzdLOvHyMJcpdmRzHNs65G9r1sM=";
  };

  propagatedBuildInputs = [
    six
    unidecode
  ];

  # upstream tests are nose-based and do not run under pytest without patching
  doCheck = false;

  meta = {
    description = "Generates unicode slugs";
    homepage = "https://pypi.org/project/unicode-slugify/";
    license = lib.licenses.bsd3;
  };
}

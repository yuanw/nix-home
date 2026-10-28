{
  buildPythonApplication,
  fetchFromGitHub,
  lib,
  packaging,
}:
buildPythonApplication rec {
  pname = "tccutil-manage";
  version = "1.5.5";
  format = "other";

  src = fetchFromGitHub {
    owner = "jacobsalmela";
    repo = "tccutil";
    # upstream tags are vN.N.N; the bare version 404s
    tag = "v${version}";
    hash = "sha256-pEVH8VZp8fsXMpIu/SlZ6JmwH/oKKGm7ANxZZlEPfUk=";
  };

  propagatedBuildInputs = [ packaging ];

  installPhase = ''
    mkdir -p $out/bin
    cp tccutil.py $out/bin/tccutil-manage
    chmod +x $out/bin/tccutil-manage
    patchShebangs $out/bin/tccutil-manage
  '';

  doCheck = false;

  meta = {
    description = "Manage macOS Privacy Preferences (TCC) from the command line";
    homepage = "https://github.com/jacobsalmela/tccutil";
    license = lib.licenses.mit;
    mainProgram = "tccutil-manage";
    platforms = lib.platforms.darwin;
  };
}

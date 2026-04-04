{
  buildPythonApplication,
  fetchFromGitHub,
  lib,
  packaging,
}:
buildPythonApplication rec {
  pname = "tccutil-manage";
  version = "1.5.1";
  format = "other";

  src = fetchFromGitHub {
    owner = "jacobsalmela";
    repo = "tccutil";
    rev = "28b1942216d30b0d9970690a238c3fc8785e5279";
    hash = "sha256-gb67xM8daBA03Oq8XCkLdNcPjx5qymz0U859gRaHofs=";
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

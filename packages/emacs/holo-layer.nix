{
  python3,
  melpaBuild,
  fetchFromGitHub,
  ...
}:
let
  python = python3.withPackages (
    ps: with ps; [
      epc
      inflect
      pyqt6
      pyqt6-sip
      sexpdata
      six
      xlib
    ]
  );
in
melpaBuild {
  version = "0-unstable-2025-04-05";
  pname = "holo-layer";

  src = fetchFromGitHub {
    owner = "manateelazycat";
    repo = "holo-layer";
    rev = "d99a7e2f21eeed74eafe491d5a33c861fb1f879c";
    hash = "sha256-8wNq9a7yNbpkuOH7oMRO1kbdAQb87HXeMHvOamYSwyQ=";
  };

  patchPhase = ''
    substituteInPlace holo-layer.el \
      --replace "\"python3\"" \
                "\"${python.interpreter}\""
  '';

  files = ''
    ("*.el"
     "*.py"
     "icon_cache"
     "plugin"
     "resources")
  '';

}

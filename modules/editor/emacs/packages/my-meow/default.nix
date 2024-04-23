{ trivialBuild
, meow
, dot-mode
,
}:
trivialBuild rec {
  pname = "my-meow";
  version = "0.0.1";
  src = ./my-meow.el;
  # elisp dependencies
  propagatedUserEnvPkgs = [
    meow
    dot-mode
  ];
  buildInputs = propagatedUserEnvPkgs;
}

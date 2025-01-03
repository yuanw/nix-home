{
  lib,
  f,
  fetchFromGitHub,
  markdown-mode,
  rustPlatform,
  trivialBuild,
  yasnippet,
}:

let
  version = "unstable-2024-04-27";

  src = fetchFromGitHub {
    owner = "zbelial";
    repo = "lspce";

    rev = "a5e27ba7e9add9ad09804572d04810ba6568c666";
    hash = "sha256-m353lvhxgE3zbgPKXD9x5lmCq+9GGxuAn4lOi5fUAcc=";
  };

  meta = {
    homepage = "https://github.com/zbelial/lspce";
    description = "LSP Client for Emacs implemented as a module using rust";
    license = lib.licenses.gpl3Only;
    maintainers = [ ];
  };

  lspce-module = rustPlatform.buildRustPackage {
    inherit version src meta;
    pname = "lspce-module";
    cargoHash = "sha256-3Ch2PFYf5fwTuHQIvy9frbZ5NNJJfNzxqqHjMXGFuqk=";
    checkFlags = [
      # flaky test
      "--skip=msg::tests::serialize_request_with_null_params"
    ];

    postInstall = ''
      mkdir -p $out/share/emacs/site-lisp
      for f in $out/lib/*; do
        mv $f $out/share/emacs/site-lisp/lspce-module.''${f##*.}
      done
      rmdir $out/lib
    '';
  };
in
trivialBuild rec {
  inherit version src meta;
  pname = "lspce";

  buildInputs = propagatedUserEnvPkgs;

  propagatedUserEnvPkgs = [
    f
    markdown-mode
    yasnippet
    lspce-module
  ];

  passthru = {
    inherit lspce-module;
  };
}

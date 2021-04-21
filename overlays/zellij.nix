{ lib, stdenv, fetchFromGitHub, rustPlatform, pkg-config, openssl
, installShellFiles, libiconv, Security }:

rustPlatform.buildRustPackage rec {
  pname = "zellij";
  version = "0.5.0-beta";

  src = fetchFromGitHub {
    owner = "zellij-org";
    repo = pname;
    rev = "v${version}";
    sha256 = "0mhxq0pr87lz36wfn3g9yvgx5k6xwf69yvvjvrm3vf1l6hwm9nf6";
  };

  nativeBuildInputs = [ installShellFiles ]
    ++ lib.optionals stdenv.isLinux [ pkg-config ];

  buildInputs = lib.optionals stdenv.isLinux [ openssl ]
    ++ lib.optionals stdenv.isDarwin [ libiconv Security ];

  cargoSha256 = "1d4ca8yzx437x53i7z2kddv9db89zy6ywbgl6y1cwwd6wscbrxcq";

  meta = with lib; {
    description =
      "A minimal, blazing fast, and extremely customizable prompt for any shell";
    homepage = "https://starship.rs";
    license = licenses.isc;
    maintainers = with maintainers; [
      bbigras
      davidtwco
      Br1ght0ne
      Frostman
      marsam
    ];
  };
}

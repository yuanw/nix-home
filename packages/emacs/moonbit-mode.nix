{ trivialBuild, fetchFromGitHub }:

trivialBuild rec {
  pname = "moonbit-mode";
  version = "0.1.0";
  src = fetchFromGitHub {
    owner = "tonyfettes";
    repo = "moonbit-mode";
    rev = "5b330a92033bf4739da322c9aaccd86c9cc4ba17";
    sha256 = "sha256-dWC7zIJ4BWghYH1WKD1meQ6VNwGs1Oi+wp6Sj7rNuqg=";
  };
}

{ stdenv, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "kn";
  version = "0.11.0";
  # rev is the release commit, mainly for version command output
  rev = "b10580ff726276f893bf93c22a9bc4f562e84cc0";

  src = fetchFromGitHub {
    owner  = "knative";
    repo   = "client";
    rev    = version;
    sha256 = "0k27mfccz563r18zlbaxll305vrmrx19ym6znsikvqxlmhy86g36";
  };

  buildFlagsArray = ''
    -ldflags=
      -s -w
      -X github.com/derailed/k9s/cmd.version=${version}
      -X github.com/derailed/k9s/cmd.commit=${rev}
  '';

  modSha256 = "09rwbl8zd06ax5hidm5l1schwqvsr5ndlqh09w1rq9fqjijy649y";

  meta = with stdenv.lib; {
    description = "Knative Client";
    homepage = https://github.com/knative/client;
    license = licenses.asl20;
    maintainers = with maintainers;
  };
}

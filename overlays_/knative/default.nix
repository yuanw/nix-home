self: super: {
  knative = super.buildGoModule rec {
    name = "knative-${version}";
    version = "0.11.0";
    rev = "b10580ff726276f893bf93c22a9bc4f562e84cc0";

    src = super.fetchFromGitHub {
      owner = "knative";
      repo = "client";
      rev = "v${version}";
      sha256 = "0wwvx6yvyr80grnxi7lpwyv7rp322a7vk9c6fwzq4jvp26d1ndri";
    };

    modSha256 = "1db3s5r6njyns7ka7sy4cqx89w22nwc41gg64yzdx8zi81aqgwzc";


    buildFlagsArray = ''
    -ldflags=
      -s -w
      -X github.com/knative/client/cmd.version=${version}
      -X github.com/knative/client/cmd.commit=${rev}
  '';


    meta = with super.lib; {
      description = "Knative developer experience, docs, reference Knative CLI implementation ";
      homepage = https://github.com/knative/client;
      license = licenses.mit;
      platforms = platforms.linux ++ platforms.darwin;
    };
  };
}

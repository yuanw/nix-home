let sources = import ../nix/sources.nix;
in
_: super: {

  ihp = with super;
    stdenv.mkDerivation rec {

      buildInputs = [ git makeWrapper ];
      pname = "iph";
      version = "0.8.0";

      src = fetchFromGitHub { inherit (sources.ihp) owner repo rev sha256; };

      # We override the install phase, as the emojify project doesn't use make
      installPhase = ''
        # Make the output directory
        mkdir -p $out/bin;
        cp ProjectGenerator/bin/ihp-new $out;
        makeWrapper $out/ihp-new $out/bin/ihp-new --prefix PATH ":" "${git}/bin";
      '';
    };
}

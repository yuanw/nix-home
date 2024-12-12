_: super: {

  emojify =
    with super;
    stdenv.mkDerivation rec {

      pname = "emojify";
      version = "2.1.0";

      src = fetchFromGitHub {
        owner = "mrowa44";
        repo = "emojify";
        # The git tag to fetch
        rev = "${version}";
        # Hashes must be specified so that the build is purely functional
        sha256 = "0l53fm1j72hcaais2k8499gcgqndxy93nkbw4r5mkpm6hn2qa9xw";
      };

      # We override the install phase, as the emojify project doesn't use make
      installPhase = ''
        # Make the output directory
        mkdir -p $out/bin

        # Copy the script there and make it executable
        cp emojify $out/bin/
        chmod +x $out/bin/emojify
      '';
    };
}

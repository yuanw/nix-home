self: super: {

gh = with super; stdenv.mkDerivation rec {

  pname = "gh";
  version = "0.5.4";

  nativeBuildInputs = [
    unzip
  ];

  src = fetchurl {
    url = "https://github.com/cli/cli/releases/download/v${version}/gh_${version}_macOS_amd64.tar.gz";
    sha256 = "0y610r9mw3b3ydaiyd2x8js9lnlcaq8qnrznr9h770y9jbnzzvyg";
  };


  installPhase = ''
    mkdir -p $out/gh
    cp -R * $out/gh/

    # create wrappers with correct env
    for program in gh; do
        programPath="$out/gh/bin/$program"
        binaryPath="$out/bin/$program"
        mkdir -p $out/bin
        ln -s $programPath $binaryPath
    done
  '';

  meta = {
    homepage = https://cli.github.com/;
    description = "Take github to the command line";
    license = stdenv.lib.licenses.bsd3;
  };
};
}

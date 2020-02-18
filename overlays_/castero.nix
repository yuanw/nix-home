self: super: {
  castero = with super;
    with python3Packages; buildPythonPackage
      rec {
        pname = "castero";
        version = "0.7.0";
        name = "${pname}-${version}";

        src = fetchFromGitHub {
            owner = "xgi";
            repo="castero";
            rev="b091d7088e4e9343f6621276413aa2a874a4b15e";
            # date = 2019-12-13T19:48:45-05:00"
            sha256= "1w4cz50msyhi4x7da5056r6r5z8x925rc4kc29kyyyk8v8jva236";
        };

        buildInputs = [ pyxdg
                        pytest
                        mpv
                        requests
                        beautifulsoup4
                        coverage
                        flake8
                        python-vlc
                      ];

        doCheck = false;

        pythonPath = [ pyxdg ];

        meta = {
          homepage = https://github.com/xgi/castero;
          description = "a TUI podcast client for the terminal";
          license = lib.licenses.mit;
          maintainers = lib.maintainers;
        };
      };
}

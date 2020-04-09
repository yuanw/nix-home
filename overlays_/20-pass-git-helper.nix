self: super: {

  pass-git-helper = with super;
    with python3Packages; buildPythonPackage
      rec {
        pname = "pass-git-helper";
        version = "1.1.1";
        name = "${pname}-${version}";

        src = fetchFromGitHub {
          owner = "languitar";
          repo = "pass-git-helper";
          rev = "713d67d73662f2c072ff2cbde896462dd7b1da9f";
          sha256 = "18nvwlp0w4aqj268wly60rnjzqw2d8jl0hbs6bkwp3hpzzz5g6yd";
          # date = 2019-06-13T21:22:01+02:00;
        };

        buildInputs = [
          pyxdg
          pytest
        ];

        doCheck = false;

        pythonPath = [ pyxdg ];

        meta = {
          homepage = https://github.com/languitar/pass-git-helper;
          description = "A git credential helper interfacing with pass, the standard unix password manager";
          license = lib.licenses.lgpl3;
          maintainers = lib.maintainers;
        };
      };
}

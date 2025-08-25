{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "thrift-mode";
  version = "0-unstable-2017-06-12";
  src = fetchFromGitHub {
    owner = "emacsmirror";
    repo = "thrift-mode";
    rev = "1293a54c6630297209d023c479712b539060d7f0";

    sha256 = "sha256-SQhmJ1QizEbJp7OTN/5CyQe65AwUnqwvgnzk+UaEkZs=";
  };

  preferLocalBuild = true;
  allowSubstitutes = false;

}

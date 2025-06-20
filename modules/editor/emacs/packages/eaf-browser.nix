{
  melpaBuild,
  buildNpmPackage,
  jq,
  fetchFromGitHub,
  ...
}:
melpaBuild (
  _finalAttrs:
  let
    version = "0-unstable-2025-06-13";
    nodeName = "browser";
    src = fetchFromGitHub {
      owner = "emacs-eaf";
      repo = "eaf-browser";
      rev = "120967319132f361a2b27f89ee54d1984aa23eaf";
      #sha256 = lib.fakeSha256;
      sha256 = "sha256-DsPrctB1bSGBPQLI2LsnSUtqnzWpZRrWrVZM8lS9fms=";
    };
    nodeModules = buildNpmPackage rec {
      pname = nodeName;
      inherit version src;

      npmDepsHash = "sha256-UfQL7rN47nI1FyhgBlzH4QtyVCn0wGV3Rv5Y+aidRNE=";
      prePatch = ''
        find . -mindepth 1 -maxdepth 1 ! -name "*.json" -exec rm -rf {} ';'
        ${jq}/bin/jq 'setpath(["name"]; "${pname}") | setpath(["version"]; "${version}")' package.json > package.json.tmp
        mv package.json.tmp package.json
      '';
      dontNpmBuild = true;
    };
  in
  {
    pname = "eaf-browser";
    inherit version src;
    postInstall = ''
      DST=$out/share/emacs/site-lisp/elpa/$ename-$melpaVersion/
      mv buffer.py $DST
      mv easylist.txt $DST
      ln -s ${nodeModules}/lib/node_modules/${nodeName}/node_modules $DST/node_modules
    '';
  }
)

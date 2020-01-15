self: super:
let extraHaskellPackages = hp: [ hp.colour hp.lens hp.MonadRandom ];
    source = super.fetchFromGitHub {
      owner  = "cdepillabout";
      repo   = "termonad";
      rev = "9b64dabec0e2c15a05246c4e20a065d56b184859";
      sha256 = "10xf699l3ivkhxhdhh9bn1ib59y61iryngp0z701da490iv46v16";
    };
in
{ termonad = import source { inherit extraHaskellPackages; };
}

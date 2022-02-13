{ config, lib, pkgs, isDarwin ? true, ... }:

with lib;
let
  cfg = config.programs.haskell;
  haskell-env = pkgs.haskellPackages.ghcWithHoogle (hp:
    with hp;
    [ apply-refact hlint lens turtle haskell-language-server
     xmonad xmonad-contrib ]);
in {
  options.programs.haskell = { enable = mkEnableOption "haskell"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ haskell-env ];
      home.file = {
        ".ghci".text = ''
          :set prompt "λ> "
          :set prompt-cont "| "
          :def hoogle \x -> pure $ ":!hoogle search \"" ++ x ++ "\""
          :set +t
          :set -XArrows
          :set -XBangPatterns
          :set -XBinaryLiterals
          :set -XBlockArguments
          :set -XConstraintKinds
          :set -XDataKinds
          :set -XDeriveTraversable
          :set -XDerivingVia
          :set -XEmptyCase
          :set -XEmptyDataDecls
          :set -XFlexibleContexts
          :set -XFlexibleInstances
          :set -XGADTs
          :set -XGeneralisedNewtypeDeriving
          :set -XImportQualifiedPost
          :set -XLambdaCase
          :set -XLiberalTypeSynonyms
          :set -XMonadComprehensions
          :set -XMultiWayIf
          :set -XNamedFieldPuns
          :set -XNumericUnderscores
          :set -XOverloadedStrings
          :set -XPatternSynonyms
          :set -XPolyKinds
          :set -XRankNTypes
          :set -XRecordWildCards
          :set -XRecursiveDo
          :set -XScopedTypeVariables
          :set -XStandaloneDeriving
          :set -XTupleSections
          :set -XTypeApplications
          :set -XTypeOperators
          :set -XUnicodeSyntax
          :set -XViewPatterns
          import Control.Applicative
          import Control.Arrow
          import Control.Lens
          import Control.Monad
          import Data.Bifunctor
          import Data.Bits
          import Data.Char
          import Data.Complex
          import Data.Either
          import Data.Foldable
          import Data.Function
          import Data.Functor
          import Data.Functor.Identity
          import Data.Ix
          import Data.List
          import Data.Map (Map)
          import Data.Map qualified as Map
          import Data.Maybe
          import Data.Monoid
          import Data.Ord
          import Data.Ratio
          import Data.Semigroup
          import Data.Set (Set)
          import Data.Set qualified as Set
          import Data.String
          import Data.Traversable
          import Data.Tuple
          import Data.Void
          import System.Environment
          import System.Exit
          import System.IO
          import System.Random
          import Text.Read
        '';
      };
    };
  };
}

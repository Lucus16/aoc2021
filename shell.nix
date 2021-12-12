with import <nixpkgs> { };

let
  ghc = haskellPackages.ghcWithHoogle (p: with p; [
    containers
    extra
    megaparsec
    text
  ]);

in mkShell {
  buildInputs = [ cabal-install ghc ];
}

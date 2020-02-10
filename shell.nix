{ pkgs ? import ./nixpkgs.nix }:

let
  ormolu = pkgs.haskellPackages.callCabal2nix "ormolu" (pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "3abadaefa5e190ff346f9aeb309465ac890495c2";
    sha256 = "0vqrb12bsp1dczff3i5pajzhjwz035rxg8vznrgj5p6j7mb2vcnd";
  }) { };

in pkgs.haskellPackages.shellFor {
  packages = p: [ (pkgs.callPackage ./default.nix { }) ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    ormolu
  ];
}

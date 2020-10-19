let pkgs = import ./nixpkgs.nix;

in pkgs.haskellPackages.shellFor {
  packages = p: [ (import ./default.nix) ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.ormolu
  ];
}

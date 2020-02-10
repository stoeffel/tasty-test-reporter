{ pkgs ? import ./nixpkgs.nix }:

pkgs.haskellPackages.callCabal2nix "tasty-test-reporter" ./. { }

# Pulls in a Haskell library for producing junit-xml style reports, used by our
# Shake code. The library isn't available from `nixpkgs` yet, hence this
# overlay. As soon as we can get it from `nixpkgs` we should delete this
# overlay.
# 
# https://github.com/jwoudenberg/junit-xml
self: super:

{
  haskellPackages = super.haskellPackages.extend (haskellSelf: haskellSuper: {
    junit-xml = let
      src = builtins.fetchGit {
        url = "git@github.com:jwoudenberg/junit-xml";
        rev = "9b5745c3c190205f662233abe5e3bf65e12fd55c";
      };
    in self.haskell.lib.dontCheck
    (self.haskellPackages.callCabal2nix "junit-xml" src { });
  });
}

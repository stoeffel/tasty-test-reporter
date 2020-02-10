let
  # This comes from https://nixos.org/channels/
  #
  # Pick a release (e.g. nixpkgs-unstable) and open the `git-revision`
  # file. It will contain a revision hash. Copy and paste it below.
  rev = "a8fc904c7c0d66f07d22bcb59a46d2bd72f8ddae";
  # Generate the SHA256 hash for this revision's tarball.
  #
  #   $ nix-prefetch-url --unpack --type sha256 \
  #   >   https://github.com/NixOS/nixpkgs/archive/${rev-defined-above}.tar.gz
  sha256 = "142gzi01601c95hrwjizjc25qsmswldzw2zg6fvgw52lpag2w2qb";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256;
  };
in import nixpkgs { overlays = [ (import ./junit-xml.nix) ]; }

let
  # This comes from https://nixos.org/channels/
  #
  # Pick a release (e.g. nixpkgs-unstable) and open the `git-revision`
  # file. It will contain a revision hash. Copy and paste it below.
  rev = "0c59c1296b23abc25a6383ff26db2eeb17ad8a81";
  # Generate the SHA256 hash for this revision's tarball.
  #
  #   $ nix-prefetch-url --unpack --type sha256 \
  #   >   https://github.com/NixOS/nixpkgs/archive/${rev-defined-above}.tar.gz
  sha256 = "03sifcpkc3prszaycd6snvpxam66phmj0b7m4723l5dmmsyq4bkw";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256;
  };
in import nixpkgs { overlays = [ (import ./junit-xml.nix) ]; }

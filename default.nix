{ nixpkgs ? import <nixos-18.03> {}
, compiler ? "default"
, doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs;

  baseHaskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      tagsoup-navigate = doJailbreak (super.callCabal2nix "tagsoup-navigate" (pkgs.fetchzip {
        url = "mirror://hackage/tagsoup-navigate-0.1.0.2/tagsoup-navigate-0.1.0.2.tar.gz";
        sha256 = "0ijp0vhprbi8dm48h728sjji3zg95mf0c5i3s36wspn3sq3azc2m";
      }) {});
      tagsoup-selection = doJailbreak super.tagsoup-selection;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
in
  variant (haskellPackages.callPackage ./asx-is-great.nix {})

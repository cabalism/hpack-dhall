let
  config = import ./nix/config.nix {};
  pkgs = import ./nix/nixpkgs.nix { inherit config; };
in
  { hpack-dhall = pkgs.haskellPackages.hpack-dhall; }

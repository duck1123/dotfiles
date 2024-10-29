# Shell for bootstrapping flake-enabled nix and home-manager, from any nix version
{ pkgs ? let
  inherit (builtins) currentSystem pathExists fromJSON readFile;

  nixpkgs = if pathExists ./flake.lock then
  # If we have a lock, fetch locked nixpkgs
    let inherit ((fromJSON (readFile ./flake.lock)).nodes.nixpkgs) locked;
    in fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${locked.rev}.tar.gz";
      sha256 = locked.narHash;
    }
  else
  # If not (probably because not flake-enabled), fetch nixos-unstable impurely
    fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/nixos-unstable.tar.gz";
    };

  system = currentSystem;
  overlays = [ ];

in import nixpkgs { inherit system overlays; }, ... }:
let
  # Enable experimental features without having to specify the argument
  nix = pkgs.writeShellScriptBin "nix" ''
    exec ${pkgs.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
  '';
in pkgs.mkShell {
  # See https://github.com/disassembler/network/blob/c341a3af27611390f13f86d966767ea30c726a92/shell.nix
  sopsPGPKeyDirs = [ "./nixos/secrets/keys" ];

  nativeBuildInputs = with pkgs; [
    nix
    nixpkgs-fmt
    home-manager
    git
    kubectl
    sops
    ssh-to-pgp
  ];
}

# Shell for bootstrapping flake-enabled nix and home-manager, from any nix version
{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  # See https://github.com/disassembler/network/blob/c341a3af27611390f13f86d966767ea30c726a92/shell.nix
  sopsPGPKeyDirs = [ "./nixos/secrets/keys" ];

  nativeBuildInputs = with pkgs; [
    nix
    nixpkgs-fmt
    home-manager
    git
    kubectl
    kubeseal
    sops
    ssh-to-pgp
  ];
}

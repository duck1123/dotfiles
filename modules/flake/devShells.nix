{ inputs, ... }:
let
  inherit (inputs.flake-utils.lib) eachSystemMap defaultSystems;
  eachDefaultSystemMap = eachSystemMap defaultSystems;
in {
  flake.devShells = eachDefaultSystemMap (system:
    let pkgs = import inputs.nixpkgs { inherit system; };
    in {
      default = pkgs.mkShell {
        name = "installation-shell";

        # See https://github.com/disassembler/network/blob/c341a3af27611390f13f86d966767ea30c726a92/shell.nix
        sopsPGPKeyDirs = [ "../nixos/secrets/keys" ];

        buildInputs = with pkgs; [
          age
          babashka
          cachix
          clojure
          pkgs.colmena
          emacs
          git
          pkgs.home-manager
          keepassxc
          kubectl
          nh
          nix
          nixpkgs-fmt
          nmap
          runme
          sops
          ssh-to-age
          ssh-to-pgp
          vals
          wget
        ];
      };
    });
}

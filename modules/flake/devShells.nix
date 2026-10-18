{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    let
      pkgs = import inputs.nixpkgs { inherit system; };
    in
    {
      devShells.default = pkgs.mkShell {
        name = "installation-shell";

        buildInputs =
          with pkgs;
          [
            age
            cachix
            emacs
            git
            home-manager
            keepassxc
            kubectl
            nh
            nix
            nix-output-monitor
            nixpkgs-fmt
            nmap
            runme
            sops
            ssh-to-age
            statix
            vals
            wget
          ]
          ++ [ inputs.nur-taskrunner.packages.${system}.default ];
      };
    };
}

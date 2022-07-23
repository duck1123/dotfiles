{ system, nixpkgs, home-manager, ... }:
let
  username = "duck";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.dotfiles";

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    config.xdg.configHome = configHome;
    # overlays = [ nurpkgs.overlay ];
  };
in {
  main = home-manager.lib.homeManagerConfiguration rec {
    inherit pkgs system username homeDirectory;

    stateVersion = "21.03";
    configuration = import ./home.nix {
      inherit pkgs;
      inherit (pkgs) config lib stdenv;
    };
  };
}

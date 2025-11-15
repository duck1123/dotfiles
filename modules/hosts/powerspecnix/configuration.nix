{ ... }:
let
  hosts = import ../../../hosts/default.nix { };
  host = hosts.powerspecnix;
in {
  flake.modules.homeManager.powerspecnix = { pkgs, ... }: {
    imports = [ ../../../programs ];
    inherit host hosts;

    home = {
      packages = with pkgs; [
        alacritty
        colmena
        discord
        distrobox
        docker
        # fastfetch
        ffmpeg
        # gitu
        # kakoune
        # kb
        # keet
        # khoj
        kty
        libnotify
        # logseq
        # mdcat
        minio-client
        # mullvad-browser
        networkmanager
        nix-tree
        # obsidian
        # onlyoffice-bin
        playerctl
        # postman
        # sparrow
        syncthing
        telegram-desktop
        # tilt
        transmission_4-gtk
        # tree
        unzip
        # virtualbox
        vscode
        wine
        xsel
        # yq
      ];

      sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
    };
  };

  flake.modules.nixos.powerspecnix = { inputs, ... }:
    let
      core = [
        {
          inherit host hosts;

          boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
          };

          nixpkgs.overlays = [ inputs.sddm-sugar-candy-nix.overlays.default ];
        }
        inputs.self.modules.nixos.base
        ../../../hosts/powerspecnix/base.nix
        ../../../hosts/powerspecnix/hardware-configuration.nix
      ];
      mkSpecialisation = module: {
        inheritParentConfig = false;
        configuration = {
          imports = core ++ [ module ];
          _module.args = { inherit inputs; };
        };
      };
      specialisations = with inputs.self.modules.nixos; {
        budgie = mkSpecialisation environments-budgie;
        hyprland = mkSpecialisation environments-hyprland;
        gnome = mkSpecialisation environments-gnome;
        i3 = mkSpecialisation environments-i3;
        plasma6 = mkSpecialisation environments-plasma6;
      };
      host-module = {
        imports = specialisations.hyprland.configuration.imports;
        specialisation = {
          inherit (specialisations) budgie;
          # inherit (specialisations) gnome;
          # inherit (specialisations) i3;
          # inherit (specialisations) hyprland;
          # inherit (specialisations) plasma6;
        };
      };
    in {
      _module.args = { inherit inputs; };
      imports = [ host-module ../../../nixosModules ];
    };
}

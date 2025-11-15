{ ... }:
let loadHosts = config: import ../../hosts/default.nix { inherit config; };
in {
  flake.modules.homeManager.powerspecnix = { pkgs, config, ... }:
    let
      hosts = loadHosts config;
      host = hosts.powerspecnix;
    in {
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

  flake.modules.nixos.powerspecnix = { inputs, pkgs, config, ... }:
    let
      hosts = loadHosts config;
      host = hosts.powerspecnix;
      core = [
        {
          inherit host hosts;

          boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
          };

          environment.systemPackages = with pkgs; [ git ];

          hardware = {
            flipperzero.enable = true;
            rtl-sdr.enable = true;
          };

          nixpkgs.config.chromium.enableWideVine = true;

          programs = {
            dconf.enable = true;

            gnupg.agent = {
              enable = true;
              enableSSHSupport = true;
            };

            nix-ld = {
              enable = true;
              libraries = with pkgs; [ alsa-lib libGL ];
            };
          };

          services = {
            gnome.gnome-keyring.enable = true;
            flatpak.enable = true;
            plex.enable = true;
            printing.enable = true;
            udev.packages = with pkgs; [ gnome-settings-daemon ];
          };

          system.stateVersion = "25.05";

          time.timeZone = "America/Detroit";

          virtualisation = {
            docker.enable = true;
            libvirtd.enable = true;
          };
        }
        inputs.self.modules.nixos.base
        inputs.self.modules.nixos.sddm
        ../../hosts/powerspecnix/hardware-configuration.nix
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
      imports = [ host-module ];
    };
}

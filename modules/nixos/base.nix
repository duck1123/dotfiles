{ ... }: {
  flake.modules.nixos.base = { inputs, ... }:
    let
      features = with inputs.self.modules.nixos; [
        battery-feature
        bitcoin-feature
        bluetooth-feature
        docker-feature
        font-feature
        gaming-feature
        kubernetes-feature
        media-feature
        network-feature
        nfs-feature
        nix-feature
        sddm-feature
        sound-feature
        ssh-feature
        stylix-feature
        syncthing-feature
        tailscale-feature
        touch-feature
        virtualization-feature
        xserver-feature
        zsh-feature
      ];
    in {
      imports = features ++ [
        inputs.home-manager.nixosModules.home-manager
        inputs.sddm-sugar-candy-nix.nixosModules.default
        inputs.self.modules.generic.options
        inputs.sops-nix.nixosModules.sops
        inputs.stylix.nixosModules.stylix
        ../../nixosModules
      ];
    };
}

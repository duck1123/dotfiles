{ ... }: {
  flake.types.generic.feature-options.kubernetes = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in mkOption {
      type = types.submodule {
        options = {
          client = simpleFeature "kubernetes client";
          server = simpleFeature "kubernetes server";
        };
      };
      default = { };
      description = "Kubernetes configuration";
    };

  flake.modules.nixos.kubernetes-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.kubernetes.server.enable {
      environment.systemPackages = with pkgs; [ k3s nfs-utils openiscsi ];

      networking.firewall = {
        enable = false;
        allowedTCPPorts = [ 6443 ];
        allowedUDPPorts = [ 8472 ];
      };

      services = {
        k3s = {
          enable = true;
          role = "server";
          extraFlags =
            toString [ "--cluster-cidr=10.42.0.0/16" "--disable=traefik" ];
        };

        openiscsi = {
          enable = true;
          name = "${config.host.hostname}-initiatorhost";
        };
      };

      system.activationScripts.makeUsrBinSymlinks = ''
        mkdir -p /usr/bin
        ln -sf ${pkgs.openiscsi}/bin/iscsiadm /usr/bin/iscsiadm
      '';
    };
  };
}


{ ... }: {
  flake.types.generic.feature-options.samba = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "Samba support";

  flake.modules.nixos.samba = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.samba.enable {
      environment.systemPackages = with pkgs; [ samba ];
      networking.firewall.allowedTCPPorts = [ 445 ];

      services.samba = {
        enable = true;
        settings.global = {
          security = "user";
          "client min protocol" = "SMB2";
          "client max protocol" = "SMB3";
          workgroup = "WORKGROUP";
        };
      };

      users.users.${config.host.identity.username}.extraGroups = [ "samba" ];
    };
  };
}

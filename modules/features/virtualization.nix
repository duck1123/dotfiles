{ ... }: {
  flake.modules.nixos.virtualization-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.virtualization.enable {
      users.extraGroups.vboxusers.members = [ config.host.identity.username ];

      virtualisation.virtualbox.host = {
        addNetworkInterface = false;
        enable = true;
        enableExtensionPack = true;
        enableKvm = true;
      };
    };
  };
}


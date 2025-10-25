{ host, lib, ... }: {
  options = {
    features.virtualization.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable virtualization";
    };
  };

  config = lib.mkIf host.features.virtualization.enable {
    users.extraGroups.vboxusers.members = [ host.identity.username ];

    virtualisation.virtualbox.host = {
      addNetworkInterface = false;
      enable = true;
      enableExtensionPack = true;
      enableKvm = true;
    };
  };
}

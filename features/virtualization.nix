{
  nixos =
    { config, ... }:
    {
      users = {
        extraGroups.vboxusers.members = [ config.host.identity.username ];
        users.${config.host.identity.username}.extraGroups = [ "libvirtd" ];
      };

      virtualisation.virtualbox.host = {
        addNetworkInterface = false;
        enable = true;
        enableExtensionPack = true;
        enableKvm = true;
      };
    };
}

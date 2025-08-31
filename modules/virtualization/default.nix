{ host, ... }: {
  users.extraGroups.vboxusers.members = [ host.identity.username ];

  virtualisation.virtualbox.host = {
    addNetworkInterface = false;
    enable = true;
    enableExtensionPack = true;
    enableKvm = true;
  };
}

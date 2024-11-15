{ config, inputs, ... }:
let
  pkgs = inputs.pkgs;
  username = config.username;
in {
  # environment.systemPackages = with pkgs; [ virtualbox ];

  virtualisation.virtualbox.host = {
    addNetworkInterface = false;
    enable = true;
    enableExtensionPack = true;
    enableKvm = true;
  };

  users.extraGroups.vboxusers.members = [ username ];
}

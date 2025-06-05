{ config, hosts, identity, inputs, ... }:
let inherit (identity) username;
in {
  assertions = [
    {
      assertion = hosts ? pixel8;
      message = "The host 'pixel8' must be present in the hosts attrset!";
    }
  ];
  services.syncthing = {
    enable = true;
    user = username;
    dataDir = "/home/${username}/Documents";
    configDir = "/home/${username}/Documents/.config/syncthing";
    openDefaultPorts = true;
    overrideFolders = false;
    overrideDevices = true;

    settings = {
      devices = {
        "powerspecnix" = {
          id = hosts.powerspecnix.id;
          autoAcceptFolders = true;
        };

        "Pixel 8" = {
          id = hosts.pixel8.id;
          autoAcceptFolders = true;
        };

        "steamdeck" = {
          id = hosts.steamdeck.id;
          autoAcceptFolders = true;
        };

        "VallenPC" = {
          id = hosts.vallenpc.id;
          autoAcceptFolders = true;
        };

        "inspernix" = {
          id = hosts.inspernix.id;
          autoAcceptFolders = true;
        };
      };

      folders = {
        "Camera" = {
          label = "Camera";
          path = "/home/${username}/Camera";
          devices = [ "Pixel 8" "powerspecnix" "inspernix" ];
        };

        "keepass" = {
          label = "keepass";
          path = "/home/${username}/keepass";
          devices = [ "Pixel 8" "VallenPC" "inspernix" "powerspecnix" ];
        };

        "org-roam" = {
          path = "/home/${username}/org-roam";
          devices = [ "Pixel 8" "inspernix" "powerspecnix" ];
          versioning = {
            type = "simple";
            params.keep = "10";
          };
        };

        "steamdeck-renpy" = {
          path = "/home/${username}/.renpy";
          devices = [ "steamdeck" "powerspecnix" ];
        };
      };

      options.urAccepted = -1;
    };
  };
}

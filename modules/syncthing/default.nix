{ host, hosts, lib, ... }: {
  config = lib.mkIf host.features.syncthing.enable {
    inherit hosts;

    services.syncthing = let inherit (host.identity) username;
    in {
      enable = true;
      user = username;
      dataDir = "/home/${username}/Documents";
      configDir = "/home/${username}/Documents/.config/syncthing";
      openDefaultPorts = true;
      overrideFolders = false;
      overrideDevices = true;

      settings = with hosts; {
        devices = {
          ${powerspecnix.name} = {
            id = powerspecnix.id;
            autoAcceptFolders = true;
          };

          ${pixel8.name} = {
            id = pixel8.id;
            autoAcceptFolders = true;
          };

          "steamdeck" = {
            id = steamdeck.id;
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
  };
}

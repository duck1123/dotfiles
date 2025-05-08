{ config, inputs, ... }:
let
  pkgs = inputs.pkgs;
  username = config.username;
in {
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
        "powespecnix" = {
          id =
            "JZHCKZ4-6WQOOMW-VK3J7WZ-LN7O3KU-C6IO3EY-3D4XBDT-P3R73MM-DUARSA3";
          autoAcceptFolders = true;
        };


        "Pixel 8" = {
          id =
            "7Y3NTUQ-MRUHGO4-5L34ZC7-EDRXHKA-QVCG7AJ-HWHIINY-OV5B2T7-OFQS2QP";
          autoAcceptFolders = true;
        };

        "steamdeck" = {
          id =
            "ZPO3QWJ-LQHVWBH-TAI3LLD-ZS6WSBM-N5IQ7JX-P4HUVF3-XNOX6N4-NBIF3AX";
          autoAcceptFolders = true;
        };

        "VallenPC" = {
          id =
            "TEED77K-QOLTQ37-BL76MFB-LJD46CW-EJ7CZTJ-7GQNEF6-FZAMQRP-BCCRTQ6";
          autoAcceptFolders = true;
        };

        "inspernix" = {
          id =
            "OWMQLRL-CD5VB7H-A3T436E-6XT4H66-6XRF22Y-MQXMNAU-DFRNGOV-ADSKFAV";
          autoAcceptFolders = true;
        };
      };

      folders = {
        "Camera" = {
          label = "Camera";
          path = "/home/${username}/Camera";
          devices = [ "Pixel 8" "powespecnix" "inspernix" ];
        };

        "keepass" = {
          label = "keepass";
          path = "/home/${username}/keepass";
          devices = [ "Pixel 8" "VallenPC" "inspernix" "powespecnix" ];
        };

        "org-roam" = {
          path = "/home/${username}/org-roam";
          devices = [ "Pixel 8" "inspernix" "powespecnix" ];
          versioning = {
            type = "simple";
            params.keep = "10";
          };
        };

        "steamdeck-renpy" = {
          path = "/home/${username}/.renpy";
          devices = [ "steamdeck" "powespecnix" ];
        };
      };

      options.urAccepted = -1;
    };
  };
}

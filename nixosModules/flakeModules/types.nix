{ lib }:
with lib;
let
  # Helper function to create a simple feature submodule with just an enable option
  simpleFeature = description:
    mkOption {
      type = types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = "Enable ${description}";
          };
        };
      };
      default = { };
      description = "${description} configuration";
    };
in let
  identitySubmodule = types.submodule {
    options = {
      name = mkOption {
        type = types.str;
        description = "Full name of the user";
      };
      username = mkOption {
        type = types.str;
        description = "Username";
      };
      email = mkOption {
        type = types.str;
        description = "Email address";
      };
      gpgKey = mkOption {
        type = types.str;
        description = "GPG key ID";
      };
    };
  };

  # Host submodule type factory - takes the simpleFeature and identitySubmodule
  hostSubmodule = { simpleFeature, identitySubmodule }:
    types.submodule {
      options = {
        system = mkOption {
          type = types.str;
          description = "The system architecture";
        };

        id = mkOption {
          type = types.str;
          description = "The host id";
        };

        identity = mkOption {
          type = identitySubmodule;
          description =
            "The identity configuration (name, username, email, gpgKey)";
        };

        name = mkOption {
          type = types.str;
          description = "The host name";
        };

        hostname = mkOption {
          type = types.str;
          description = "The hostname";
        };

        home-manager = mkOption {
          type = types.submodule {
            options = {
              enable = mkOption {
                type = types.bool;
                default = false;
                description = "Enable home-manager configuration";
              };
            };
          };
          default = { };
          description = "Home-manager configuration";
        };

        features = mkOption {
          type = types.submodule {
            options = {
              backups = simpleFeature "backups feature";
              battery = simpleFeature "battery feature";
              bitcoin = simpleFeature "bitcoin feature";
              bluetooth = simpleFeature "bluetooth feature";
              chm = simpleFeature "chm feature";
              clojure = simpleFeature "clojure feature";
              dbt = simpleFeature "dbt feature";
              dconf = simpleFeature "dconf feature";
              developer = simpleFeature "developer feature";
              docker = simpleFeature "docker feature";
              dunst = simpleFeature "dunst feature";
              emacs = simpleFeature "emacs feature";
              emacs-prelude = simpleFeature "emacs-prelude feature";
              email = simpleFeature "email feature";
              font = simpleFeature "font feature";
              gaming = simpleFeature "gaming feature";
              git = simpleFeature "git feature";
              gnome = simpleFeature "gnome feature";
              flipper = simpleFeature "flipper feature";
              hyprland = simpleFeature "hyprland feature";
              hyprpanel = simpleFeature "hyprpanel feature";
              i3 = simpleFeature "i3 feature";
              java = simpleFeature "java feature";
              jujutsu = simpleFeature "jujutsu feature";

              kubernetes = mkOption {
                type = types.submodule {
                  options = {
                    client = simpleFeature "kubernetes client";
                    server = simpleFeature "kubernetes server";
                  };
                };
                default = { };
                description = "Kubernetes configuration";
              };

              media = mkOption {
                type = types.submodule {
                  options = {
                    enable = mkOption {
                      type = types.bool;
                      default = false;
                      description = "Enable media feature";
                    };

                    server = mkOption {
                      type = types.submodule {
                        options = {
                          enable = mkOption {
                            type = types.bool;
                            default = false;
                            description = "Enable media server";
                          };
                        };
                      };
                      default = { };
                      description = "Media server configuration";
                    };
                  };
                };
                default = { };
                description = "Media configuration";
              };

              music = simpleFeature "music feature";
              ncmpcpp = simpleFeature "ncmpcpp feature";
              network = simpleFeature "network feature";
              nfs = simpleFeature "nfs feature";
              nix = simpleFeature "nix feature";
              nostr = simpleFeature "nostr feature";
              nushell = simpleFeature "nushell feature";
              office = simpleFeature "office feature";
              pictures = simpleFeature "pictures feature";
              radio = simpleFeature "radio feature";
              sddm = simpleFeature "sddm feature";
              sound = simpleFeature "sound feature";
              ssh = simpleFeature "ssh feature";
              stylix = simpleFeature "stylix feature";

              syncthing = mkOption {
                type = types.submodule {
                  options = {
                    enable = mkOption {
                      type = types.bool;
                      default = false;
                      description = "Enable syncthing";
                    };

                    shares = mkOption {
                      type = types.submodule {
                        options = {
                          camera = simpleFeature "camera share";
                          keepass = simpleFeature "keepass share";
                          org-roam = simpleFeature "org-roam share";
                          renpy = simpleFeature "renpy share";
                        };
                      };
                      default = { };
                      description = "Syncthing shares configuration";
                    };
                  };
                };
                default = { };
                description = "Syncthing configuration";
              };

              tailscale = simpleFeature "tailscale feature";
              touch = simpleFeature "touch feature";
              vim = simpleFeature "vim feature";
              virtualization = simpleFeature "virtualization feature";
              vscode = simpleFeature "vscode feature";
              waybar = simpleFeature "waybar feature";
              xserver = simpleFeature "xserver feature";
              zsh = simpleFeature "zsh feature";
            };
          };
          default = { };
          description = "Feature flags for the host";
        };

        nixos = mkOption {
          type = types.submodule {
            options = {
              enable = mkOption {
                type = types.bool;
                default = false;
                description = "Enable nixos configuration";
              };

              budgie = simpleFeature "budgie environment";
              gnome = simpleFeature "gnome environment";
              hyprland = simpleFeature "hyprland environment";
              i3 = simpleFeature "i3 environment";
              plasma6 = simpleFeature "plasma6 environment";
            };
          };
          default = { };
          description = "NixOS environment configuration";
        };

        android = mkOption {
          type = types.submodule {
            options = {
              enable = mkOption {
                type = types.bool;
                default = false;
                description = "Enable android configuration";
              };
            };
          };
          default = { };
          description = "Android configuration";
        };
      };
    };
in { inherit simpleFeature identitySubmodule hostSubmodule; }

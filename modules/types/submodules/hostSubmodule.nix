{ ... }:
{
  flake.types.generic.hostSubmodule =
    { inputs, lib, ... }:
    let
      inherit (inputs.self.types) generic;
      android-submodule = generic.android-submodule { inherit inputs lib; };
      featureSubmodule = generic.feature-submodule { inherit inputs lib; };
      home-manager-submodule = generic.home-manager-submodule { inherit inputs lib; };
      identitySubmodule = generic.identitySubmodule { inherit inputs lib; };
      nixos-submodule = generic.nixos-submodule { inherit inputs lib; };
    in
    with lib;
    types.submodule {
      options = {
        android = mkOption {
          type = android-submodule;
          default = { };
          description = "Android configuration";
        };

        features = mkOption {
          type = featureSubmodule;
          default = { };
          description = "Feature flags for the host";
        };

        home-manager = mkOption {
          type = home-manager-submodule;
          default = { };
          description = "Home-manager configuration";
        };

        hostname = mkOption {
          type = types.str;
          description = "The hostname";
        };

        id = mkOption {
          type = types.str;
          description = "The host id";
        };

        identity = mkOption {
          type = identitySubmodule;
          description = "The identity configuration (name, username, email, gpgKey)";
        };

        name = mkOption {
          type = types.str;
          description = "The host name";
        };

        nixos = mkOption {
          type = nixos-submodule;
          default = { };
          description = "NixOS environment configuration";
        };

        pubkey = mkOption {
          type = types.str;
          default = "";
          description = "The pubkey for this system";
        };

        system = mkOption {
          type = types.str;
          description = "The system architecture";
        };
      };
    };
}

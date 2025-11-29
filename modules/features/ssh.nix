{ ... }: {
  flake.types.generic.feature-options.ssh = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "ssh feature";

  flake.modules.nixos.ssh-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.ssh.enable {
      services.openssh = {
        enable = true;

        settings = {
          KbdInteractiveAuthentication = false;
          PasswordAuthentication = false;
        };
      };
    };
  };
}


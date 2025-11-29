{ ... }: {
  flake.types.generic.feature-options.ssh = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "ssh feature";

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


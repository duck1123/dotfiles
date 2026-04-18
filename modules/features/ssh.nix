{ ... }:
{
  flake.types.generic.feature-options.ssh =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "ssh feature";

  flake.modules.nixos.ssh-feature =
    { config, lib, ... }:
    let
      hostPubkeys = lib.filter (k: k != "") (lib.mapAttrsToList (_: h: h.pubkey) config.hosts);
    in
    {
      config = lib.mkIf config.host.features.ssh.enable {
        services.openssh = {
          enable = true;

          settings = {
            KbdInteractiveAuthentication = false;
            PasswordAuthentication = false;
          };
        };

        users.users.${config.host.identity.username}.openssh.authorizedKeys.keys = hostPubkeys;
      };
    };
}

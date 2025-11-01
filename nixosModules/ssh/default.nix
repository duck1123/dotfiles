{ config, lib, ... }: {
  config = lib.mkIf config.host.features.ssh.enable {
    services.openssh = {
      enable = true;

      settings = {
        KbdInteractiveAuthentication = false;
        PasswordAuthentication = false;
      };
    };
  };
}

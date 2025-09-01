{ host, lib, ... }: {
  config = lib.mkIf host.features.ssh.enable {
    services.openssh = {
      enable = true;

      settings = {
        KbdInteractiveAuthentication = false;
        PasswordAuthentication = false;
      };
    };
  };
}

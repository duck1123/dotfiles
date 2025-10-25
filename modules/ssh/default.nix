{ host, lib, ... }: {

  options = {
    features.ssh.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable ssh";
    };
  };

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

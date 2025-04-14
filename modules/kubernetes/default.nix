{ config, inputs, pkgs, ... }: {
  environment.systemPackages = with pkgs; [ k3s nfs-utils openiscsi ];

  # Create the expected symlink for iscsiadm
  environment.etc."iscsiadm".source = "${pkgs.openiscsi}/bin/iscsiadm";
  environment.etc."open-iscsi".source = "${pkgs.openiscsi}";
  # Optionally, also symlink to /usr/bin if necessary
  environment.sessionVariables.PATH = [ "/usr/bin" ];

  system.activationScripts.makeUsrBinSymlinks = ''
    mkdir -p /usr/bin
    ln -sf ${pkgs.openiscsi}/bin/iscsiadm /usr/bin/iscsiadm
  '';

  networking.firewall = {
    enable = false;
    allowedTCPPorts = [ 6443 ];
    allowedUDPPorts = [ 8472 ];
  };

  services = {
    k3s = {
      # enable = true;
      enable = false;
      role = "server";
      extraFlags = toString [ "--disable=traefik" ];
    };

    openiscsi = {
      enable = true;
      name = "${config.hostname}-initiatorhost";
    };
  };
}

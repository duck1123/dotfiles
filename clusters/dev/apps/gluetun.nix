{ secrets, ... }:
{
  services.gluetun = {
    controlServer = { inherit (secrets.gluetun) password username; };
    enable = true;
    # hostAffinity = "edgenix";
    wireguardPrivateKey = secrets.mullvad.wireguardKey;
    wireguardAddresses = "10.69.161.167/32,fc00:bbbb:bbbb:bb01::6:a1a6/128";
    storageClassName = "longhorn";

  };
}

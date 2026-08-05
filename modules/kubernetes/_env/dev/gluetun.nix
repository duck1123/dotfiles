{ secrets, ... }:
{
  services.gluetun = {
    controlServer = { inherit (secrets.gluetun) password username; };
    enable = true;
    hostAffinity = "edgenix";
    # FIXME: services.gluetun.mullvadAccountNumber no longer exists upstream.
    # Auth is now wireguardPrivateKey/wireguardAddresses (mullvad.net/en/account/wireguard-config);
    # secrets only has mullvad.id (the old account number). Stubbed empty until real values are added.
    wireguardPrivateKey = "";
    wireguardAddresses = "";
    storageClassName = "longhorn";
  };
}

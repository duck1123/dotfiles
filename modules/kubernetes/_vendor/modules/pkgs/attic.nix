{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      # nix build .#attic-server-bundle
      # Mirrors what the nix-csi nixExpr builds: binary + CA certificates
      # bundled together so SSL_CERT_FILE=result/etc/ssl/certs/ca-bundle.crt works.
      # attic-client is included so `attic`/`atticadm` are exec-able inside the
      # running pod for admin/cache-create operations.
      packages.attic-server-bundle = pkgs.buildEnv {
        name = "attic-server-bundle";
        paths = [
          inputs.attic.packages.${pkgs.system}.attic-server
          inputs.attic.packages.${pkgs.system}.attic-client
          pkgs.cacert
        ];
      };
    };
}

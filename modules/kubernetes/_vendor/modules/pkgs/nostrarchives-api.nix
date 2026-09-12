{ ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      src = pkgs.fetchFromGitHub {
        owner = "barrydeen";
        repo = "nostrarchives-api";
        # Pin to a specific commit for reproducibility.
        # To find the right hashes after changing rev:
        #   nix build .#nostrarchives-api  →  first failure shows the correct hash
        rev = "d616cdd09119bf3e1f1db50f8d7a823e7459dedf";
        hash = "sha256-EmeElAXv6LhBt9kZRzYx6LK4rLAY3vCIWt3YsCCIOA4=";
      };

      api = pkgs.rustPlatform.buildRustPackage {
        pname = "nostrarchives-api";
        version = "unstable";
        inherit src;
        cargoLock.lockFile = src + "/Cargo.lock";
        # If Cargo.lock has git-sourced crates, add their hashes:
        # cargoLock.outputHashes."some-crate-0.1.0" = "sha256-...";
        nativeBuildInputs = [ pkgs.pkg-config ];
        buildInputs = [ pkgs.openssl ];
        env.OPENSSL_NO_VENDOR = "1";
      };
    in
    {
      # nix build .#nostrarchives-api
      packages.nostrarchives-api = api;

      # nix build .#nostrarchives-api-bundle
      # Mirrors what the nix-csi nixExpr builds: binary + CA certificates
      # bundled together so SSL_CERT_FILE=result/etc/ssl/certs/ca-bundle.crt works.
      packages.nostrarchives-api-bundle = pkgs.buildEnv {
        name = "nostrarchives-api-bundle";
        paths = [
          api
          pkgs.cacert
        ];
      };
    };
}

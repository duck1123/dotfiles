{ ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      src = pkgs.fetchFromGitLab {
        owner = "soapbox-pub";
        repo = "ditto-relay";
        # Pin to a specific commit for reproducibility.
        # To find the right hashes after changing rev:
        #   nix build .#ditto-relay  →  first failure shows the correct hash
        rev = "6b3aa8332c20d896f6be4ae5b1b364347c6a1abe";
        hash = "sha256-iGk9HDIxgizKZOG2pvxnkt1iq5cYeZiquSM/KzditCo=";
      };

      # `bun install` needs a reachable registry, which stdenv only allows
      # inside a fixed-output derivation (hence outputHash below).
      # dontPatchShebangs/dontFixup skip stdenv's normal fixup pass -- without
      # it, patchShebangs rewrites node_modules/.bin scripts' shebangs to an
      # absolute nix store bash path, and a FOD isn't allowed to reference
      # other store paths in its output.
      nodeModules = pkgs.stdenvNoCC.mkDerivation {
        pname = "ditto-relay-node-modules";
        version = "unstable";
        inherit src;
        nativeBuildInputs = [
          pkgs.bun
          pkgs.cacert
        ];
        dontPatchShebangs = true;
        dontFixup = true;
        buildPhase = ''
          export HOME=$TMPDIR
          export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          bun install --frozen-lockfile --production --ignore-scripts
        '';
        installPhase = ''
          mkdir -p $out
          cp -r node_modules $out/
        '';
        outputHashMode = "recursive";
        outputHash = "sha256-jpvYUonfwno4hLkj4vbQg/b8XOOjWwBzyooWzA6A/ZQ=";
      };

      # No build step -- bun runs the TypeScript sources directly (same as
      # upstream's own Dockerfile), so this just assembles sources +
      # node_modules and wraps `bun run src/server.ts` with the right cwd.
      relay = pkgs.stdenvNoCC.mkDerivation {
        pname = "ditto-relay";
        version = "unstable";
        inherit src;
        nativeBuildInputs = [ pkgs.makeWrapper ];
        dontBuild = true;
        installPhase = ''
          mkdir -p $out/share/ditto-relay
          cp -r src scripts public package.json bun.lock tsconfig.json $out/share/ditto-relay/
          ln -s ${nodeModules}/node_modules $out/share/ditto-relay/node_modules

          makeWrapper ${pkgs.bun}/bin/bun $out/bin/ditto-relay \
            --chdir $out/share/ditto-relay \
            --add-flags "run $out/share/ditto-relay/src/server.ts"
        '';
      };
    in
    {
      # nix build .#ditto-relay
      packages.ditto-relay = relay;

      # nix build .#ditto-relay-bundle
      # Mirrors what the nix-csi nixExpr builds: binary + CA certificates
      # bundled together so SSL_CERT_FILE=result/etc/ssl/certs/ca-bundle.crt works.
      packages.ditto-relay-bundle = pkgs.buildEnv {
        name = "ditto-relay-bundle";
        paths = [
          relay
          pkgs.cacert
        ];
      };
    };
}

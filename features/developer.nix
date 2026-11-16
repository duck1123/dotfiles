{
  homeManager =
    { config, pkgs, ... }:
    let
      soap-cli = pkgs.stdenv.mkDerivation {
        pname = "soap-cli";
        version = "1.3";
        src = pkgs.fetchFromGitHub {
          owner = "pmamico";
          repo = "soap-cli";
          rev = "v1.3";
          hash = "sha256-2YIzeHMhF8HQDI04olqZ6q8z3+L66VpqlyrClNp2BLQ=";
        };
        nativeBuildInputs = [ pkgs.makeWrapper ];
        dontBuild = true;
        installPhase = ''
          mkdir -p $out/bin
          cp src/soap $out/bin/soap
          chmod +x $out/bin/soap
          wrapProgram $out/bin/soap \
            --prefix PATH : ${
              pkgs.lib.makeBinPath [
                pkgs.curl
                pkgs.libxml2.bin
                pkgs.xmlstarlet
                pkgs.gnugrep
              ]
            }
        '';
        meta.mainProgram = "soap";
      };
    in
    {
      home.packages = with pkgs; [
        autokuma # provides the `kuma` CLI, used by `nur kuma-cli config`
        diffr
        # doctl
        # extraNodePackages.prettier
        # fish
        # gcc9
        # gnumake
        go
        gum
        jet
        # md-tangle
        # minio-client
        nodejs
        # postman
        # runme
        sqlite
        # sqsh
        soap-cli
        vscode
        yq-go # used by scripts/k8s-bootstrap-argocd-repo.sh and k8s-post-process-manifests.nu
      ];
    };
}

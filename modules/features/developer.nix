{ ... }:
{
  flake.types.generic.feature-options.developer =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "developer feature";

  flake.modules.homeManager.developer =
    {
      config,
      lib,
      pkgs,
      ...
    }:
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
            --prefix PATH : ${pkgs.lib.makeBinPath [
              pkgs.curl
              pkgs.libxml2.bin
              pkgs.xmlstarlet
              pkgs.gnugrep
            ]}
        '';
        meta.mainProgram = "soap";
      };
    in
    {
      config = lib.mkIf config.host.features.developer.enable {
        home.packages = with pkgs; [
          age
          # argo-workflows
          # argocd
          # arkade
          # colmena
          # dbx
          dbeaver-bin
          # devpod
          # devpod-desktop
          devspace
          devenv
          diffr
          dig
          distrobox
          # dnschef
          # dnslookup
          # dnsmap
          docker
          # docker-compose
          doctl
          earthly
          # extraNodePackages.prettier
          # fish
          # gcc9
          gh
          gh-cal
          gh-dash
          gh-f
          # gnumake
          # gitu
          go
          gum
          jet
          k3d
          k9s
          # krew
          # kty
          kubernetes-helm
          kubectl
          kubernix
          kubeseal
          kustomize
          # lens
          # md-tangle
          minio-client
          # mr
          nixd
          nmap
          nodejs
          # openjdk
          # openjdk17
          # podman
          # podman-desktop
          # podman-tui
          # postman
          runme
          sqlite
          sops
          # sqsh
          soap-cli
          ssh-to-age
          ssh-to-pgp
          # tilt
          # vcluster
          # virtualbox
        ];
      };
    };
}

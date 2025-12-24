{ ... }: {
  flake.types.generic.feature-options.developer = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "developer feature";

  flake.modules.homeManager.developer = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.developer.enable {
      home.packages = with pkgs; [
        age
        argo-workflows
        argocd
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
        kty
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
        # python3
        runme
        sqlite
        sops
        # sqsh
        ssh-to-age
        ssh-to-pgp
        # tilt
        # vcluster
        # virtualbox
      ];
    };
  };
}

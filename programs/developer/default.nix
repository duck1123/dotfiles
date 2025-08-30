{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.developer.enable {
    home.packages = with pkgs; [
      age
      argo
      argocd
      # arkade
      # dbx
      dbeaver-bin
      # devpod
      # devpod-desktop
      devspace
      dig
      # dnschef
      # dnslookup
      # dnsmap
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
      kubernetes-helm
      kubectl
      kubernix
      kubeseal
      kustomize
      # lens
      # md-tangle
      # mr
      nixd
      nmap
      nodejs
      # openjdk
      # openjdk17
      # podman
      # podman-desktop
      # podman-tui
      # python3
      runme
      sqlite
      sops
      # sqsh
      ssh-to-age
      ssh-to-pgp
      # vcluster
      # virtualbox
    ];
  };
}

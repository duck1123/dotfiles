{ pkgs, ... }: {
  home.packages = with pkgs; [
    argo
    argocd
    # arkade
    azure-cli
    azure-cli-extensions.powerbidedicated
    dbeaver-bin
    devpod
    devpod-desktop
    devspace
    dig
    # dnschef
    # dnslookup
    # dnsmap
    # doctl
    earthly
    # extraNodePackages.prettier
    # gnumake
    gitu
    go
    k3d
    k9s
    kchmviewer
    # krew
    kubernetes-helm
    kubectl
    kustomize
    # lens
    md-tangle
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
    # sqsh
    # vcluster
    # virtualbox
  ];
}

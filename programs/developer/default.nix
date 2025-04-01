{ pkgs, ... }: {
  home.packages = with pkgs; [
    argo
    argocd
    # arkade
    (azure-cli.withExtensions [
      azure-cli-extensions.azure-devops
      azure-cli-extensions.powerbidedicated
    ])
    databricks-cli
    databricks-sql-cli
    # dbx
    dbeaver-bin
    # devpod
    # devpod-desktop
    devspace
    dig
    # dnschef
    # dnslookup
    # dnsmap
    doctl
    earthly
    # extraNodePackages.prettier
    gh
    gh-cal
    gh-dash
    gh-f
    # gnumake
    # gitu
    go
    k3d
    k9s
    kchmviewer
    # krew
    kubernetes-helm
    kubectl
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
    # sqsh
    # vcluster
    # virtualbox
  ];
}

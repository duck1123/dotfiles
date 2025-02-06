{ pkgs, ... }: {
  home.packages = with pkgs; [
    argo
    argocd
    # arkade

    # Next generation multi-platform command line experience for Azure
    # azure-cli

    # Universal SQL Client for developers, DBA and analysts. Supports MySQL, PostgreSQL, MariaDB, SQLite, and more
    dbeaver-bin

    devpod
    devpod-desktop

    devspace

    # Domain name server
    dig

    # Highly configurable DNS proxy for penetration testers and malware analysts
    # dnschef

    # Simple command line utility to make DNS lookups to the specified server
    # dnslookup

    # Scan for subdomains using brute-force techniques
    # dnsmap

    # A command line tool for DigitalOcean services
    # doctl

    earthly

    # extraNodePackages.prettier

    # gnumake

    gitu

    go

    k3d
    k9s

    # krew
    kubernetes-helm
    kubectl
    kustomize

    # Kubernetes IDE
    # lens

    md-tangle

    # mr

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

    # Command line tool for querying Sybase/MSSQL databases
    # sqsh

    # Create fully functional virtual Kubernetes clusters
    # vcluster

    # virtualbox
  ];
}

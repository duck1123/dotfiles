# NixOS hosts (tab completion)
def nixos-hosts [] {
    ["edgenix", "inspernix", "nasnix", "nixmini", "powerspecnix", "vidcentre"]
}

# Home-manager hosts (tab completion)
def home-hosts [] {
    ["edgenix", "inspernix", "nasnix", "nixmini", "powerspecnix", "steamdeck", "vallen", "vidcentre"]
}

# Map a host's friendly name to its flake attribute name
def host-flake-name [host: string]: nothing -> string {
    match $host {
        "vallen" => "VAVIRL-PW0BWNQ8",
        _ => $host
    }
}

# Map a host to its primary user
def host-user [host: string]: nothing -> string {
    match $host {
        "steamdeck" => "deck",
        "vallen" | "VAVIRL-PW0BWNQ8" => "drenfer",
        _ => "duck"
    }
}

def all-installables []: nothing -> list<string> {
    [
        ".#homeConfigurations.duck@inspernix.activationPackage"
        ".#homeConfigurations.duck@nasnix.activationPackage"
        ".#homeConfigurations.duck@powerspecnix.activationPackage"
        ".#homeConfigurations.deck@steamdeck.activationPackage"
        ".#homeConfigurations.drenfer@VAVIRL-PW0BWNQ8.activationPackage"
        ".#nixosConfigurations.inspernix.config.system.build.toplevel"
        ".#nixosConfigurations.nasnix.config.system.build.toplevel"
        ".#nixosConfigurations.powerspecnix.config.system.build.toplevel"
    ]
}

# Build configurations (local, --host <name>, or --all)
export def "nur build" [
    --host: string@nixos-hosts = ""
    --all = false
    --fallback = false
]: nothing -> nothing {
    if $all and ($host | is-not-empty) {
        error make {
            msg: "--all and --host are mutually exclusive"
            label: {text: "--host provided here", span: (metadata $host).span}
        }
    }
    if $all {
        ^nom build ...(if $fallback { ["--fallback"] } else { [] }) ...(all-installables)
    } else {
        nur build-home --host $host --fallback $fallback
        nur build-os --host $host --fallback $fallback
    }
}

# Build home configurations (local, --host <name>, or --all)
export def "nur build-home" [
    --host: string@home-hosts = ""
    --all = false
    --fallback = false
] {
    if $all and not ($host | is-empty) {
        error make { msg: "--all and --host are mutually exclusive" }
    }
    let args = (if $fallback { ["--fallback"] } else { [] })
    if $all {
        nur build-home --host inspernix --fallback $fallback
        nur build-home --host nasnix --fallback $fallback
        nur build-home --host powerspecnix --fallback $fallback
        nur build-home --host steamdeck --fallback $fallback
        nur build-home --host vallen --fallback $fallback
    } else if ($host | is-empty) {
        ^nh home build ...$args .
    } else {
        let user = (host-user $host)
        let flake_host = (host-flake-name $host)
        ^nom build ...$args $".#homeConfigurations.($user)@($flake_host).activationPackage"
    }
}

# Build NixOS configurations (local, --host <name>, or --all)
export def "nur build-os" [
    --host: string@nixos-hosts = ""
    --all = false
    --fallback = false
] {
    if $all and not ($host | is-empty) {
        error make { msg: "--all and --host are mutually exclusive" }
    }
    let args = (if $fallback { ["--fallback"] } else { [] })
    if $all {
        nur build-os --host inspernix --fallback $fallback
        nur build-os --host nasnix --fallback $fallback
        nur build-os --host powerspecnix --fallback $fallback
    } else if ($host | is-empty) {
        ^nh os build ...$args .
    } else {
        ^nom build ...$args $".#nixosConfigurations.($host).config.system.build.toplevel"
    }
}

# Run validation on the project
export def "nur check" []: nothing -> nothing {
    ^nix flake check
}

# Build all targets (check + build --all)
export def "nur ci" []: nothing -> nothing {
    nur check
    nur build --all true
}

# Format all .nix files using nixfmt
export def "nur format" []: nothing -> nothing {
    ^find . -name '*.nix' -exec nixfmt {} +
}

# Install cert-manager into the current cluster
export def "nur install cert-manager" []: nothing -> nothing {
    let version = "1.14.4"
    let url = $"https://github.com/cert-manager/cert-manager/releases/download/v($version)/cert-manager.yaml"
    let result = (kubectl apply -f $url | complete)
    if $result.exit_code != 0 {
        error make {
            msg: $result.stderr
            label: {text: "kubectl apply failed", span: (metadata $url).span}
        }
    }
}

# List GPG secret keys
export def "nur secrets list-keys" []: nothing -> string {
    gpg --list-secret-keys --keyid-format=long
}

# Read windows key from firmware
export def "nur secrets windows-key" []: nothing -> string {
    sudo grep -Eao '(-?[A-Z0-9]{5}){5}' /sys/firmware/acpi/tables/MSDM
}

# Switch both home-manager and NixOS (local if no --host, otherwise remote)
export def "nur switch" [--host: string@nixos-hosts = ""] {
    nur switch-home --host $host
    nur switch-os --host $host
}

# Switch the home-manager configuration (local if no --host, otherwise remote)
export def "nur switch-home" [
  --host: string@home-hosts = ""
] {
    if ($host | is-empty) {
        let ts = (date now | format date '%s')
        ^home-manager switch --flake . -b $"backup.($ts)" --show-trace
    } else {
        do-switch-remote-home $host (host-user $host)
    }
}

# Switch the NixOS configuration (local if no --host, otherwise remote)
export def "nur switch-os" [--host: string@nixos-hosts = ""] {
    if ($host | is-empty) {
        try {
            ^sudo nixos-rebuild switch --flake . --show-trace
        } catch {|_e|
            print "\n=== systemd journal (last 50 lines) ==="
            ^journalctl -xe --no-pager -n 50
            exit $env.LAST_EXIT_CODE
        }
    } else {
        do-switch-remote-os $host
    }
}

# Build NixOS config and set as boot default (local if no --host, otherwise remote)
export def "nur boot-os" [--host: string@nixos-hosts = ""] {
    if ($host | is-empty) {
        ^sudo nixos-rebuild boot --flake . --show-trace
    } else {
        do-boot-remote-os $host
    }
}

# Apply NixOS config on a fresh install where nix-command is not yet enabled
export def "nur bootstrap-os" [] {
    with-env { NIX_CONFIG: "experimental-features = nix-command flakes" } {
        ^nh os boot .
    }
}

# Show package changes between current system and new build on a remote host
export def "nur diff-os" [--host: string@nixos-hosts] {
    if ($host | is-empty) {
        error make { msg: "diff-os requires --host <hostname>" }
    }
    do-diff-remote-os $host
}

# Show what would change without applying (local if no --host, otherwise remote dry-run)
export def "nur dry-run-os" [--host: string@nixos-hosts = ""] {
    if ($host | is-empty) {
        ^sudo nixos-rebuild dry-activate --flake . --show-trace
    } else {
        with-env { NIX_SSHOPTS: "-t" } {
            ^nixos-rebuild dry-activate --flake $".#($host)" --target-host $host --build-host localhost --sudo --ask-sudo-password
        }
    }
}

# Helper: diff OS closures between local build and remote host
def do-diff-remote-os [host: string] {
    print $"Building new NixOS configuration for ($host)..."
    let new_system = (
        ^nom build $".#nixosConfigurations.($host).config.system.build.toplevel" --no-link --print-out-paths
        o+e>| lines
        | where { |l| $l | str starts-with "/nix/store" }
        | last
    )
    let current_system = (^ssh $host "readlink -f /nix/var/nix/profiles/system" | str trim)
    if ($new_system | is-empty) or ($current_system | is-empty) {
        print "ERROR: Could not determine system paths"
        print $"New system path: ($new_system)"
        print $"Current system path: ($current_system)"
    } else {
        print "\n=== Package Changes ==="
        print $"Current: ($current_system)"
        print $"New:     ($new_system)\n"
        ^nix store diff-closures $current_system $new_system
    }
}

# Helper: build NixOS config locally with nom, copy and switch on remote host
def do-switch-remote-os [host: string] {
    print --stderr $"Building NixOS configuration for ($host) with nom..."
    let out_link = $"/tmp/($host)-system-result"
    ^nom build $".#nixosConfigurations.($host).config.system.build.toplevel" --out-link $out_link
    let system_path = (^realpath $out_link | str trim)
    if ($system_path | str starts-with "/nix/store") {
        print --stderr $"Copying system to ($host): ($system_path)"
        ^nix copy --to $"ssh://($host)" $system_path
        print --stderr $"Activating on ($host) \(will prompt for sudo password\)..."
        ^ssh -t $host $"sudo nix-env -p /nix/var/nix/profiles/system --set ($system_path) && sudo ($system_path)/bin/switch-to-configuration switch"
    } else {
        print --stderr "ERROR: Failed to build or get system path"
    }
}

# Helper: build NixOS config locally with nom, copy and set as boot default on remote host
def do-boot-remote-os [host: string] {
    print --stderr $"Building NixOS configuration for ($host) with nom..."
    let out_link = $"/tmp/($host)-system-result"
    ^nom build $".#nixosConfigurations.($host).config.system.build.toplevel" --out-link $out_link
    let system_path = (^realpath $out_link | str trim)
    if ($system_path | str starts-with "/nix/store") {
        print --stderr $"Copying system to ($host): ($system_path)"
        ^nix copy --to $"ssh://($host)" $system_path
        print --stderr $"Setting boot default on ($host) \(will prompt for sudo password\)..."
        ^ssh -t $host $"sudo nix-env -p /nix/var/nix/profiles/system --set ($system_path) && sudo ($system_path)/bin/switch-to-configuration boot"
    } else {
        print --stderr "ERROR: Failed to build or get system path"
    }
}

# Helper: build home-manager package locally, copy and activate on remote host
def do-switch-remote-home [host: string, user: string] {
    print --stderr $"Building home-manager activation package for ($host)..."
    let out_link = $"/tmp/($host)-home-result"
    let flake_host = (host-flake-name $host)
    ^nom build $".#homeConfigurations.($user)@($flake_host).activationPackage" --out-link $out_link
    let activation_path = (^realpath $out_link | str trim)
    if ($activation_path | str starts-with "/nix/store") {
        print --stderr $"Copying to ($host): ($activation_path)"
        ^nix copy --to $"ssh://($host)" $activation_path
        print --stderr $"Activating on ($host)..."
        let ts = (date now | format date '%s')
        ^ssh $host $"HOME_MANAGER_BACKUP_EXT=backup.($ts) ($activation_path)/activate"
    } else {
        print --stderr "ERROR: Failed to build activation package or invalid path"
    }
}

# Install ArgoCD into the current cluster
export def "nur install argocd" [] {
    ^kubectl create namespace argocd
    ^kubectl apply -n argocd -f https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml
}

# Show listening ports
export def "nur show-ports" [] {
    ^netstat -ltnp
}

# Watch kubernetes namespaces
export def "nur watch-namespaces" [] {
    ^watch kubectl get namespaces
}

# Watch kubernetes pods across all namespaces
export def "nur watch-pods" [] {
    ^watch kubectl get pods --all-namespaces
}

# Wipe k3s directories
export def "nur k3s wipe" [] {
    ^sudo rm -rf /etc/rancher/k3s
    ^sudo rm -rf /var/lib/rancher/k3s
}

# Build nixidy manifests and write to kubernetes/manifests/
export def "nur k8s switch-charts" [] {
    ^sh scripts/k8s-switch-charts.sh
}

# Commit and push generated manifests to the private manifests repo
export def "nur k8s push" [] {
    ^sh scripts/k8s-push-manifests.sh
}

# Build manifests and push to private repo (switch-charts + push)
export def "nur k8s deploy" [] {
    nur k8s-switch-charts
    nur k8s-push
}

# Decrypt kubernetes secrets to secrets/k8s.yaml (plaintext — do not commit)
export def "nur k8s decrypt" [] {
    ^sops --decrypt secrets/k8s.enc.yaml | save -f secrets/k8s.yaml
}

# Encrypt secrets/k8s.yaml back to secrets/k8s.enc.yaml
export def "nur k8s encrypt" [] {
    ^sops --encrypt secrets/k8s.yaml | save -f secrets/k8s.enc.yaml
}

# Edit kubernetes secrets in-place (no plaintext file written)
export def "nur k8s edit-secrets" [] {
    ^sops secrets/k8s.enc.yaml
}

# Generate SSH deploy key pair for argo-manifests
export def "nur k8s generate-deploy-key" [] {
    ^sh scripts/k8s-generate-deploy-key.sh
}

# Apply ArgoCD repository credential for argo-manifests (one-time bootstrap)
export def "nur k8s bootstrap-argocd-repo" [] {
    ^sh scripts/k8s-bootstrap-argocd-repo.sh
}

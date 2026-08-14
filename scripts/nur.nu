# NixOS hosts (tab completion)
def nixos-hosts []: nothing -> list<string> {
  [
    edgenix
    inspernix
    nasnix
    nixmini
    powerspecnix
  ]
}

# Home-manager hosts (tab completion)
def home-hosts []: nothing -> list<string> {
  [
    edgenix
    inspernix
    nasnix
    nixmini
    powerspecnix
    steamdeck
    vallen
  ]
}

# Map a host's friendly name to its flake attribute name
def host-flake-name [host: string]: nothing -> string {
  match $host {
    vallen => "VAVIRL-PW0BWNQ8",
    _ => $host
  }
}

# Map a host to its primary user
def host-user [host: string]: nothing -> string {
  match $host {
    steamdeck => "deck"
    vallen | "VAVIRL-PW0BWNQ8" => "drenfer",
    _ => "duck"
  }
}

def all-home-installables []: nothing -> list<string> {
  home-hosts | each {|host|
    let flake_host = (host-flake-name $host)
    let user = (host-user $host)
    $".#homeConfigurations.($user)@($flake_host).activationPackage"
  }
}

def all-os-installables []: nothing -> list<string> {
  nixos-hosts | each {|host|
    $".#nixosConfigurations.($host).config.system.build.toplevel"
  }
}

# Valid target names for build/switch (tab completion)
def only-targets []: nothing -> list<string> { [os home k8s] }

# Validate target names and resolve which targets are selected.
# No targets means the historical default: os and home (k8s is opt-in only).
def parse-only [only: list<string>]: nothing -> record<os: bool, home: bool, k8s: bool> {
  let invalid = ($only | where {|o| $o not-in (only-targets)})

  if ($invalid | is-not-empty) {
    error make {
      msg: $"invalid target: ($invalid | str join ', ') \(expected one of: os, home, k8s\)"
    }
  }

  if ($only | is-empty) {
    {os: true, home: true, k8s: false}
  } else {
    {os: ("os" in $only), home: ("home" in $only), k8s: ("k8s" in $only)}
  }
}

# Whether this machine is a NixOS system (has os-level Nix configuration to switch/build).
def is-nixos-host []: nothing -> bool {
  "/etc/NIXOS" | path exists
}

# Validate and resolve which targets to act on.
# When no target was explicitly requested, on a purely local invocation (no --host, no --all)
# silently drop os on hosts that aren't NixOS (e.g. vallen, steamdeck) instead of failing for
# lack of support. An explicit target, or --host/--all, is a deliberate request and still fails loudly.
def resolve-targets [
  only: list<string>
  host: string
  --all
]: nothing -> record<os: bool, home: bool, k8s: bool> {
  mut targets = (parse-only $only)

  if ($only | is-empty) and ($host | is-empty) and (not $all) and $targets.os and not (is-nixos-host) {
    print "note: this host has no NixOS configuration, skipping os"
    $targets.os = false
  }

  $targets
}

# Build configurations (local, --host <name>, or --all). Pass os/home/k8s to restrict, e.g. `nur build home k8s`.
export def "nur build" [
  --all
  --fallback
  --host: string@home-hosts = ""
  ...only: string@only-targets
]: nothing -> nothing {
  if $all and ($host | is-not-empty) {
    error make {
      msg: "--all and --host are mutually exclusive"
      label: {text: "--host provided here", span: (metadata $host).span}
    }
  }

  let targets = (if $all { resolve-targets $only $host --all } else { resolve-targets $only $host })

  if $targets.k8s and ($host | is-not-empty) {
    error make {
      msg: "k8s target cannot be combined with --host (k8s has no remote build)"
      label: {text: "--host provided here", span: (metadata $host).span}
    }
  }

  let args = (if $fallback { ["--fallback"] } else { [] })

  if $all {
    let installables = [
      ...(if $targets.os { all-os-installables } else { [] })
      ...(if $targets.home { all-home-installables } else { [] })
    ]

    if ($installables | is-not-empty) {
      ^nom build ...$args --no-link ...$installables
    }
  } else if ($host | is-empty) {
    if $targets.home { ^nh home build ...$args . }
    if $targets.os { ^nh os build ...$args . }
  } else {
    if $targets.home {
      let user = (host-user $host)
      let flake_host = (host-flake-name $host)
      ^nom build ...$args $".#homeConfigurations.($user)@($flake_host).activationPackage"
    }

    if $targets.os {
      ^nom build ...$args $".#nixosConfigurations.($host).config.system.build.toplevel"
    }
  }

  if $targets.k8s {
    nur k8s switch-charts
  }
}

# Run validation on the project
export def "nur check" []: nothing -> nothing {
  ^nix flake check
}

# Build all targets (check + build --all)
export def "nur ci" []: nothing -> nothing {
  nur check
  nur build --all
}

# Format all .nix files using nixfmt
export def "nur format" []: nothing -> nothing {
  ^find . -name '*.nix' -exec nixfmt {} + # nu-lint-ignore: unhandled_external_error
}

# Lint all .nix files using statix
export def "nur lint" []: nothing -> nothing {
  ^statix check .
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

# Switch home-manager, NixOS, and/or k8s manifests (local if no --host, otherwise remote).
# Pass os/home/k8s to restrict which targets are switched, e.g. `nur switch home k8s`.
# Use --boot to set the os target as boot default instead of activating immediately (safe for slow activations).
export def "nur switch" [
  --boot
  --host: string@home-hosts = ""
  ...only: string@only-targets
]: nothing -> nothing {
  let targets = (resolve-targets $only $host)

  if $targets.k8s and ($host | is-not-empty) {
    error make {
      msg: "k8s target cannot be combined with --host (k8s has no remote switch)"
      label: {text: "--host provided here", span: (metadata $host).span}
    }
  }

  if $boot and not $targets.os {
    error make {
      msg: "--boot only applies to the os target"
      label: {text: "--boot provided here", span: (metadata $boot).span}
    }
  }

  if $targets.home {
    if ($host | is-empty) {
      let ts = (date now | format date '%s')
      ^home-manager switch --flake . -b $"backup.($ts)" --show-trace
    } else {
      do-switch-remote-home $host (host-user $host)
    }
  }

  if $targets.os {
    if ($host | is-empty) {
      if $boot {
        ^sudo nixos-rebuild boot --flake . --show-trace
      } else {
        try {
          ^sudo nixos-rebuild switch --flake . --show-trace
        } catch {|e|
          print "\n=== systemd journal (last 50 lines) ==="
          ^journalctl -xe --no-pager -n 50
          error make {
            msg: $e.msg
            label: {text: "nixos-rebuild switch failed", span: (metadata $host).span}
          }
        }
      }
    } else {
      if $boot {
        do-boot-remote-os $host
      } else {
        do-switch-remote-os $host
      }
    }
  }

  if $targets.k8s {
    nur k8s deploy
  }
}

# Apply NixOS config on a fresh install where nix-command is not yet enabled
export def "nur bootstrap-os" []: nothing -> nothing {
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
def do-switch-remote-home [
  host: string
  user: string
] {
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

# Clone the private manifests repo into kubernetes/manifests/ (one-time setup)
export def "nur k8s manifests init" []: nothing -> nothing {
  ^git clone git@github.com:duck1123/argo-manifests.git kubernetes/manifests
}

# Pull the latest changes into the kubernetes/manifests/ checkout
export def "nur k8s manifests sync" []: nothing -> nothing {
  if not ("kubernetes/manifests/.git" | path exists) {
    error make {
      msg: "kubernetes/manifests is not a git repository. Run 'nur k8s manifests init' first."
    }
  }
  ^git -C kubernetes/manifests pull
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
  nur k8s switch-charts
  nur k8s push
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

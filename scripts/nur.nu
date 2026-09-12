# NixOS hosts (tab completion)
def nixos-hosts []: nothing -> list<string> {
  [
    edgenix
    inspernix
    nasnix
    nixmini
    powerspecnix
    vavirl-pw0bwnq8
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
  --tarball  # Build the NixOS-WSL tarball for importing with wsl --import
  ...only: string@only-targets
]: nothing -> nothing {
  if $tarball {
    if $all {
      error make {msg: "--tarball and --all are mutually exclusive"}
    }
    let resolved = if ($host | is-not-empty) {
      host-flake-name $host | str lowercase
    } else {
      sys host | get hostname | str lowercase
    }
    let args = (if $fallback { ["--fallback"] } else { [] })
    ^nom build ...$args $".#nixosConfigurations.($resolved).config.system.build.tarballBuilder"
    return
  }

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

# Lint all .nix files using statix. `nur lint nushell` exists separately
# (ported from k3s-fleetops) but isn't wired in here yet -- the existing
# nur.nu content currently has ~43 pre-existing nu-lint warnings unrelated
# to this migration, so enabling it by default would break `nur lint`/`nur
# ci` project-wide. Run `nur lint nushell` directly, or fold it in here once
# that pre-existing backlog is cleaned up.
export def "nur lint" []: nothing -> nothing {
  ^statix check .
}

# Lint all .nix files using statix (same as `nur lint`, for parity with the
# `nur lint nushell` naming ported from k3s-fleetops)
export def "nur lint nix" []: nothing -> nothing {
  ^statix check .
}

# Lint all nushell files, failing on warnings (not just errors). Not yet
# wired into `nur lint`/`nur ci` -- see the note on `nur lint`.
export def "nur lint nushell" [] {
  let response = nu-lint | complete

  if $response.exit_code != 0 {
    print $"Linting failed with exit code ($response.exit_code)"
    print $response.stderr
    exit $response.exit_code # nu-lint-ignore: exit_only_in_main
  }

  # nu-lint always exits 0, even with warnings, so check its summary line ourselves
  let warning_matches = $response.stdout | parse --regex 'Found (?<warnings>\d+) warning'
  let warning_count = if ($warning_matches | is-empty) { 0 } else { $warning_matches | get --optional warnings.0 | default "0" | into int }

  if $warning_count > 0 {
    print -e $response.stdout
    print -e $"nu-lint found ($warning_count) warning\(s\)"
    exit 1 # nu-lint-ignore: exit_only_in_main
  }
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
  # --filename-override matches creation rules against the real target path --
  # without it sops matches against secrets/k8s.yaml, which falls through to
  # the generic secrets/*.yaml rule instead of the dedicated k8s.enc.yaml one
  # (wrong recipient set: missing the k8s primary key, adding nixmini).
  ^sops --encrypt --filename-override secrets/k8s.enc.yaml secrets/k8s.yaml | save -f secrets/k8s.enc.yaml
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

# ─── App management (ported from k3s-fleetops) ──────────────────────────────
# Application templates still live in the k3s-fleetops flake input (see
# modules/kubernetes/default.nix) rather than locally under applications/,
# so these resolve the flake input's store path first instead of reading a
# local applications/default.nix like the fleetops original did.

def k3s-fleetops-path []: nothing -> string {
  try {
    ^nix eval --raw --impure --expr 'let flake = builtins.getFlake (toString ./.); in flake.inputs.k3s-fleetops.outPath'
    | str trim
  } catch { |err|
    error make {msg: $"Failed to resolve k3s-fleetops flake input path: ($err.msg)"}
  }
}

# Every app name registered in k3s-fleetops' applications/default.nix imports list
def "nu-complete apps" []: nothing -> list<string> {
  try {
    open --raw $"(k3s-fleetops-path)/applications/default.nix"
    | lines
    | each { str trim }
    | where {|line| $line | str starts-with './' }
    | each {|line| $line | str replace --all --regex '^\./|\.nix$' '' }
    | uniq
    | sort
  } catch { |err|
    error make {msg: $"Failed to read applications/default.nix: ($err.msg)"}
  }
}

# List every app name accepted by `nur apps restart` (one per line)
export def "nur apps list" [] {
  nu-complete apps | str join "\n" | print
}

# Restart an app's pod(s) — rolls its Deployment (falling back to StatefulSet) in
# the namespace of the same name, which is the mkArgoApp default and covers the
# common single-workload case. Apps with a non-default namespace or several
# workloads (Helm charts, nix-csi) aren't resolved here — restart those manually
# with kubectl.
export def "nur apps restart" [
  name: string   # App name — see `nur apps list`
]: nothing -> nothing {
  if not ($name in (nu-complete apps)) {
    error make {msg: $"Unknown app ($name). Run `nur apps list` to see valid names."}
  }

  if (^kubectl get deployment $name -n $name | complete).exit_code == 0 {
    try {
      ^kubectl rollout restart $"deployment/($name)" -n $name
      ^kubectl rollout status $"deployment/($name)" -n $name
    } catch { |err|
      error make {msg: $"Failed to restart deployment/($name): ($err.msg)"}
    }
    return
  }

  if (^kubectl get statefulset $name -n $name | complete).exit_code == 0 {
    try {
      ^kubectl rollout restart $"statefulset/($name)" -n $name
      ^kubectl rollout status $"statefulset/($name)" -n $name
    } catch { |err|
      error make {msg: $"Failed to restart statefulset/($name): ($err.msg)"}
    }
    return
  }

  error make {
    msg: $"No Deployment or StatefulSet named ($name) found in namespace ($name). ($name) may use a non-default namespace or ship multiple workloads \(Helm chart, nix-csi\) — restart it manually with kubectl."
  }
  return
}

# ─── AutoKuma / kuma-cli (ported from k3s-fleetops) ─────────────────────────

const AUTOKUMA_NS = "autokuma"
const AUTOKUMA_SECRET = "autokuma-kuma-credentials"
const UPTIME_KUMA_NS = "uptime-kuma"
const UPTIME_KUMA_INGRESS = "uptime-kuma"

def kuma-cli-config-path []: nothing -> string {
  let base = ($env.XDG_CONFIG_HOME? | default $"($env.HOME)/.config")
  $"($base)/kuma/config.toml"
}

# Write ~/.config/kuma/config.toml from the cluster: uptime-kuma's ingress
# host for `url`, and the same SOPS-managed credentials autokuma itself uses
# (secret ($AUTOKUMA_SECRET) in namespace ($AUTOKUMA_NS)) for `username`/`password`.
# Run this once (and again after rotating the password) so `kuma` (kuma-cli)
# works without passing --url/--username/--password on every invocation.
export def "nur kuma-cli config" [] {
  let host = (
    try {
      ^kubectl get ingress $UPTIME_KUMA_INGRESS -n $UPTIME_KUMA_NS -o jsonpath='{.spec.rules[0].host}'
      | str trim
    } catch { |err|
      error make {msg: $"Failed to query ($UPTIME_KUMA_INGRESS) ingress: ($err.msg)"}
    }
  )
  if ($host | is-empty) {
    error make {msg: $"Could not find ($UPTIME_KUMA_INGRESS) ingress in namespace ($UPTIME_KUMA_NS)"}
  }

  let username = (
    try {
      ^kubectl get secret $AUTOKUMA_SECRET -n $AUTOKUMA_NS -o jsonpath='{.data.USERNAME}'
      | ^base64 -d
    } catch { |err|
      error make {msg: $"Failed to read AutoKuma username secret: ($err.msg)"}
    }
  )
  let password = (
    try {
      ^kubectl get secret $AUTOKUMA_SECRET -n $AUTOKUMA_NS -o jsonpath='{.data.PASSWORD}'
      | ^base64 -d
    } catch { |err|
      error make {msg: $"Failed to read AutoKuma password secret: ($err.msg)"}
    }
  )
  if ($username | is-empty) or ($password | is-empty) {
    error make {msg: (
      $"Secret ($AUTOKUMA_SECRET) in namespace ($AUTOKUMA_NS) has no USERNAME/PASSWORD yet. "
      + "Set services.autokuma.kuma.username/password from secrets.autokuma.* "
      + "(nur k8s edit-secrets), then nur switch, before running this."
    )}
  }

  let path = (kuma-cli-config-path)
  try {
    mkdir ($path | path dirname)
    (
      {
        url: $"https://($host)/",
        username: $username,
        password: $password,
      }
      | to toml
      | save --force $path
    )
    ^chmod 600 $path
  } catch { |err|
    error make {msg: $"Failed to write ($path): ($err.msg)"}
  }
  print $"Wrote ($path)"
}

# ─── ArgoCD (ported from k3s-fleetops) ───────────────────────────────────────
# Bootstrap manifests live in kubernetes/infra-manifests/ (mirrors fleetops'
# infra-manifests/, copied in ahead of the GitOps cutover). The live 00-master
# Application still points at k3s-fleetops today -- see the consolidation plan --
# so `nur argocd apply-master` here is not yet a functional replacement for
# that until the cutover happens.

# Download latest stable ArgoCD install manifest to kubernetes/infra-manifests/argocd/install.yaml
export def "nur argocd update-manifest" [] {
  try {
    mkdir kubernetes/infra-manifests/argocd
  } catch { |err|
    error make {msg: $"Failed to create kubernetes/infra-manifests/argocd: ($err.msg)"}
  }
  print "Fetching latest stable ArgoCD manifest..."
  try {
    (
      http get "https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml"
      | save --force kubernetes/infra-manifests/argocd/install.yaml
    )
  } catch { |err|
    error make {msg: $"Failed to download ArgoCD manifest: ($err.msg)"}
  }
  print "Done. Commit kubernetes/infra-manifests/argocd/install.yaml to pin the version."
}

# Install or upgrade ArgoCD into the cluster (safe to re-run)
export def "nur argocd install" [] {
  if not ("kubernetes/infra-manifests/argocd/install.yaml" | path exists) {
    print "install.yaml not found, downloading..."
    nur argocd update-manifest
  }
  try {
    ^kubectl apply --server-side --force-conflicts -k kubernetes/infra-manifests/argocd/
    print "Waiting for argocd-server rollout..."
    ^kubectl rollout status deployment/argocd-server -n argocd --timeout=120s
  } catch { |err|
    error make {msg: $"ArgoCD install failed: ($err.msg)"}
  }
  print "ArgoCD install complete"
}

# Register 00-master app with ArgoCD (triggers full sync)
export def "nur argocd apply-master" []: nothing -> nothing {
  try {
    ^kubectl apply -f kubernetes/infra-manifests/00-master.yaml
  } catch { |err|
    error make {msg: $"Failed to apply 00-master: ($err.msg)"}
  }
}

# Force an immediate ArgoCD reconcile instead of waiting on its poll interval --
# unlike `apply-master`, this doesn't re-apply anything, it just tells ArgoCD to
# re-diff against git right now. With no name, refreshes every Application
# (00-master and all its children); pass one to target just that app, e.g.
# `nur argocd refresh ditto-relay`.
export def "nur argocd refresh" [name?: string]: nothing -> nothing {
  try {
    if ($name | is-empty) {
      ^kubectl annotate application -n argocd --all argocd.argoproj.io/refresh=hard --overwrite
    } else {
      ^kubectl annotate application -n argocd $name argocd.argoproj.io/refresh=hard --overwrite
    }
  } catch { |err|
    error make {msg: $"Failed to refresh ArgoCD application\(s\): ($err.msg)"}
  }
}

# Trigger an actual ArgoCD sync, not just a refresh -- a refresh only recomputes
# the diff against git, it doesn't apply anything or run PostSync hooks. Uses
# `argocd`'s --core mode, which talks to the k8s API directly via the local
# kubeconfig context -- no port-forward/login needed. With no name, syncs every
# Application; pass one to target just that app, e.g. `nur argocd sync bookorbit`.
export def "nur argocd sync" [name?: string]: nothing -> nothing {
  if ($name | is-empty) {
    let apps = (
      try {
        ^kubectl get applications -n argocd -o jsonpath='{.items[*].metadata.name}'
        | str trim
        | split row " "
      } catch { |err|
        error make {msg: $"Failed to list ArgoCD applications: ($err.msg)"}
      }
    )
    ^argocd app sync ...$apps --core
  } else {
    ^argocd app sync $name --core
  }
}

# ─── Port-forwarding (ported from k3s-fleetops) ─────────────────────────────

# Port-forward ArgoCD UI to localhost:8080
export def "nur forward argocd" []: nothing -> nothing {
  try {
    ^kubectl port-forward svc/argocd-server -n argocd 8080:443
  } catch { |err|
    error make {msg: $"Port-forward failed: ($err.msg)"}
  }
}

# Expose Traefik dashboard on localhost:9000
export def "nur forward traefik" []: nothing -> nothing {
  let pod = (
    try {
      ^kubectl get pods --selector "app.kubernetes.io/name=traefik" --output=name | str trim
    } catch { |err|
      error make {msg: $"Failed to find traefik pod: ($err.msg)"}
    }
  )
  try {
    ^kubectl port-forward $pod 9000:9000
  } catch { |err|
    error make {msg: $"Port-forward failed: ($err.msg)"}
  }
}

# ─── PostgreSQL (ported from k3s-fleetops) ──────────────────────────────────

const PG_NS = "postgresql"
const PG_SECRET = "postgresql-password"
const PG_USER = "postgres"
const PG_PORT = "5432"

def pg-pod []: nothing -> string {
  let pod = (
    try {
      ^kubectl get pods -n $PG_NS -l "app.kubernetes.io/name=postgres" -o jsonpath='{.items[0].metadata.name}'
      | str trim
    } catch { |err|
      error make {msg: $"Failed to query PostgreSQL pod: ($err.msg)"}
    }
  )
  if ($pod | is-empty) {
    error make {msg: $"Could not find PostgreSQL pod in namespace ($PG_NS)"}
  }
  $pod
}

def pg-password []: nothing -> string {
  try {
    ^kubectl get secret $PG_SECRET -n $PG_NS -o jsonpath='{.data.adminPassword}'
    | ^base64 -d
    | str trim
  } catch { |err|
    error make {msg: $"Failed to fetch PostgreSQL password: ($err.msg)"}
  }
}

def pg-databases [pod: string, password: string]: nothing -> list<string> {
  try {
    ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" psql -h localhost -U $PG_USER -p $PG_PORT -t -A -c "SELECT datname FROM pg_database WHERE datistemplate = false ORDER BY datname;" postgres
    | lines
    | each { str trim }
    | where { |it| $it | is-not-empty }
  } catch { |err|
    error make {msg: $"Failed to list PostgreSQL databases: ($err.msg)"}
  }
}

# List PostgreSQL databases and their sizes
export def "nur postgres list" []: nothing -> table {
  let pod = (pg-pod)
  let password = (pg-password)
  print $"Namespace: ($PG_NS) | Pod: ($pod)"
  print ""
  try {
    ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" psql -h localhost -U $PG_USER -p $PG_PORT -t -A -F"," -c "SELECT datname, pg_size_pretty(pg_database_size(datname)) FROM pg_database WHERE datistemplate = false ORDER BY pg_database_size(datname) DESC;" postgres
    | lines
    | where { |it| $it | is-not-empty }
    | each { |line|
      let parts = ($line | split row ",")
      {name: ($parts | first | str trim), size: ($parts | last | str trim)}
    }
  } catch { |err|
    error make {msg: $"Failed to list databases: ($err.msg)"}
  }
}

# List available PostgreSQL backups on the postgresql-backups PVC
export def "nur postgres list-backups" []: nothing -> nothing {
  let pod_name = "postgresql-backup-lister"
  let pod_spec = {
    apiVersion: "v1"
    kind: "Pod"
    metadata: {name: $pod_name, namespace: $PG_NS}
    spec: {
      restartPolicy: "Never"
      containers: [{
        name: "lister"
        image: "pgvector/pgvector:pg17"
        command: ["/bin/bash", "-c", "ls -lht /backups/postgresql-backup-*.sql.gz 2>/dev/null || echo 'No backups found'"]
        volumeMounts: [{name: "backups", mountPath: "/backups"}]
      }]
      volumes: [{name: "backups", persistentVolumeClaim: {claimName: "postgresql-backups"}}]
    }
  }
  try {
    $pod_spec | to yaml | ^kubectl apply -f -
    ^kubectl -n $PG_NS wait --for=condition=Ready pods $pod_name --timeout=60s
    ^kubectl -n $PG_NS logs $pod_name
    ^kubectl -n $PG_NS delete pods $pod_name --ignore-not-found=true
  } catch { |err|
    error make {msg: $"Failed to list PostgreSQL backups: ($err.msg)"}
  } | ignore
}

# Backup PostgreSQL databases (omit --database to backup all)
export def "nur postgres backup" [
  --database: string = ""
  --output-dir: string = "./backups/postgresql"
]: nothing -> table {
  let pod = (pg-pod)
  let password = (pg-password)
  let timestamp = (date now | format date '%Y%m%d_%H%M%S')
  try {
    mkdir $output_dir
  } catch { |err|
    error make {msg: $"Failed to create ($output_dir): ($err.msg)"}
  }

  let dbs = if ($database | is-empty) {
    pg-databases $pod $password
  } else {
    [$database]
  }

  print $"=== PostgreSQL Backup ==="
  print $"Namespace: ($PG_NS) | Pod: ($pod)"
  print $"Output: ($output_dir) | Timestamp: ($timestamp)"
  print ""

  for db in $dbs {
    print $"Backing up: ($db)"
    let base = $"($output_dir)/($db)_($timestamp)"

    try {
      ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" pg_dump -h localhost -U $PG_USER -p $PG_PORT --clean --if-exists --create --format=plain --no-owner --no-privileges $db
      | ^gzip
      | save --raw --force $"($base).sql.gz"
    } catch { |err|
      error make {msg: $"Backup of ($db) \(plain\) failed: ($err.msg)"}
    }
    print $"  ✓ ($base).sql.gz"

    try {
      ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" pg_dump -h localhost -U $PG_USER -p $PG_PORT --clean --if-exists --create --format=custom --no-owner --no-privileges $db
      | save --raw --force $"($base).custom"
    } catch { |err|
      error make {msg: $"Backup of ($db) \(custom\) failed: ($err.msg)"}
    }
    print $"  ✓ ($base).custom"
  }

  print ""
  print $"=== Backup Complete: ($output_dir) ==="
  try {
    ls $output_dir | sort-by modified -r | first 10
  } catch { |err|
    error make {msg: $"Failed to list ($output_dir): ($err.msg)"}
  }
}

# Restore PostgreSQL from a backup — local file or bare PVC filename
export def "nur postgres restore" [
  backup_file: string       # Local path (.sql, .sql.gz, .custom) or PVC filename (no slash)
  --database: string = ""   # Target database; inferred from filename if omitted
  --recreate                # Drop and recreate the target database before restore
]: nothing -> nothing {
  # Bare filename with no slash and file absent locally → restore from PVC via Job
  if (not ($backup_file | path exists)) and (not ($backup_file | str contains "/")) and (
    ($backup_file | str ends-with ".sql.gz") or ($backup_file | str ends-with ".sql")
  ) {
    let job_name = $"postgresql-restore-(date now | format date '%s')"
    let restore_cmd = $"set -e
echo 'Restoring from /backups/($backup_file)'
gunzip -c /backups/($backup_file) | PGPASSWORD=\"$PGPASSWORD\" psql -h postgresql.($PG_NS) -U ($PG_USER) -d postgres
echo 'Restore completed successfully.'"
    let job_spec = {
      apiVersion: "batch/v1"
      kind: "Job"
      metadata: {name: $job_name, namespace: $PG_NS}
      spec: {
        ttlSecondsAfterFinished: 300
        template: {
          spec: {
            restartPolicy: "Never"
            containers: [{
              name: "restore"
              image: "pgvector/pgvector:pg17"
              command: ["/bin/bash", "-c", $restore_cmd]
              env: [{
                name: "PGPASSWORD"
                valueFrom: {secretKeyRef: {name: $PG_SECRET, key: "adminPassword"}}
              }]
              volumeMounts: [{name: "backups", mountPath: "/backups"}]
            }]
            volumes: [{name: "backups", persistentVolumeClaim: {claimName: "postgresql-backups"}}]
          }
        }
      }
    }
    print $"=== PostgreSQL Restore from PVC: ($backup_file) ==="
    try {
      $job_spec | to yaml | ^kubectl apply -f -
      print $"Job: ($job_name)"
      print $"Monitor: kubectl logs -n ($PG_NS) -f job/($job_name)"
      ^kubectl wait --for=condition=complete $"job/($job_name)" -n $PG_NS --timeout=600s
    } catch { |err|
      error make {msg: $"PVC restore job failed: ($err.msg)"}
    }
    print "=== Restore Complete ==="
    return
  }

  if not ($backup_file | path exists) {
    error make {msg: $"Backup file not found: ($backup_file)"}
  }

  let pod = (pg-pod)
  let password = (pg-password)
  let basename = ($backup_file | path basename)

  let db_name = if ($database | is-not-empty) {
    $database
  } else {
    let m = ($basename | parse --regex '^(?P<name>[^_]+)_\d{8}_\d{6}')
    if ($m | is-empty) {
      error make {msg: $"Cannot infer database name from '($basename)' — pass --database"}
    }
    $m | first | get name
  }

  print $"=== PostgreSQL Restore ==="
  print $"Namespace: ($PG_NS) | Pod: ($pod)"
  print $"File: ($backup_file) | Target DB: ($db_name)"
  print ""

  if $recreate {
    print "Dropping existing database..."
    try {
      ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" psql -h localhost -U $PG_USER -p $PG_PORT -c $"DROP DATABASE IF EXISTS \"($db_name)\";" postgres
    } catch { |err|
      error make {msg: $"Failed to drop ($db_name): ($err.msg)"}
    }
    print ""
  }

  if ($backup_file | str ends-with ".custom") {
    print "Restoring from custom format..."
    try {
      ^kubectl cp $backup_file $"($PG_NS)/($pod):/tmp/restore.custom"
      ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" pg_restore -h localhost -U $PG_USER -p $PG_PORT --clean --if-exists --create --no-owner --no-privileges -d postgres /tmp/restore.custom
      ^kubectl exec -n $PG_NS $pod -- rm -f /tmp/restore.custom
    } catch { |err|
      error make {msg: $"Restore from ($backup_file) failed: ($err.msg)"}
    }
  } else if ($backup_file | str ends-with ".sql.gz") {
    print "Restoring from gzipped SQL dump..."
    let tmp_sql = (^mktemp --suffix=.sql | str trim)
    try {
      ^gzip -dc $backup_file | save --force $tmp_sql
      ^kubectl cp $tmp_sql $"($PG_NS)/($pod):/tmp/restore.sql"
    } catch { |err|
      error make {msg: $"Failed to stage ($backup_file) for restore: ($err.msg)"}
    }
    try {
      rm $tmp_sql
    } catch { |err|
      error make {msg: $"Failed to clean up ($tmp_sql): ($err.msg)"}
    }
    try {
      ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" psql -h localhost -U $PG_USER -p $PG_PORT -f /tmp/restore.sql postgres
      ^kubectl exec -n $PG_NS $pod -- rm -f /tmp/restore.sql
    } catch { |err|
      error make {msg: $"Restore from ($backup_file) failed: ($err.msg)"}
    }
  } else if ($backup_file | str ends-with ".sql") {
    print "Restoring from SQL dump..."
    try {
      ^kubectl cp $backup_file $"($PG_NS)/($pod):/tmp/restore.sql"
      ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" psql -h localhost -U $PG_USER -p $PG_PORT -f /tmp/restore.sql postgres
      ^kubectl exec -n $PG_NS $pod -- rm -f /tmp/restore.sql
    } catch { |err|
      error make {msg: $"Restore from ($backup_file) failed: ($err.msg)"}
    }
  } else {
    error make {msg: $"Unsupported format: ($backup_file) — expected .sql, .sql.gz, or .custom"}
  }

  print ""
  print $"=== Restore Complete: ($db_name) ==="
  try {
    ^kubectl exec -n $PG_NS $pod -- env $"PGPASSWORD=($password)" psql -h localhost -U $PG_USER -p $PG_PORT -c '\l' postgres
  } catch { |err|
    error make {msg: $"Failed to list databases after restore: ($err.msg)"}
  }
}

# ─── MariaDB (ported from k3s-fleetops) ─────────────────────────────────────

# List available MariaDB backups on the mariadb-backups PVC
export def "nur mariadb list-backups" []: nothing -> nothing {
  let namespace = "mariadb"
  let pod_name = "mariadb-backup-lister"
  let pod_spec = {
    apiVersion: "v1"
    kind: "Pod"
    metadata: {name: $pod_name, namespace: $namespace}
    spec: {
      restartPolicy: "Never"
      containers: [{
        name: "lister"
        image: "bitnami/mariadb:latest"
        command: ["/bin/bash", "-c", "ls -lh /backups/*.sql.gz 2>/dev/null || echo 'No backups found'"]
        volumeMounts: [{name: "backups", mountPath: "/backups"}]
      }]
      volumes: [{name: "backups", persistentVolumeClaim: {claimName: "mariadb-backups"}}]
    }
  }
  try {
    $pod_spec | to yaml | ^kubectl apply -f -
    ^kubectl -n $namespace wait --for=condition=Ready pods $pod_name --timeout=60s
    ^kubectl -n $namespace logs $pod_name
    ^kubectl -n $namespace delete pods $pod_name --ignore-not-found=true
  } catch { |err|
    error make {msg: $"Failed to list MariaDB backups: ($err.msg)"}
  } | ignore
}

# Restore MariaDB from a backup file on the PVC (omit --backup-file to be prompted)
export def "nur mariadb restore" [--backup-file: string = ""] {
  let namespace = "mariadb"
  let backup_filename = if ($backup_file | is-empty) {
    nur mariadb list-backups
    input "Enter backup filename (e.g., mariadb-backup-20250101_020000.sql.gz): "
  } else {
    $backup_file
  }

  let job_name = $"mariadb-restore-(date now | format date '%s')"
  let restore_cmd = $"set -e
echo 'Starting restore from: ($backup_filename)'
echo 'WARNING: This will replace all existing databases!'
gunzip -c /backups/($backup_filename) | mysql -h mariadb.mariadb -u root -p\"$MARIADB_ROOT_PASSWORD\"
echo 'Restore completed successfully!'"

  let job_spec = {
    apiVersion: "batch/v1"
    kind: "Job"
    metadata: {name: $job_name, namespace: $namespace}
    spec: {
      ttlSecondsAfterFinished: 300
      template: {
        spec: {
          restartPolicy: "Never"
          containers: [{
            name: "restore"
            image: "bitnami/mariadb:latest"
            command: ["/bin/bash", "-c", $restore_cmd]
            env: [{
              name: "MARIADB_ROOT_PASSWORD"
              valueFrom: {secretKeyRef: {name: "mariadb-password", key: "mariadb-root-password"}}
            }]
            volumeMounts: [{name: "backups", mountPath: "/backups"}]
          }]
          volumes: [{name: "backups", persistentVolumeClaim: {claimName: "mariadb-backups"}}]
        }
      }
    }
  }

  try {
    $job_spec | to yaml | ^kubectl apply -f -
  } catch { |err|
    error make {msg: $"Failed to start MariaDB restore job: ($err.msg)"}
  }
  print $"Restore job: ($job_name)"
  print $"Monitor: kubectl logs -n ($namespace) -f job/($job_name)"
}

# Register git hooks for this repo (no .githooks/ committed yet -- fleetops'
# own pre-commit hook auto-ran a full k8s switch+push on every commit, which
# doesn't fit dotfiles' separate manifests-repo workflow; add hooks here
# deliberately if/when wanted rather than carrying that behavior over)
export def "nur apply-git-hooks" []: nothing -> nothing {
  ^git config core.hooksPath .githooks
}

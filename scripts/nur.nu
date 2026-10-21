# Every host's info from the flake (`info` in modules/flake/hosts.nix), from the
# working tree. Each record has sshHost (the name used for --host and ssh),
# hostname, homeConfiguration and nixosConfiguration (flake attrs, or null).
def host-infos []: nothing -> table {
  ^nix eval --json .#hostInfo | from json | values
}

# Look up a host by its sshHost or hostname
def host-info [host: string]: nothing -> record {
  let matches = (host-infos | where {|h| $h.sshHost == $host or $h.hostname == $host })

  if ($matches | is-empty) {
    error make {msg: $"unknown host: ($host) \(expected one of: ((host-infos | get sshHost | str join ', '))\)"}
  }

  $matches | first
}

# Name to ssh to for a host
def ssh-target [host: string]: nothing -> string {
  (host-info $host).sshHost
}

# homeConfigurations attribute for a host
def home-attr [host: string]: nothing -> string {
  let attr = (host-info $host).homeConfiguration
  if $attr == null { error make {msg: $"($host) has no home-manager configuration"} }
  $attr
}

# nixosConfigurations attribute for a host
def os-attr [host: string]: nothing -> string {
  let attr = (host-info $host).nixosConfiguration
  if $attr == null { error make {msg: $"($host) has no NixOS configuration"} }
  $attr
}

# NixOS hosts (tab completion)
def nixos-hosts []: nothing -> list<string> {
  host-infos | where nixosConfiguration != null | get sshHost
}

# Home-manager hosts (tab completion)
def home-hosts []: nothing -> list<string> {
  host-infos | where homeConfiguration != null | get sshHost
}

# linkFarm entry ({name, expr}) for a host's home-manager activation package.
# `expr` is a Nix expression relative to the flake bound as `f` (see build-combined).
def home-part [host: string]: nothing -> record<name: string, expr: string> {
  {name: $"(host-info $host | get hostname)-home", expr: $"f.homeConfigurations.(home-attr $host | to json).activationPackage"}
}

# linkFarm entry ({name, expr}) for a host's NixOS toplevel.
def os-part [host: string]: nothing -> record<name: string, expr: string> {
  {name: $"(host-info $host | get hostname)-os", expr: $"f.nixosConfigurations.(os-attr $host | to json).config.system.build.toplevel"}
}

# linkFarm entries for every host's home-manager activation package.
def all-home-parts []: nothing -> list<record<name: string, expr: string>> {
  host-infos | where homeConfiguration != null | each {|h|
    {name: $"($h.hostname)-home", expr: $"f.homeConfigurations.($h.homeConfiguration | to json).activationPackage"}
  }
}

# linkFarm entries for every host's NixOS toplevel.
def all-os-parts []: nothing -> list<record<name: string, expr: string>> {
  host-infos | where nixosConfiguration != null | each {|h|
    {name: $"($h.hostname)-os", expr: $"f.nixosConfigurations.($h.nixosConfiguration | to json).config.system.build.toplevel"}
  }
}

# Build several flake outputs as one derivation: a linkFarm with a symlink per part,
# so nom tracks a single build graph instead of one installable at a time.
def build-combined [
  ...parts: record<name: string, expr: string>
  --fallback
  --no-link
]: nothing -> nothing {
  let args = [
    ...(if $fallback { ["--fallback"] } else { [] })
    ...(if $no_link { ["--no-link"] } else { [] })
  ]
  let entries = ($parts | each {|p| $"{ name = ($p.name | to json); path = ($p.expr); }" } | str join " ")
  let expr = ([
    $"let f = builtins.getFlake ($env.PWD | to json);"
    "pkgs = f.inputs.nixpkgs.legacyPackages.${builtins.currentSystem};"
    $"in pkgs.linkFarm \"nur-build\" [ ($entries) ]"
  ] | str join " ")

  ^nom build --impure ...$args --expr $expr
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
      os-attr $host
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
    let parts = [
      ...(if $targets.os { all-os-parts } else { [] })
      ...(if $targets.home { all-home-parts } else { [] })
    ]

    if ($parts | is-not-empty) {
      build-combined ...$parts --fallback=$fallback --no-link
    }
  } else if ($host | is-empty) {
    # Build everything in one nom graph first, so the nh builds below find it all in the
    # store and print their package diffs back to back.
    let current = (sys host | get hostname | str lowercase)
    let parts = [
      ...(if $targets.os { [(os-part $current)] } else { [] })
      ...(if $targets.home { [(home-part $current)] } else { [] })
    ]

    if ($parts | is-not-empty) {
      build-combined ...$parts --fallback=$fallback --no-link
    }

    if $targets.home { ^nh home build ...$args . }
    if $targets.os { ^nh os build ...$args . }
  } else {
    let parts = [
      ...(if $targets.os { [(os-part $host)] } else { [] })
      ...(if $targets.home { [(home-part $host)] } else { [] })
    ]

    if ($parts | is-not-empty) {
      build-combined ...$parts --fallback=$fallback
    }
  }

  if $targets.k8s {
    if $fallback { nur k8s switch-charts --fallback } else { nur k8s switch-charts }
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

# Read windows key from firmware
export def "nur secrets windows-key" []: nothing -> string {
  sudo grep -Eao '(-?[A-Z0-9]{5}){5}' /sys/firmware/acpi/tables/MSDM
}

const SECRETS_DEFAULT_FILE = "secrets/k8s.enc.yaml"

def "nu-complete secret-files" []: nothing -> list<string> {
  ls secrets | where name =~ '\.(yaml|json)$' | get name
}

# Plaintext path for an `*.enc.*` file (secrets/k8s.enc.yaml -> secrets/k8s.yaml),
# refusing anything that isn't git-ignored so a decrypt can never be committed.
def secret-plaintext-path [file: string]: nothing -> string {
  if not ($file | str contains ".enc.") {
    error make {msg: $"($file) is encrypted in place (no .enc. in its name); use `nur secrets edit ($file)` instead"}
  }
  let plain = ($file | str replace ".enc." ".")
  if (^git check-ignore -q $plain | complete).exit_code != 0 {
    error make {msg: $"($plain) is not git-ignored; add it to .gitignore before decrypting to it"}
  }
  $plain
}

def secret-load [file: string]: nothing -> record {
  ^sops --decrypt --output-type json $file | from json
}

# Dotted paths of every leaf value, e.g. postgresql.password
def secret-leaf-paths [data: any, prefix: string = ""]: nothing -> list<string> {
  match ($data | describe | str replace --regex '<.*' '') {
    "record" => {
      $data | transpose key value | each { |kv|
        secret-leaf-paths $kv.value ([$prefix $kv.key] | where $it != "" | str join ".")
      } | flatten
    }
    "list" | "table" => {
      $data | enumerate | each { |it|
        secret-leaf-paths $it.item ([$prefix ($it.index | into string)] | where $it != "" | str join ".")
      } | flatten
    }
    _ => [$prefix]
  }
}

def secret-lookup [data: record, path: string, file: string]: nothing -> string {
  let value = ($path | split row "." | reduce --fold $data { |part, acc|
    let key = if ($acc | describe | str starts-with "list") or ($acc | describe | str starts-with "table") {
      $part | into int
    } else { $part }
    let next = ($acc | get --optional $key)
    if $next == null {
      error make {msg: $"No secret at ($path) in ($file); see `nur secrets keys --file ($file)`"}
    }
    $next
  })
  if ($value | describe) =~ '^(record|list|table)' {
    error make {msg: $"($path) in ($file) is not a leaf value; see `nur secrets keys --file ($file)`"}
  }
  $value | into string
}

# Edit an encrypted secrets file in-place (no plaintext file written)
export def "nur secrets edit" [
  file: string@"nu-complete secret-files" = $SECRETS_DEFAULT_FILE
]: nothing -> nothing {
  ^sops $file
}

# Decrypt an `*.enc.*` secrets file next to itself (secrets/k8s.enc.yaml -> secrets/k8s.yaml).
# The plaintext must be git-ignored; never commit it.
export def "nur secrets decrypt" [
  file: string@"nu-complete secret-files" = $SECRETS_DEFAULT_FILE
]: nothing -> nothing {
  let plain = (secret-plaintext-path $file)
  ^sops --decrypt $file | save --force $plain
  print $"Decrypted to ($plain) — do not commit this file"
}

# Encrypt the plaintext produced by `nur secrets decrypt` back into the `*.enc.*` file
export def "nur secrets encrypt" [
  file: string@"nu-complete secret-files" = $SECRETS_DEFAULT_FILE
]: nothing -> nothing {
  let plain = (secret-plaintext-path $file)
  # --filename-override matches creation rules against the real target path --
  # without it sops matches against secrets/k8s.yaml, which falls through to
  # the generic secrets/*.yaml rule instead of the dedicated k8s.enc.yaml one
  # (wrong recipient set: missing the k8s primary key, adding nixmini).
  ^sops --encrypt --filename-override $file $plain | save --force $file
  print $"Encrypted ($plain) to ($file)"
}

# List the key paths in a secrets file, without decrypting anything (sops keeps keys in plaintext)
export def "nur secrets keys" [
  --file: string@"nu-complete secret-files" = $SECRETS_DEFAULT_FILE
  prefix: string = ""   # only show paths starting with this, e.g. "postgresql"
]: nothing -> list<string> {
  let data = (open --raw $file | from yaml | reject --optional sops)
  secret-leaf-paths $data | where { str starts-with $prefix }
}

# Run a command with secrets injected as environment variables, without printing them.
# Each leading VAR=path argument maps an env var to a dotted path in the secrets file;
# everything after is the command. Any secret value in the command's output is replaced
# with [REDACTED:VAR] unless --no-redact is given (which also streams output/stdin).
#
#   nur secrets exec PGPASSWORD=postgresql.password psql -h db -U postgres -c '\l'
#   nur secrets exec --file secrets/mcp.yaml TOKEN=trilium_token sh -c 'curl -H "Authorization: $TOKEN" ...'
export def --wrapped "nur secrets exec" [
  --file: string@"nu-complete secret-files" = $SECRETS_DEFAULT_FILE
  --no-redact
  ...rest   # untyped: nur re-parses the CLI as Nushell, so `true`/`false`/`null` arrive as literals
]: nothing -> nothing {
  let args = ($rest | each { |a| if $a == null { "null" } else { $a | into string } })
  let mappings = ($args | take while { $in =~ '^[A-Za-z_][A-Za-z0-9_]*=' })
  let command = ($args | skip ($mappings | length))
  if ($mappings | is-empty) or ($command | is-empty) {
    error make {msg: "usage: nur secrets exec [--file F] VAR=path [VAR=path ...] command [args ...]"}
  }

  let data = (secret-load $file)
  let secrets = ($mappings | each { |m|
    let parts = ($m | split row --number 2 "=")
    {var: $parts.0, value: (secret-lookup $data $parts.1 $file)}
  })
  let env_record = ($secrets | reduce --fold {} { |s, acc| $acc | insert $s.var $s.value })

  if $no_redact {
    with-env $env_record { run-external ...$command }
    return
  }

  let result = (with-env $env_record { run-external ...$command | complete })
  # Longest values first so a secret containing another is redacted whole
  let redactions = ($secrets | where { ($in.value | str length) > 0 } | sort-by --reverse { $in.value | str length })
  let redact = { |text: string|
    $redactions | reduce --fold $text { |s, acc| $acc | str replace --all $s.value $"[REDACTED:($s.var)]" }
  }
  print --no-newline (do $redact $result.stdout)
  print --no-newline --stderr (do $redact $result.stderr)
  if $result.exit_code != 0 { exit $result.exit_code }
}

# Show whether a secret is set, plus its length and a short fingerprint, without printing it
export def "nur secrets check" [
  --file: string@"nu-complete secret-files" = $SECRETS_DEFAULT_FILE
  ...paths: string
]: nothing -> table {
  let data = (secret-load $file)
  $paths | each { |path|
    let value = (try { secret-lookup $data $path $file } catch { null })
    {
      path: $path
      set: ($value != null and $value != "")
      length: ($value | default "" | str length)
      sha256: (if $value == null { null } else { $value | hash sha256 | str substring 0..7 })
    }
  }
}

# Switch home-manager, NixOS, and/or k8s manifests (local if no --host, otherwise remote).
# Pass os/home/k8s to restrict which targets are switched, e.g. `nur switch home k8s`.
# Use --boot to set the os target as boot default instead of activating immediately (safe for slow activations).
export def "nur switch" [
  --boot
  --no-review  # k8s: push manifests without reviewing them in magit first
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
      # Build with nom first so progress is visible; home-manager then finds it all in the store.
      ^nom build $".#homeConfigurations.($env.USER)@((sys host).hostname).activationPackage" --no-link --show-trace
      let ts = (date now | format date '%s')
      ^home-manager switch --flake . -b $"backup.($ts)" --show-trace
    } else {
      do-switch-remote-home $host
    }
  }

  if $targets.os {
    if ($host | is-empty) {
      # Build with nom first so progress is visible; nixos-rebuild then finds it all in the store.
      ^nom build $".#nixosConfigurations.((sys host).hostname).config.system.build.toplevel" --no-link --show-trace
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
    if $no_review { nur k8s deploy --no-review } else { nur k8s deploy }
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

# VS Code settings that don't match what Nix manages (features/vscode.nix plus the
# host's features.vscode.settings), local or on --host. `unmanaged` keys exist only
# in settings.json; `changed` keys were edited in the UI and reset on the next switch.
export def "nur vscode drift" [--host: string@home-hosts = ""]: nothing -> table {
  let read = {|rel|
    if ($host | is-empty) {
      open --raw ($env.HOME | path join $rel)
    } else {
      ^ssh (ssh-target $host) cat $rel
    } | from json
  }

  # One row per setting; nested blocks like "[nix]" are split into their keys
  # (with the block as `section`), matching how activation deep-merges them.
  let flatten = {|settings|
    $settings | transpose key value | each {|row|
      if ($row.value | describe | str starts-with record) {
        $row.value | transpose key value | insert section $row.key
      } else {
        [($row | insert section null)]
      }
    } | flatten
  }

  let current = (do $flatten (do $read ".config/Code/User/settings.json"))
  let managed = (do $flatten (do $read ".config/dotfiles/vscode-settings.json"))

  $current | each {|row|
    let match = ($managed | where section == $row.section and key == $row.key)
    if ($match | is-empty) {
      {section: $row.section, key: $row.key, status: unmanaged, value: $row.value, nix: null}
    } else if ($match.0.value != $row.value) {
      {section: $row.section, key: $row.key, status: changed, value: $row.value, nix: $match.0.value}
    }
  } | compact
}

# Claude Code permission rules in ~/.claude/settings.json that features/claude.nix
# doesn't manage, local or on --host: candidates to promote to the shared list (or to
# the host's features.claude.allow), or one-off approvals to delete. `pattern` rules
# end in a wildcard; the rest approve one exact command. Values of *PASSWORD=, *TOKEN=,
# *SECRET= and *KEY= assignments are masked, since approved commands can contain them.
export def "nur claude drift" [--host: string@home-hosts = ""]: nothing -> table {
  let read = {|rel|
    if ($host | is-empty) {
      open --raw ($env.HOME | path join $rel)
    } else {
      ^ssh (ssh-target $host) cat $rel
    } | from json
  }

  let current = (do $read ".claude/settings.json" | get -o permissions.allow | default [])
  let managed = (do $read ".config/dotfiles/claude-settings.json" | get permissions.allow)

  $current
  | where $it not-in $managed
  | each {|rule|
    {
      rule: ($rule | str replace -ar '(?i)(\w*(password|token|secret|key)\w*=)(\S+)' '${1}[REDACTED]')
      pattern: ($rule =~ '\*\)$')
    }
  }
}

# Remove throwaway Claude Code permission rules from ~/.claude/settings.json, local or
# on --host: Bash rules without a wildcard (they approve one exact command line), and
# rules naming a /nix/store hash, /tmp or a scratchpad path. Lists them unless --apply;
# --apply first copies the file to settings.json.bak-<timestamp>. Shared rules from
# features/claude.nix are never removed (activation would add them back anyway).
export def "nur claude prune" [--host: string@home-hosts = "", --apply]: nothing -> any {
  let rel = ".claude/settings.json"
  let read = {|path|
    if ($host | is-empty) {
      open --raw ($env.HOME | path join $path)
    } else {
      ^ssh (ssh-target $host) cat $path
    } | from json
  }

  let settings = (do $read $rel)
  let managed = (do $read ".config/dotfiles/claude-settings.json" | get permissions.allow)
  let junk = {|rule|
    ($rule not-in $managed) and (
      ($rule starts-with "Bash(" and not ($rule | str contains "*"))
      or ($rule =~ '/nix/store/[0-9a-z]{32}-' or $rule =~ '//?tmp/[^*]' or ($rule | str contains "scratchpad"))
    )
  }
  let allow = ($settings | get -o permissions.allow | default [])
  let removed = ($allow | where {|r| do $junk $r })

  if not $apply {
    return ($removed | each {|rule|
      {rule: ($rule | str replace -ar '(?i)(\w*(password|token|secret|key)\w*=)(\S+)' '${1}[REDACTED]')}
    })
  }

  let kept = ($allow | where {|r| not (do $junk $r) })
  let json = ($settings | upsert permissions.allow $kept | to json --indent 2)
  let backup = $"($rel).bak-(date now | format date '%Y%m%d-%H%M%S')"
  if ($host | is-empty) {
    cd $env.HOME
    cp $rel $backup
    $json | save --force $rel
  } else {
    $json | ^ssh (ssh-target $host) $"cp ($rel) ($backup) && cat > ($rel)"
  }
  print $"Removed ($removed | length) of ($allow | length) rules; backup at ~/($backup)"
}

# Show what would change without applying (local if no --host, otherwise remote dry-run)
export def "nur dry-run-os" [--host: string@nixos-hosts = ""] {
  if ($host | is-empty) {
    ^sudo nixos-rebuild dry-activate --flake . --show-trace
  } else {
    with-env { NIX_SSHOPTS: "-t" } {
      ^nixos-rebuild dry-activate --flake $".#(os-attr $host)" --target-host (ssh-target $host) --build-host localhost --sudo --ask-sudo-password
    }
  }
}

# Helper: diff OS closures between local build and remote host
def do-diff-remote-os [host: string] {
  let attr = (os-attr $host)
  let target = (ssh-target $host)
  print $"Building new NixOS configuration for ($host)..."

  let new_system = (
    ^nom build $".#nixosConfigurations.($attr).config.system.build.toplevel" --no-link --print-out-paths
      o+e>| lines
      | where { |l| $l | str starts-with "/nix/store" }
      | last
  )

  let current_system = (^ssh $target "readlink -f /nix/var/nix/profiles/system" | str trim)

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
  let attr = (os-attr $host)
  let target = (ssh-target $host)
  print --stderr $"Building NixOS configuration for ($host) with nom..."
  let out_link = $"/tmp/($host)-system-result"
  ^nom build $".#nixosConfigurations.($attr).config.system.build.toplevel" --out-link $out_link
  let system_path = (^realpath $out_link | str trim)

  if ($system_path | str starts-with "/nix/store") {
    print --stderr $"Copying system to ($host): ($system_path)"
    ^nix copy --to $"ssh://($target)" $system_path
    print --stderr $"Activating on ($host) \(will prompt for sudo password\)..."
    ^ssh -t $target $"sudo nix-env -p /nix/var/nix/profiles/system --set ($system_path) && sudo ($system_path)/bin/switch-to-configuration switch"
  } else {
    print --stderr "ERROR: Failed to build or get system path"
  }
}

# Helper: build NixOS config locally with nom, copy and set as boot default on remote host
def do-boot-remote-os [host: string] {
  let attr = (os-attr $host)
  let target = (ssh-target $host)
  print --stderr $"Building NixOS configuration for ($host) with nom..."
  let out_link = $"/tmp/($host)-system-result"
  ^nom build $".#nixosConfigurations.($attr).config.system.build.toplevel" --out-link $out_link
  let system_path = (^realpath $out_link | str trim)

  if ($system_path | str starts-with "/nix/store") {
    print --stderr $"Copying system to ($host): ($system_path)"
    ^nix copy --to $"ssh://($target)" $system_path
    print --stderr $"Setting boot default on ($host) \(will prompt for sudo password\)..."
    ^ssh -t $target $"sudo nix-env -p /nix/var/nix/profiles/system --set ($system_path) && sudo ($system_path)/bin/switch-to-configuration boot"
  } else {
    print --stderr "ERROR: Failed to build or get system path"
  }
}

# Helper: build home-manager package locally, copy and activate on remote host
def do-switch-remote-home [host: string] {
  print --stderr $"Building home-manager activation package for ($host)..."
  let out_link = $"/tmp/($host)-home-result"
  let target = (ssh-target $host)
  ^nom build $".#homeConfigurations.(home-attr $host).activationPackage" --out-link $out_link
  let activation_path = (^realpath $out_link | str trim)

  if ($activation_path | str starts-with "/nix/store") {
    print --stderr $"Copying to ($host): ($activation_path)"
    ^nix copy --to $"ssh://($target)" $activation_path
    print --stderr $"Activating on ($host)..."
    let ts = (date now | format date '%s')
    ^ssh $target $"HOME_MANAGER_BACKUP_EXT=backup.($ts) ($activation_path)/activate"
  } else {
    print --stderr "ERROR: Failed to build activation package or invalid path"
  }
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
export def "nur k8s switch-charts" [
  --fallback  # Pass --fallback to the underlying nix build (rebuild locally if a substituter is unavailable)
] {
  with-env {FALLBACK: ($fallback | into string)} {
    ^sh scripts/k8s-switch-charts.sh
  }
}

# Commit and push generated manifests to the private manifests repo
export def "nur k8s push" [] {
  ^sh scripts/k8s-push-manifests.sh
}

# Whether kubernetes/manifests has anything to push: uncommitted changes, or
# commits not yet on its upstream (e.g. committed by hand during review).
def k8s-manifests-pending []: nothing -> bool {
  let dir = "kubernetes/manifests"
  let dirty = (^git -C $dir status --porcelain | str trim | is-not-empty)
  let ahead = (^git -C $dir rev-list --count "@{u}..HEAD" | str trim | into int) > 0
  $dirty or $ahead
}

# Open magit on kubernetes/manifests, like the Mod+E binding, and block until
# the frame is closed (a graphical frame, or a tty frame without a display).
def k8s-review-manifests []: nothing -> nothing {
  let dir = ("kubernetes/manifests" | path expand)
  let expr = $"\(magit-status \"($dir)\"\)"
  let frame = if ($env.WAYLAND_DISPLAY? | is-not-empty) or ($env.DISPLAY? | is-not-empty) { "-c" } else { "-t" }
  print "Review the manifest changes in magit; close the frame (C-x 5 0) when done."
  ^emacsclient $frame -a "" --eval $expr
}

# Build manifests and push to private repo (sync + switch-charts + review + push).
# Syncing first keeps the local checkout from being stale relative to origin,
# so `git push` in `nur k8s push` doesn't get rejected as non-fast-forward.
# Unless --no-review, the changes are opened in magit first and pushing needs
# confirmation; aborting leaves them uncommitted in kubernetes/manifests
# (push them later with `nur k8s push`).
export def "nur k8s deploy" [
  --no-review  # Commit and push without reviewing or confirming
] {
  nur k8s manifests sync
  nur k8s switch-charts

  if not $no_review {
    if not (k8s-manifests-pending) {
      print "No manifest changes to push."
      return
    }
    if not (is-terminal --stdin) {
      error make {msg: "reviewing manifests needs a terminal; pass --no-review to push without it"}
    }
    k8s-review-manifests
    let push = "Commit and push"
    # Abort first, so a bare Enter doesn't deploy
    let choice = (["Abort (keep changes in kubernetes/manifests)" $push] | input list "Push these manifest changes?")
    if $choice != $push {
      print "Not pushed. Run 'nur k8s push' to push later."
      return
    }
  }

  nur k8s push
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
# Apps are appTemplates/<name>.nix (mkArgoApp templates, see
# modules/kubernetes/applications.nix) plus modules/kubernetes/nixidy-apps/<name>.nix
# (plain nixidy modules that don't go through mkArgoApp).

# Every app name defined by a file in either directory
def "nu-complete apps" []: nothing -> list<string> {
  glob "{appTemplates,modules/kubernetes/nixidy-apps}/*.nix"
  | each { path parse | get stem }
  | where {|name| not ($name | str starts-with '_') }
  | uniq
  | sort
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
      + "(nur secrets edit), then nur switch, before running this."
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
# infra-manifests/). The live 00-master Application now points at
# argo-manifests (the GitOps cutover is done -- see the consolidation plan),
# so `nur argocd apply-master` re-applies kubernetes/infra-manifests/00-master.yaml
# for real.

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

# Run `argocd ... --core` against the argocd namespace. Core mode reads argocd-cm
# from the kubeconfig context's namespace (there's no flag for it; -N is only
# the app namespace), so run it with a temporary copy of the current context
# whose namespace is argocd, leaving the real kubeconfig alone.
def argocd-core [...args: string]: nothing -> nothing {
  let kubeconfig = (^mktemp -t argocd-kubeconfig.XXXXXX | str trim)
  let result = (try {
    ^kubectl config view --minify --flatten | save --force $kubeconfig
    with-env {KUBECONFIG: $kubeconfig} {
      ^kubectl config set-context --current --namespace=argocd | ignore
      ^argocd ...$args --core
    }
    null
  } catch { |err| $err.msg })
  rm --force $kubeconfig
  if $result != null {
    error make {msg: $result}
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
    argocd-core app sync ...$apps
  } else {
    argocd-core app sync $name
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
        command: ["/bin/bash", "-c", "ls -lht /backups/*.sql.gz 2>/dev/null || echo 'No backups found'; df -h /backups"]
        volumeMounts: [{name: "backups", mountPath: "/backups", subPath: "postgresql"}]
      }]
      volumes: [{name: "backups", persistentVolumeClaim: {claimName: "postgresql-backups-nas"}}]
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
              volumeMounts: [{name: "backups", mountPath: "/backups", subPath: "postgresql"}]
            }]
            volumes: [{name: "backups", persistentVolumeClaim: {claimName: "postgresql-backups-nas"}}]
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

# List available MariaDB backups on the NAS (DatabaseBackups/mariadb)
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
        image: "docker.io/mariadb:11.8.9"
        command: ["/bin/bash", "-c", "ls -lh /backups/*.sql.gz 2>/dev/null || echo 'No backups found'"]
        volumeMounts: [{name: "backups", mountPath: "/backups", subPath: "mariadb"}]
      }]
      volumes: [{name: "backups", persistentVolumeClaim: {claimName: "mariadb-backups-nas"}}]
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
gunzip -c /backups/($backup_filename) | mariadb -h mariadb.mariadb -u root -p\"$MARIADB_ROOT_PASSWORD\"
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
            image: "docker.io/mariadb:11.8.9"
            command: ["/bin/bash", "-c", $restore_cmd]
            env: [{
              name: "MARIADB_ROOT_PASSWORD"
              valueFrom: {secretKeyRef: {name: "mariadb-password", key: "root-password"}}
            }]
            volumeMounts: [{name: "backups", mountPath: "/backups", subPath: "mariadb"}]
          }]
          volumes: [{name: "backups", persistentVolumeClaim: {claimName: "mariadb-backups-nas"}}]
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

# ─── Longhorn cold archive (detached volumes -> the NAS) ────────────────────
# Copies a detached Longhorn volume to the NAS's ClusterBackups share twice:
# a Longhorn block backup (backupstore/, the cluster's backup target) and a
# browsable file export (exports/<ns>/<pvc>/: data/, MANIFEST.sha256, INFO).
# Then it restores the block backup into a scratch volume and checks it
# against the manifest, and labels the Backup cold-verified=true. It never
# deletes the source volume. Scratch PVs/PVCs/pods live in COLD_NS.

const LH_NS = "longhorn-system"
const COLD_NS = "volume-export"
const COLD_IMAGE = "debian:bookworm-slim"
const CLUSTER_BACKUPS = {server: "192.168.0.124", path: "/volume1/ClusterBackups"}

# Copy /src to $OUT/data (via data.tmp, so a failed run never leaves a
# half-copy as data/), checksum the source, and check the copy against it.
# The NAS's shares apply their own ACLs (directories come out 0777), so the
# copy can't keep modes: METADATA records the source's paths, owners, modes
# and types instead, and the block backup keeps them exactly.
const COLD_EXPORT_SCRIPT = r#'set -euo pipefail
mkdir -p "$OUT"
rm -rf "$OUT/data.tmp"
mkdir "$OUT/data.tmp"
cp -a /src/. "$OUT/data.tmp/"
(cd /src && find . -type f -print0 | sort -z | xargs -0 -r sha256sum) > "$OUT/MANIFEST.sha256"
(cd /src && find . -printf '%p %U:%G %m %y\n' | sort) > "$OUT/METADATA"
if [ -s "$OUT/MANIFEST.sha256" ]; then
  (cd "$OUT/data.tmp" && sha256sum --check --quiet --strict ../MANIFEST.sha256)
fi
rm -rf "$OUT/data"
mv "$OUT/data.tmp" "$OUT/data"
printf 'volume=%s\npvc=%s\nexported=%s\nblock-backup=%s\n' "$VOL" "$PVC" "$(date -Is)" "$BACKUP" > "$OUT/INFO"
echo "exported $(wc -l < "$OUT/MANIFEST.sha256") files, $(du -sh "$OUT/data" | cut -f1)"'#

# Check the volume restored from the block backup against the export's
# manifest and file count, and against the source's owners and modes as
# recorded in METADATA.
const COLD_VERIFY_SCRIPT = r#'set -euo pipefail
cd /src
if [ -s "$OUT/MANIFEST.sha256" ]; then
  sha256sum --check --quiet --strict "$OUT/MANIFEST.sha256"
fi
n=$(find . -type f | wc -l)
m=$(wc -l < "$OUT/MANIFEST.sha256")
[ "$n" -eq "$m" ] || { echo "file count differs: restored $n, manifest $m"; exit 1; }
diff <(find . -printf '%p %U:%G %m %y\n' | sort) "$OUT/METADATA"
echo "verified-restore=$(date -Is)" >> "$OUT/INFO"
echo "restored $n files match the manifest, owners and modes"'#

# Longhorn's kubernetesStatus only remembers the last PVC that bound a
# volume, and cold-archive's scratch PVCs overwrite it. The cold-* Backup's
# `pvc` label keeps the original <ns>/<pvc>, so prefer that when present.
def lh-volumes []: nothing -> table {
  let pvc_of = (
    ^kubectl -n $LH_NS get backups.longhorn.io -o json | from json | get items
    | where { |b| ($b.metadata.name | str starts-with "cold-") and ($b.spec.labels? | default {} | get --optional pvc) != null }
    | each { |b| {volume: ($b.metadata.labels | get --optional backup-volume | default ""), pvc: $b.spec.labels.pvc} }
  )
  ^kubectl -n $LH_NS get volumes.longhorn.io -o json | from json | get items | each { |v|
    let k = ($v.status.kubernetesStatus? | default {})
    let labelled = ($pvc_of | where volume == $v.metadata.name | get --optional 0.pvc)
    let ns_pvc = if $labelled != null {
      $labelled | split row "/"
    } else if ($k.namespace? | default "" | is-empty) or ($k.namespace? == $COLD_NS) {
      ["" ""]
    } else {
      [$k.namespace $k.pvcName]
    }
    {
      name: $v.metadata.name
      namespace: ($ns_pvc | first)
      pvc: ($ns_pvc | last)
      pv: ($k.pvName? | default "")
      state: ($v.status.state? | default "")
      size: ($v.spec.size | into int)
      actual: ($v.status.actualSize? | default 0 | into int)
    }
  }
}

# A jsonpath field of a resource, or "" if it doesn't exist
def kget [kind: string, name: string, path: string, --namespace (-n): string = ""]: nothing -> string {
  let ns = if ($namespace | is-empty) { [] } else { ["-n" $namespace] }
  let r = (^kubectl ...$ns get $kind $name -o $"jsonpath=($path)" | complete)
  if $r.exit_code == 0 { $r.stdout | str trim } else { "" }
}

def kapply [docs: list<record>]: nothing -> nothing {
  $docs | each { to yaml } | str join "\n---\n" | ^kubectl apply -f - | ignore
}

def wait-for [what: string, timeout: duration, check: closure]: nothing -> nothing {
  let deadline = ((date now) + $timeout)
  loop {
    if (do $check) { return }
    if (date now) > $deadline {
      error make {msg: $"Timed out waiting for ($what)"}
    }
    sleep 5sec
  }
}

# A static PV + PVC in COLD_NS for an existing Longhorn volume (Retain, so
# deleting them never touches the volume)
def cold-claim [pv: string, claim: string, volume: string, size: int, fs: string]: nothing -> list<record> {
  let storage = ($size | into string)
  [
    {
      apiVersion: "v1"
      kind: "PersistentVolume"
      metadata: {name: $pv}
      spec: {
        capacity: {storage: $storage}
        accessModes: ["ReadWriteOnce"]
        persistentVolumeReclaimPolicy: "Retain"
        storageClassName: ""
        claimRef: {namespace: $COLD_NS, name: $claim}
        csi: {driver: "driver.longhorn.io", fsType: $fs, volumeHandle: $volume}
      }
    }
    {
      apiVersion: "v1"
      kind: "PersistentVolumeClaim"
      metadata: {name: $claim, namespace: $COLD_NS}
      spec: {
        accessModes: ["ReadWriteOnce"]
        storageClassName: ""
        volumeName: $pv
        resources: {requests: {storage: $storage}}
      }
    }
  ]
}

# Run a script in a pod with the claim read-only at /src and ClusterBackups
# at /nas; fail if it fails
def cold-run-pod [name: string, claim: string, script: string, pod_env: record]: nothing -> nothing {
  kapply [{
    apiVersion: "v1"
    kind: "Pod"
    metadata: {name: $name, namespace: $COLD_NS}
    spec: {
      restartPolicy: "Never"
      containers: [{
        name: "main"
        image: $COLD_IMAGE
        command: ["bash" "-c" $script]
        env: ($pod_env | transpose name value)
        volumeMounts: [
          {name: "src", mountPath: "/src", readOnly: true}
          {name: "nas", mountPath: "/nas"}
        ]
      }]
      volumes: [
        {name: "src", persistentVolumeClaim: {claimName: $claim, readOnly: true}}
        {name: "nas", persistentVolumeClaim: {claimName: "cluster-backups"}}
      ]
    }
  }]
  let started = (date now)
  wait-for $"pod ($name)" 2hr {
    let phase = (kget pod $name "{.status.phase}" -n $COLD_NS)
    # A pod that never starts (the volume won't attach or mount) shows up
    # here as Pending; don't sit out the whole timeout on it.
    if $phase == "Pending" and ((date now) - $started) > 10min {
      error make {msg: $"($name) still Pending after 10 minutes:\n(cold-pod-events $name)"}
    }
    if ($phase | is-empty) and ((date now) - $started) > 1min {
      error make {msg: $"($name) disappeared:\n(cold-pod-events $name)"}
    }
    $phase in ["Succeeded" "Failed"]
  }
  let logs = ((^kubectl -n $COLD_NS logs $name | complete) | $"($in.stdout)($in.stderr)" | str trim)
  $logs | lines | last 3 | each { |l| print $"  ($l)" } | ignore
  if (kget pod $name "{.status.phase}" -n $COLD_NS) != "Succeeded" {
    let term = (kget pod $name "{.status.containerStatuses[0].state.terminated.reason} exit={.status.containerStatuses[0].state.terminated.exitCode} {.status.containerStatuses[0].state.terminated.message}" -n $COLD_NS)
    error make {msg: $"($name) failed \(($term)\):\n($logs)\n(cold-pod-events $name)"}
  }
}

# ClusterBackups as a PV/PVC in COLD_NS rather than an inline `nfs:` pod
# volume, which can't take mount options: without nfsvers=3 some nodes'
# clients try NFSv4.2 first, the NAS's v4 pseudo-root fails ("Cannot export
# /"), and the mount ends in "No such file or directory" with no v3 retry.
const COLD_NAS_PV = "cold-archive-cluster-backups"

def cold-nas-claim []: nothing -> nothing {
  # A PV left Released by an earlier run keeps a stale claimRef; start over.
  ^kubectl delete pv $COLD_NAS_PV --ignore-not-found --wait=true | ignore
  kapply [
    {
      apiVersion: "v1"
      kind: "PersistentVolume"
      metadata: {name: $COLD_NAS_PV}
      spec: {
        capacity: {storage: "1Ti"}
        accessModes: ["ReadWriteMany"]
        persistentVolumeReclaimPolicy: "Retain"
        storageClassName: ""
        mountOptions: ["nfsvers=3" "nolock" "hard"]
        claimRef: {namespace: $COLD_NS, name: "cluster-backups"}
        nfs: $CLUSTER_BACKUPS
      }
    }
    {
      apiVersion: "v1"
      kind: "PersistentVolumeClaim"
      metadata: {name: "cluster-backups", namespace: $COLD_NS}
      spec: {
        accessModes: ["ReadWriteMany"]
        storageClassName: ""
        volumeName: $COLD_NAS_PV
        resources: {requests: {storage: "1Gi"}}
      }
    }
  ]
}

def cold-pod-events [name: string]: nothing -> string {
  (^kubectl -n $COLD_NS get events --field-selector $"involvedObject.name=($name)" -o custom-columns=REASON:.reason,MESSAGE:.message --no-headers | complete).stdout | lines | last 5 | str join "\n"
}

def cold-cleanup [short: string, volume: string]: nothing -> nothing {
  ^kubectl -n $COLD_NS delete pod $"export-($short)" $"verify-($short)" --ignore-not-found --wait=true | ignore
  ^kubectl -n $COLD_NS delete pvc $"src-($short)" $"restore-($short)" --ignore-not-found --wait=true | ignore
  ^kubectl delete pv $"export-($volume)" $"restore-($short)" --ignore-not-found --wait=true | ignore
  ^kubectl -n $LH_NS delete volumes.longhorn.io $"restore-($short)" --ignore-not-found | ignore
}

# A ready snapshot named cold-<short> (reused on reruns), or cold-<short>-r2,
# -r3 when that name is taken. A snapshot an interrupted run left half-made
# sits on the replica marked removed, and Longhorn only purges it while the
# volume is attached, so a new snapshot under its name is linked to the dead
# one and never becomes ready. Returns the name used.
def cold-snapshot [volume: string, short: string]: nothing -> any {
  for tag in [$"cold-($short)" $"cold-($short)-r2" $"cold-($short)-r3"] {
    if (kget snapshots.longhorn.io $tag "{.status.readyToUse}" -n $LH_NS) == "true" {
      return $tag
    }
    if (kget snapshots.longhorn.io $tag "{.status.markRemoved}" -n $LH_NS) == "true" {
      continue
    }
    if (kget snapshots.longhorn.io $tag "{.metadata.name}" -n $LH_NS | is-empty) {
      kapply [{
        apiVersion: "longhorn.io/v1beta2"
        kind: "Snapshot"
        metadata: {name: $tag, namespace: $LH_NS}
        spec: {volume: $volume, createSnapshot: true, labels: {purpose: "cold-archive"}}
      }]
    }
    let deadline = ((date now) + 30min)
    mut state = ""
    while $state == "" {
      if (kget snapshots.longhorn.io $tag "{.status.readyToUse}" -n $LH_NS) == "true" {
        $state = "ready"
      } else if (kget snapshots.longhorn.io $tag "{.status.markRemoved}" -n $LH_NS) == "true" {
        $state = "taken"
      } else if (date now) > $deadline {
        error make {msg: $"Timed out waiting for snapshot ($tag)"}
      } else {
        sleep 5sec
      }
    }
    if $state == "ready" {
      return $tag
    }
    print $"  ($tag) is taken by a removed snapshot; trying the next name"
  }
  error make {msg: $"No usable snapshot name for ($volume) \(cold-($short) through -r3 are all taken\)"}
}

def cold-archive-one [v: record, out_rel: string]: nothing -> nothing {
  if $v.state != "detached" {
    error make {msg: $"($v.name) is ($v.state); only detached volumes are archived"}
  }
  if ($v.namespace | is-empty) {
    error make {msg: $"($v.name) has no known namespace/PVC to name its export after"}
  }
  let short = ($v.name | str substring 4..11)
  let out = $"/nas/($out_rel)"
  let fs = if ($v.pv | is-empty) { "" } else { kget pv $v.pv "{.spec.csi.fsType}" }
  let fs = if ($fs | is-empty) { "ext4" } else { $fs }
  print $"=== ($v.namespace)/($v.pvc) \(($v.name)\) -> ClusterBackups/($out_rel)"

  print "  block backup"
  let tag = (cold-snapshot $v.name $short)
  # An Error backup never retries; start it over.
  if (kget backups.longhorn.io $tag "{.status.state}" -n $LH_NS) == "Error" {
    ^kubectl -n $LH_NS delete backups.longhorn.io $tag --wait=true | ignore
  }
  if (kget backups.longhorn.io $tag "{.metadata.name}" -n $LH_NS | is-empty) {
    kapply [{
      apiVersion: "longhorn.io/v1beta2"
      kind: "Backup"
      metadata: {name: $tag, namespace: $LH_NS, labels: {backup-volume: $v.name}}
      spec: {snapshotName: $tag, labels: {purpose: "cold-archive", pvc: $"($v.namespace)/($v.pvc)"}}
    }]
  }
  wait-for $"backup ($tag)" 2hr {
    let state = (kget backups.longhorn.io $tag "{.status.state}" -n $LH_NS)
    if $state == "Error" {
      error make {msg: $"Backup ($tag) failed: (kget backups.longhorn.io $tag '{.status.error}' -n $LH_NS)"}
    }
    $state == "Completed"
  }
  let url = (kget backups.longhorn.io $tag "{.status.url}" -n $LH_NS)

  let pod_env = {OUT: $out, VOL: $v.name, PVC: $"($v.namespace)/($v.pvc)", BACKUP: $tag}
  try {
    print "  file export"
    kapply (cold-claim $"export-($v.name)" $"src-($short)" $v.name $v.size $fs)
    cold-run-pod $"export-($short)" $"src-($short)" $COLD_EXPORT_SCRIPT $pod_env
    # The source is detached again before the restore check starts
    ^kubectl -n $COLD_NS delete pod $"export-($short)" --wait=true | ignore

    print "  restore check"
    kapply [{
      apiVersion: "longhorn.io/v1beta2"
      kind: "Volume"
      metadata: {name: $"restore-($short)", namespace: $LH_NS}
      spec: {size: ($v.size | into string), numberOfReplicas: 1, frontend: "blockdev", dataEngine: "v1", accessMode: "rwo", fromBackup: $url}
    }]
    wait-for $"restore of ($tag)" 2hr {
      (kget volumes.longhorn.io $"restore-($short)" "{.status.restoreRequired}/{.status.state}" -n $LH_NS) == "false/detached"
    }
    kapply (cold-claim $"restore-($short)" $"restore-($short)" $"restore-($short)" $v.size $fs)
    cold-run-pod $"verify-($short)" $"restore-($short)" $COLD_VERIFY_SCRIPT $pod_env
  } catch { |err|
    cold-cleanup $short $v.name
    error make {msg: $"($v.namespace)/($v.pvc): ($err.msg)"}
  }
  cold-cleanup $short $v.name
  ^kubectl -n $LH_NS label backups.longhorn.io $tag cold-verified=true --overwrite | ignore
  print "  OK (source volume kept)"
}

# Archive detached Longhorn volumes to the NAS (block backup + file export, verified by a restore)
export def "nur longhorn cold-archive" [
  target: string   # <namespace>/<pvc>, <namespace> (all its detached volumes) or a volume name
  --as: string = ""  # With a volume name: the <namespace>/<pvc> it belongs to, when Longhorn no longer knows
]: nothing -> nothing {
  let all = (lh-volumes)
  let matches = if ($target | str starts-with "pvc-") {
    let found = ($all | where name == $target)
    if ($as | is-empty) {
      $found
    } else {
      let p = ($as | split row "/")
      $found | update namespace ($p | first) | update pvc ($p | last)
    }
  } else if ($target | str contains "/") {
    let p = ($target | split row "/")
    $all | where namespace == ($p | first) and pvc == ($p | last)
  } else {
    $all | where namespace == $target and state == "detached"
  }
  if ($matches | is-empty) {
    error make {msg: $"No Longhorn volume matches ($target)"}
  }
  # A namespace an earlier run is still deleting would take this run's
  # PVCs and pods with it.
  wait-for $"namespace ($COLD_NS) to finish deleting" 10min {
    (kget namespace $COLD_NS "{.status.phase}") != "Terminating"
  }
  ^kubectl create namespace $COLD_NS --dry-run=client -o yaml | ^kubectl apply -f - | ignore
  cold-nas-claim
  for v in $matches {
    # Two volumes can share a namespace/pvc (an app re-created its PVC):
    # tell their exports apart by the volume's short id.
    let dups = ($all | where namespace == $v.namespace and pvc == $v.pvc | length)
    let base = $"exports/($v.namespace)/($v.pvc)"
    let out_rel = if $dups > 1 { $"($base)-($v.name | str substring 4..11)" } else { $base }
    cold-archive-one $v $out_rel
  }
  ^kubectl delete namespace $COLD_NS --ignore-not-found --wait=true | ignore
  ^kubectl delete pv $COLD_NAS_PV --ignore-not-found | ignore
}

# Detached Longhorn volumes, their app's state in clusters/dev/apps, and how far their cold archive got
export def "nur longhorn cold-list" []: nothing -> table {
  let backups = (
    ^kubectl -n $LH_NS get backups.longhorn.io -o json | from json | get items
    | where { |b| $b.metadata.name | str starts-with "cold-" }
    | each { |b|
      {
        volume: ($b.metadata.labels | get --optional backup-volume | default "")
        backup: ($b.status.state? | default "")
        verified: (($b.metadata.labels | get --optional cold-verified | default "") == "true")
      }
    }
  )
  lh-volumes | where state == "detached" | each { |v|
    let f = $"clusters/dev/apps/($v.namespace).nix"
    let app = if ($v.namespace | is-empty) {
      "-"
    } else if not ($f | path exists) {
      "gone"
    } else {
      open $f | parse --regex '(?m)^    enable = (?P<e>true|false);' | get --optional 0.e | default "?"
    }
    let b = ($backups | where volume == $v.name | get --optional 0 | default {backup: "", verified: false})
    {
      pvc: $"($v.namespace)/($v.pvc)"
      volume: $v.name
      actual: ($v.actual | into filesize)
      app_enabled: $app
      backup: $b.backup
      verified: $b.verified
    }
  } | sort-by app_enabled pvc
}

# ─── SQLite (app databases on Longhorn PVCs) ────────────────────────────────

const SQLITE_BACKUP_DIR = "./backups/sqlite"
const SQLITE_IMAGE = "alpine:3.21"
# Apps keep their own copies under backups/ (e.g. slskd's pre-migration ones)
const SQLITE_EXCLUDE = '/backups?/'

# Longhorn-backed PVCs in a namespace. NFS volumes are skipped: SQLite isn't
# safe on them, and walking a media share for databases would take forever.
def sqlite-claims [ns: string]: nothing -> list<string> {
  let pvs = (
    try {
      ^kubectl get pv -o json | from json | get items
    } catch { |err|
      error make {msg: $"Failed to list PersistentVolumes: ($err.msg)"}
    }
  )
  let longhorn = (
    $pvs
    | where { |pv| ($pv.spec.csi?.driver? | default "") == "driver.longhorn.io" }
    | each { |pv| $pv.metadata.name }
  )
  let pvcs = (
    try {
      ^kubectl get pvc -n $ns -o json | from json | get items
    } catch { |err|
      error make {msg: $"Failed to list PVCs in ($ns): ($err.msg)"}
    }
  )
  $pvcs
  | where { |c| ($c.spec.volumeName? | default "") in $longhorn }
  | each { |c| $c.metadata.name }
}

# Running or pending pods (other than our helpers) that mount any of `claims`
def sqlite-users [ns: string, claims: list<string>]: nothing -> list<record> {
  let pods = (
    try {
      ^kubectl get pods -n $ns -o json | from json | get items
    } catch { |err|
      error make {msg: $"Failed to list pods in ($ns): ($err.msg)"}
    }
  )
  $pods
  | where { |p| not ($p.metadata.name | str starts-with "sqlite-helper-") }
  | where { |p| $p.status.phase in ["Running", "Pending"] }
  | where { |p|
    $p.spec.volumes? | default [] | any { |v| ($v.persistentVolumeClaim?.claimName? | default "") in $claims }
  }
  | each { |p| {name: $p.metadata.name, node: ($p.spec.nodeName? | default "")} }
}

# Start a pod with sqlite3 that mounts each claim at /pvc/<claim>
def sqlite-helper-up [ns: string, claims: list<string>]: nothing -> string {
  let name = $"sqlite-helper-(date now | format date '%s')"
  # A Longhorn RWO volume attaches to one node at a time, so join the app on
  # its node while it's running
  let node = (sqlite-users $ns $claims | where node != "" | get node? | default [] | first 1)
  let vols = ($claims | enumerate)
  let pod_spec = {
    apiVersion: "v1"
    kind: "Pod"
    metadata: {name: $name, namespace: $ns}
    spec: ({
      restartPolicy: "Never"
      terminationGracePeriodSeconds: 1
      containers: [{
        name: "sqlite"
        image: $SQLITE_IMAGE
        command: ["sh", "-c", "apk add --no-cache -q sqlite && touch /tmp/ready && sleep 3600"]
        readinessProbe: {exec: {command: ["test", "-f", "/tmp/ready"]}, periodSeconds: 2}
        volumeMounts: ($vols | each { |v| {name: $"v($v.index)", mountPath: $"/pvc/($v.item)"} })
      }]
      volumes: ($vols | each { |v| {name: $"v($v.index)", persistentVolumeClaim: {claimName: $v.item}} })
    } | merge (if ($node | is-empty) { {} } else { {nodeName: ($node | first)} }))
  }
  try {
    $pod_spec | to yaml | ^kubectl apply -f - | ignore
    ^kubectl -n $ns wait --for=condition=Ready $"pod/($name)" --timeout=180s | ignore
  } catch { |err|
    sqlite-helper-down $ns $name
    error make {msg: $"Helper pod ($name) didn't become ready: ($err.msg)"}
  }
  $name
}

def sqlite-helper-down [ns: string, pod: string]: nothing -> nothing {
  try {
    ^kubectl -n $ns delete pod $pod --ignore-not-found=true --wait=false | ignore
  } catch { |err|
    print $"Warning: failed to delete helper pod ($ns)/($pod): ($err.msg)"
  }
}

# SQLite files under /pvc in the helper pod, found by their header (so .db,
# .sqlite, .sqlite3 and extensionless files all count; -wal/-shm don't)
def sqlite-find [ns: string, pod: string]: nothing -> table {
  let script = 'find /pvc -maxdepth 6 -type f -size +0 2>/dev/null | while IFS= read -r f; do
  [ "$(head -c 15 "$f" 2>/dev/null)" = "SQLite format 3" ] && printf "%s\t%s\n" "$(stat -c %s "$f")" "$f"
done; true'
  let out = (
    try {
      ^kubectl exec -n $ns $pod -- sh -c $script
    } catch { |err|
      error make {msg: $"Failed to search for databases: ($err.msg)"}
    }
  )
  $out
  | lines
  | where { |it| $it | is-not-empty }
  | parse "{size}\t{path}"
  | each { |r|
    let rel = ($r.path | str replace '/pvc/' '')
    let claim = ($rel | split row '/' | first)
    {claim: $claim, path: ($rel | str replace $"($claim)/" ''), size: ($r.size | into filesize)}
  }
}

# Back up every database on `claims` to <out_dir>/<claim>/<path>.gz
def sqlite-backup-run [
  ns: string
  claims: list<string>
  exclude: string
  out_dir: string
]: nothing -> table {
  let pod = (sqlite-helper-up $ns $claims)
  # .backup uses SQLite's online backup API, so the copy is consistent even
  # while the app is writing. Any -wal/-shm the CLI creates is handed back to
  # the database's owner so a non-root app can still open it.
  let script = 'set -e
f="$1"
sqlite3 -cmd ".timeout 30000" "$f" ".backup /tmp/out.db"
o=$(stat -c %u:%g "$f")
for x in "$f-wal" "$f-shm"; do [ -e "$x" ] && chown "$o" "$x"; done
r=$(sqlite3 /tmp/out.db "PRAGMA integrity_check;")
[ "$r" = ok ] || { echo "integrity_check: $r" >&2; exit 1; }
gzip -c /tmp/out.db
rm -f /tmp/out.db'
  let result = (
    try {
      let dbs = (
        sqlite-find $ns $pod
        | where { |d| $exclude == "" or not ($"/($d.path)" =~ $exclude) }
      )
      if ($dbs | is-empty) {
        print $"No SQLite databases found on: ($claims | str join ', ')"
      }
      $dbs | each { |d|
        let dest = ([$out_dir $d.claim $"($d.path).gz"] | path join)
        mkdir ($dest | path dirname)
        print $"Backing up: ($d.claim)/($d.path) \(($d.size)\)"
        ^kubectl exec -n $ns $pod -- sh -c $script sh $"/pvc/($d.claim)/($d.path)" | save --raw --force $dest
        # A failed exec leaves an empty or truncated file, which gzip -t rejects
        try {
          ^gzip -t $dest
        } catch {
          rm -f $dest
          error make {msg: $"Backup of ($d.claim)/($d.path) failed \(see the error above\)"}
        }
        print $"  ✓ ($dest)"
        {claim: $d.claim, path: $d.path, size: $d.size, file: $dest, compressed: (ls $dest | get size | first)}
      }
    } catch { |err|
      sqlite-helper-down $ns $pod
      error make {msg: $"SQLite backup failed: ($err.msg)"}
    }
  )
  sqlite-helper-down $ns $pod
  $result
}

# List the SQLite databases on an app's Longhorn PVCs
export def "nur sqlite list" [
  namespace: string   # The app's namespace (usually its name)
]: nothing -> table {
  let claims = (sqlite-claims $namespace)
  if ($claims | is-empty) {
    error make {msg: $"No Longhorn PVCs in namespace ($namespace)"}
  }
  let pod = (sqlite-helper-up $namespace $claims)
  let dbs = (
    try {
      sqlite-find $namespace $pod
    } catch { |err|
      sqlite-helper-down $namespace $pod
      error make {msg: $err.msg}
    }
  )
  sqlite-helper-down $namespace $pod
  $dbs | insert excluded { |d| $"/($d.path)" =~ $SQLITE_EXCLUDE }
}

# Back up an app's SQLite databases (consistent online copies, gzipped)
export def "nur sqlite backup" [
  namespace: string                   # The app's namespace (usually its name)
  --pvc: string = ""                  # Only this PVC (default: all Longhorn PVCs in the namespace)
  --exclude: string = $SQLITE_EXCLUDE # Skip database paths matching this regex ("" keeps all)
  --output-dir: string = $SQLITE_BACKUP_DIR
]: nothing -> table {
  let claims = if ($pvc | is-empty) { sqlite-claims $namespace } else { [$pvc] }
  if ($claims | is-empty) {
    error make {msg: $"No Longhorn PVCs in namespace ($namespace)"}
  }
  let timestamp = (date now | format date '%Y%m%d_%H%M%S')
  let out_dir = ([$output_dir $namespace $timestamp] | path join)
  print $"=== SQLite Backup: ($namespace) ==="
  print $"PVCs: ($claims | str join ', ') | Output: ($out_dir)"
  print ""
  sqlite-backup-run $namespace $claims $exclude $out_dir
}

# Restore SQLite databases from a `nur sqlite backup` run. The app must be
# scaled to 0 first; its current databases are backed up before overwriting.
export def "nur sqlite restore" [
  path: string   # A run directory (backups/sqlite/<ns>/<timestamp>) or one .gz file inside it
]: nothing -> table {
  if not ($path | path exists) {
    error make {msg: $"Not found: ($path)"}
  }
  let files = if ($path | path type) == "dir" {
    glob ($path | path expand | path join "**" "*.gz")
  } else {
    [($path | path expand)]
  }
  if ($files | is-empty) {
    error make {msg: $"No .gz backups under ($path)"}
  }
  let targets = (
    $files | each { |f|
      let m = ($f | parse --regex '/(?P<ns>[^/]+)/(?P<ts>\d{8}_\d{6}[^/]*)/(?P<claim>[^/]+)/(?P<rel>.+)\.gz$')
      if ($m | is-empty) {
        error make {msg: $"($f) isn't laid out as <ns>/<YYYYMMDD_HHMMSS>[-pre-restore]/<pvc>/<path>.gz"}
      }
      $m | first | insert file $f
    }
  )
  let namespaces = ($targets | get ns | uniq)
  if ($namespaces | length) != 1 {
    error make {msg: $"Backups span several namespaces: ($namespaces | str join ', ')"}
  }
  let ns = ($namespaces | first)
  let claims = ($targets | get claim | uniq)

  let users = (sqlite-users $ns $claims)
  if ($users | is-not-empty) {
    error make {msg: $"($users | get name | str join ', ') still mount ($claims | str join ', '). Scale the app to 0 first: set its replicas to 0 in clusters/dev/apps/ and deploy \(not `kubectl scale`, which ArgoCD reverts\), then re-run."}
  }

  print $"=== SQLite Restore: ($ns) ==="
  print "Backing up the current databases first..."
  let safety_dir = ([$SQLITE_BACKUP_DIR $ns $"(date now | format date '%Y%m%d_%H%M%S')-pre-restore"] | path join)
  sqlite-backup-run $ns $claims "" $safety_dir | ignore
  print ""

  let pod = (sqlite-helper-up $ns $claims)
  # Check the copy before touching the live file, keep its owner and mode,
  # and drop any stale -wal/-shm, which would otherwise be replayed onto it
  let script = 'set -e
t="$1"
gunzip -c > /tmp/in.db
r=$(sqlite3 /tmp/in.db "PRAGMA integrity_check;")
[ "$r" = ok ] || { echo "integrity_check: $r" >&2; exit 1; }
if [ -e "$t" ]; then o=$(stat -c %u:%g "$t"); m=$(stat -c %a "$t")
else mkdir -p "$(dirname "$t")"; o=$(stat -c %u:%g "$(dirname "$t")"); m=644; fi
cp /tmp/in.db "$t.restore-tmp"
chown "$o" "$t.restore-tmp"; chmod "$m" "$t.restore-tmp"
rm -f "$t-wal" "$t-shm" "$t-journal"
mv "$t.restore-tmp" "$t"
rm -f /tmp/in.db'
  let result = (
    try {
      $targets | each { |t|
        print $"Restoring: ($t.claim)/($t.rel)"
        open --raw $t.file | ^kubectl exec -i -n $ns $pod -- sh -c $script sh $"/pvc/($t.claim)/($t.rel)"
        print "  ✓"
        {claim: $t.claim, path: $t.rel, from: $t.file}
      }
    } catch { |err|
      sqlite-helper-down $ns $pod
      error make {msg: $"SQLite restore failed: ($err.msg). Pre-restore copies are in ($safety_dir)"}
    }
  )
  sqlite-helper-down $ns $pod
  print ""
  print $"=== Restore Complete. Pre-restore copies: ($safety_dir) ==="
  print "Scale the app back up (replicas in clusters/dev/apps/, then deploy)."
  $result
}

# ─── NAS archive (RECOVERY_BACKUP USB drive) ────────────────────────────────
# Moves folders off the NAS's /volume1 onto the USB drive attached to it.
# The work happens on the NAS (scripts/nas-archive.sh, installed on every
# run) as a systemd unit, so it survives this machine sleeping.

const NAS_HOST = "192.168.0.124"
const NAS_ARCHIVE_SCRIPT = "scripts/nas-archive.sh"
const NAS_ARCHIVE_BIN = "/usr/local/bin/nas-archive"
# Share -> the *arr app whose database tracks folders in it, and where that
# share is mounted in the app's pod.
const NAS_ARR = {
  TV: {app: "sonarr", table: "Series", root: "/tv"}
  Movies: {app: "radarr", table: "Movies", root: "/movies"}
}

def sh-quote [s: string]: nothing -> string {
  $"'($s | str replace --all "'" "'\\''")'"
}

def nas-ssh [cmd: string] {
  ^ssh -o BatchMode=yes $NAS_HOST $cmd
}

# Copy this repo's scripts/nas-archive.sh to the NAS (UGOS isn't managed here)
def nas-archive-install []: nothing -> nothing {
  try {
    open --raw $NAS_ARCHIVE_SCRIPT | nas-ssh $"sudo tee ($NAS_ARCHIVE_BIN) >/dev/null && sudo chmod 755 ($NAS_ARCHIVE_BIN)"
  } catch { |err|
    error make {msg: $"Failed to install ($NAS_ARCHIVE_BIN) on the NAS: ($err.msg)"}
  }
}

# Install the script and run it as the transient unit nas-archive-<path>
# (one at a time per path)
def nas-archive-start [rel: string, flags: list<string>]: nothing -> nothing {
  nas-archive-install
  let q = (sh-quote $rel)
  let args = ($flags | append ["--" $q] | str join " ")
  let cmd = $"set -e
unit=\"nas-archive-$\(systemd-escape -- ($q))\"
if systemctl is-active --quiet \"$unit\"; then echo \"$unit is already running\" >&2; exit 1; fi
sudo systemctl reset-failed \"$unit\" 2>/dev/null || true
sudo systemd-run --quiet --unit=\"$unit\" -p Nice=10 -p IOSchedulingClass=best-effort -p IOSchedulingPriority=7 ($NAS_ARCHIVE_BIN) ($args)
echo \"Started $unit\""
  try {
    nas-ssh $cmd
  } catch { |err|
    error make {msg: $"Failed to start the archive job: ($err.msg)"}
  }
  print "Follow it with: nur nas archive-status"
}

# Unmonitor the series/movie a TV/<show> or Movies/<movie> folder belongs
# to, so it isn't downloaded again. Writes the app's database, so only while
# the app is stopped.
def arr-unmonitor [rel: string]: nothing -> nothing {
  let parts = ($rel | str trim --char "/" | split row "/")
  let arr = ($NAS_ARR | get --optional ($parts | first))
  if $arr == null { return }
  if ($parts | length) != 2 {
    print $"Note: ($rel) isn't a whole ($arr.app) folder; not changing what ($arr.app) monitors."
    return
  }
  let running = (^kubectl get pods -n $arr.app --no-headers --ignore-not-found | str trim)
  if ($running | is-not-empty) {
    error make {msg: $"($arr.app) is running. Unmonitor ($parts | last) in its UI first, then re-run with --skip-arr."}
  }
  let path = $"($arr.root)/($parts | last)"
  let sql = $"UPDATE \"($arr.table)\" SET \"Monitored\" = false WHERE \"Path\" = :'p' RETURNING \"Path\";"
  let updated = (
    try {
      $sql | ^kubectl exec -i -n $PG_NS (pg-pod) -- env $"PGPASSWORD=(pg-password)" psql -h localhost -U $PG_USER -p $PG_PORT -d $arr.app -v ON_ERROR_STOP=1 -v $"p=($path)" -At -f -
      | lines | where $it == $path
    } catch { |err|
      error make {msg: $"Failed to unmonitor ($path) in ($arr.app): ($err.msg)"}
    }
  )
  if ($updated | is-empty) {
    print $"($arr.app) doesn't track ($path); nothing to unmonitor."
  } else {
    print $"Unmonitored ($path) in ($arr.app)."
  }
}

# Copy /volume1/<share>/<folder> to the USB drive and sha256-verify it (deleting the source is up to you)
export def "nur nas archive" [
  path: string        # Under /volume1, e.g. "TV/Looney Tunes"
  --dry-run           # Show what would be copied; change nothing
  --skip-arr          # Don't touch Sonarr/Radarr
  --bwlimit: string = "60M" # Copy speed cap (rsync --bwlimit; 0 = none). Heavy I/O crashes this NAS.
]: nothing -> nothing {
  if $dry_run {
    nas-archive-install
    nas-ssh $"sudo ($NAS_ARCHIVE_BIN) --dry-run -- (sh-quote $path)"
    return
  }
  if not $skip_arr {
    arr-unmonitor $path
  }
  nas-archive-start $path [$"--bwlimit=(sh-quote $bwlimit)"]
}

# Re-check an archived folder on the drive against its .archive.sha256
export def "nur nas archive-verify" [
  path: string        # Same path as given to `nur nas archive`
]: nothing -> nothing {
  nas-archive-start $path ["--verify"]
}

# Write a .archive.sha256 (and fix UGOS's 000 directory modes) for a folder moved onto the drive some other way
export def "nur nas archive-adopt" [
  path: string        # Its path on the drive, e.g. "TV/MythBusters"
]: nothing -> nothing {
  nas-archive-start $path ["--adopt"]
}

# Show archive jobs on the NAS, the drive's free space and the end of ARCHIVE.log
export def "nur nas archive-status" []: nothing -> nothing {
  nas-ssh "drive=$(findmnt -rn -S LABEL=RECOVERY_BACKUP -o TARGET)
systemctl list-units --all --no-legend --plain 'nas-archive-*' | awk '{print $1, $3, $4}'
for u in $(systemctl list-units --state=active --no-legend --plain 'nas-archive-*' | awk '{print $1}'); do
  echo \"== $u\"; sudo journalctl -u \"$u\" --no-pager -n 3 -o cat | tr '\\r' '\\n' | tail -n 3
done
echo '== drive'; df -h \"$drive\" | tail -n 1
echo '== ARCHIVE.log'; tail -n 15 \"$drive/ARCHIVE.log\" 2>/dev/null || echo '(empty)'"
}

# Register git hooks for this repo (no .githooks/ committed yet -- fleetops'
# own pre-commit hook auto-ran a full k8s switch+push on every commit, which
# doesn't fit dotfiles' separate manifests-repo workflow; add hooks here
# deliberately if/when wanted rather than carrying that behavior over)
export def "nur apply-git-hooks" []: nothing -> nothing {
  ^git config core.hooksPath .githooks
}

# ---------------------------------------------------------------------------
# Dashboard UI components (resources/apps/dashboard-ui/), with Storybook.
# Deployed by `nur switch k8s` as the `storybook` app; these tasks are for
# working on it locally.
# ---------------------------------------------------------------------------

const UI_DIR = "resources/apps/dashboard-ui"
const UI_NIX = "modules/kubernetes/pkgs/dashboard-ui.nix"

def ui-deps []: nothing -> nothing {
  if not ($"($UI_DIR)/node_modules" | path exists) {
    ^npm install --prefix $UI_DIR
  }
}

# Run Storybook locally with live reload (http://localhost:6006), for changes
# that aren't deployed yet
export def "nur ui dev" []: nothing -> nothing {
  ui-deps
  ^npm run --prefix $UI_DIR storybook
}

# Run the component tests and the typecheck
export def "nur ui test" []: nothing -> nothing {
  ui-deps
  ^npm run --prefix $UI_DIR typecheck
  ^npm test --prefix $UI_DIR
}

# Build the static Storybook with Nix, as the cluster gets it (tests included)
export def "nur ui build" []: nothing -> string {
  ^nix build .#dashboard-ui-storybook --no-link --print-out-paths | str trim
}

# Update npmDepsHash in the Nix package after package-lock.json changes
export def "nur ui hash" []: nothing -> nothing {
  let hash = (^nix run nixpkgs#prefetch-npm-deps -- $"($UI_DIR)/package-lock.json" | lines | last | str trim)
  open --raw $UI_NIX
  | str replace --regex 'npmDepsHash = "[^"]*";' $'npmDepsHash = "($hash)";'
  | save --force $UI_NIX
  print $"npmDepsHash = ($hash)"
}

# ---------------------------------------------------------------------------
# Inbox review: a Windmill full-code app built from dashboard-ui. It's a draft
# under u/duck/, which the windmill-sync hook leaves alone (it owns only f/),
# so it's deployed with `nur inbox push`, not `nur switch k8s`.
# ---------------------------------------------------------------------------

const INBOX_APP_DIR = "resources/apps/windmill/wmill/u/duck/inbox_review.raw_app"
const INBOX_APP_PATH = "u/duck/inbox_review"

def wmill-bin []: nothing -> string {
  let out = (^nix build .#wmill-cli --no-link --print-out-paths | str trim)
  $"($out)/bin/wmill"
}

# Reinstall the app's dependencies, picking up dashboard-ui changes (it's
# installed as a copy, see the app's .npmrc)
def inbox-deps []: nothing -> nothing {
  rm -rf $"($INBOX_APP_DIR)/node_modules/dashboard-ui"
  ^npm install --prefix $INBOX_APP_DIR --no-audit --no-fund
}

# Run the inbox review app locally with live reload; backend calls run on the
# real Windmill workers. The browser is opened with xdg-open: wmill's own
# opener only recognizes Chrome, Brave, Firefox and Edge, so it fails on Zen.
export def "nur inbox dev" [
  --no-open  # Don't open a browser
]: nothing -> nothing {
  let wmill = (wmill-bin)
  let port = 4567
  inbox-deps
  if not $no_open {
    job spawn { sleep 3sec; ^xdg-open $"http://localhost:($port)" } | ignore
  }
  cd $INBOX_APP_DIR
  ^$wmill app dev . --port $port --no-open
}

# A copy of a raw app ready to push: backend files that import shared Windmill
# modules ("/f/lib/...") get those modules bundled in. Windmill redeploys
# anything importing a script whenever that script is redeployed (every
# windmill sync), and a server-side redeploy of a raw app keeps no frontend
# bundle, which breaks the app. Bundled, the pushed app imports nothing from
# f/, while the source in git still shares the module.
def raw-app-stage [app_dir: string]: nothing -> string {
  let wmill_root = ("resources/apps/windmill/wmill" | path expand)
  let stage = (^mktemp -d | str trim)
  let src = ($app_dir | path expand)
  ls -a $src | where name !~ '/node_modules$' | each {|f| cp -r $f.name $stage }
  ^ln -s ($src | path join node_modules) ($stage | path join node_modules)
  ls ($stage | path join backend) | where name =~ '\.ts$' | each {|f|
    let code = (open --raw $f.name)
    if ($code | str contains '"/f/') {
      $code | str replace --all '"/f/' $'"($wmill_root)/f/' | save --force $f.name
      ^nix shell nixpkgs#esbuild -c esbuild $f.name --bundle --format=esm --platform=node --target=esnext --external:windmill-client --external:bun --outfile=($f.name) --allow-overwrite --log-level=warning
    }
  }
  $stage
}

# Bundle and deploy the inbox review app to Windmill (u/duck/inbox_review)
export def "nur inbox push" []: nothing -> nothing {
  let wmill = (wmill-bin)
  inbox-deps
  let stage = (raw-app-stage $INBOX_APP_DIR)
  do { cd $stage; ^$wmill app push . $INBOX_APP_PATH }
  rm -rf $stage
}

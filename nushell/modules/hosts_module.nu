# Host info from the dotfiles flake: each host's `info` (see modules/flake/hosts.nix),
# written by home-manager to ~/.config/dotfiles/hosts.json. Reflects the last
# `nur switch home`, not the working tree; use `nix eval --json .#hostInfo` for that.

def hosts-file []: nothing -> path {
  $env.XDG_CONFIG_HOME? | default ($env.HOME | path join .config) | path join dotfiles hosts.json
}

def hosts-data []: nothing -> record {
  open (hosts-file)
}

def "nu-complete hosts" []: nothing -> list<string> {
  hosts list | get hostname
}

# List every host's info
export def "hosts list" []: nothing -> table {
  hosts-data | get hosts
}

# Get one host's info (defaults to this host)
export def "hosts get" [
  host?: string@"nu-complete hosts" # hostname (flake attribute name)
]: nothing -> record {
  let data = hosts-data
  let name = $host | default $data.current
  let matches = $data.hosts | where hostname == $name
  if ($matches | is-empty) {
    error make { msg: $"unknown host: ($name)" }
  }
  $matches | first
}

# List the features enabled on a host (defaults to this host)
export def "hosts features" [
  host?: string@"nu-complete hosts" # hostname (flake attribute name)
]: nothing -> list<string> {
  hosts get $host | get features
}

let carapace_completer = {|spans|
  carapace $spans.0 nushell ...$spans
    | from json
    | if ($in | default [] | where value == $"($spans | last)ERR" | is-empty) { $in } else { null }
}

let fish_completer = {|spans|
  fish --private -i --command $'complete --do-complete "($spans | str join " ")"'
    | from tsv --flexible --noheaders --no-infer
    | rename value description
    | update cells --columns ["value"] { ansi strip }
}

# This completer will use carapace by default
let external_completer = {|spans|
  let expanded_alias = scope aliases
    | where name == $spans.0
    | get --optional 0.expansion

  let spans = if $expanded_alias != null {
    $spans
      | skip 1
      | prepend ($expanded_alias | split row ' ' | take 1)
    } else {
      $spans
    }

    match $spans.0 {
      ag => $fish_completer
      alacritty => $fish_completer
      asdf => $fish_completer
      argocd => $fish_completer
      az => $fish_completer
      doctl => $fish_completer
      git => $fish_completer
      jj => $fish_completer
      k3d => $fish_completer
      keepassxc-cli => $fish_completer
      # mc => $fish_completer
      nix => $fish_completer
      nu => $fish_completer
      playerctl => $fish_completer
      sops => $fish_completer
      tailscale => $fish_completer
      wofi => $fish_completer
      _ => $carapace_completer
    } | do $in $spans
  }

$env.config.completions.external = { enable: true completer: $external_completer }

const NU_LIB_DIRS = [
  '~/.nix-profile/share/nu_scripts/modules'
]

const DOTFILES_DIR = "~/dotfiles"

def save_last_dir [] {
  pwd | save --force ~/.last_dir
}

# Hook to run before every prompt
$env.config.hooks.pre_prompt = [{ save_last_dir }]
$env.config.show_banner = false

# taken from somewhere

# Helper functions

# Filter directory by file name.
def lls [
    term: string # Search target.
] {
    ls | where { $in.name | str contains $term }
}

# Sort directory my modified.
def llm [] {
    ls | sort-by modified
}

# Filter process list by process name.
def pss [
    term: string # Search target.
] {
    ps | where { $in.name | str contains $term }
}

# Watch youtube video
def wy [
    video_id: string # video id
] {
    mpv $"https://youtube.com/watch?v=($video_id)"
}

# Start original

def "_parse keepassxc data" [] {
  lines
    | each { split column ': ' | first | { $in.column1: $in.column2 } }
    | reduce {|it| merge $it }
}

# Close tab from input
def "browsers tabs close" []: string -> any {
    xargs brotab close
}

# Close tab
def "browsers tabs close-first" [] {
    browsers tabs list |
    first |
    get id |
    browsers tabs close
}

# List all browser tabs
def "browsers tabs list" [] {
    bt list |
    from tsv -n |
    each {|x|
      {id: $x.column0, title: $x.column1 url: $x.column2}
    }
}

# convert input from edn into data
def "from edn" []: string -> any {
    jet -o json | from json
}

# Get metadata about cureently playing music
def "player get metadata" [] {
  playerctl metadata
    | split row --regex "\\n"
    | each {|row| $row | split column --regex "\\s\\s+" }
    | each {|row| { $row.column1.0: $row.column2.0 } }
    | reduce {|a b| $a | merge $b }
}

# List all tasks listed in the readme
def "st runme tasks list" []: nothing -> table<name: string, file: string, first_command: string, description: string, named: bool, run_all: bool> {
  runme list --json
    | from json
}

# completer for runme tasks
def "nu-complete st runme run" []: nothing -> table<value: string, description: string> {
  st runme tasks list
    | select name description
    | rename value description
}

# Run commands in readme
extern "runme run" [
  task: string@"nu-complete st runme run"
]

# Switch home-manager to latest flake
def "switch home" [] {
  nh home switch ~/dotfiles  -b backup -- --show-trace
}

# Switch nixos to latest flake
def "switch os" [] {
  nh os switch ~/dotfiles -- --show-trace
}

# Parse a git config row record
def parse-git-config-row [
  row # A row of data from `git config`
] {
  $row.column0
    | split column "="
    | first
    | { ($in.column1 | split column '.' | first): $in.column2 }
}

def "git-config-data" [] {
  # TODO: better way to do this to avoid column0?
  git config -l
    | from tsv --noheaders
    | each {|x| parse-git-config-row $x }
    | reduce {|a acc| $acc | merge $a}
}

$env.nu_menu_commands = {
  {
    description: "Refresh nu"
    keymap: "r"
    command: { nu }
  }
  {
    description: "Git Status"
    keymap: "gs"
    command: { git status }
    group: "Git"
  }
}


use ~/.nix-profile/share/nu_scripts/custom-completions/bat/bat-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/cargo/cargo-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/curl/curl-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/docker/docker-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/git/git-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/nix/nix-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/yarn/yarn-v4-completions.nu *

use ~/nushell/completions/algia-completions.nu *
use ~/nushell/completions/bb-completions.nu *
use ~/nushell/completions/databricks-completions.nu *
use ~/nushell/completions/devspace-completions.nu *
use ~/nushell/completions/earthly-completions.nu *
use ~/nushell/completions/nak-completions.nu *
use ~/nushell/completions/nh-completions.nu *

use ~/nushell/modules/hypr_module.nu *
use ~/nushell/modules/nostr_module.nu *
use ~/nushell/modules/pj_module.nu *
use ~/nushell/modules/platform_module.nu *
use ~/nushell/modules/project_module.nu *

# use kubernetes *

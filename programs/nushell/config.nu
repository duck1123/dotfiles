const NU_LIB_DIRS = [
  '~/.nix-profile/share/nu_scripts/modules'
]

$env.config.show_banner = false

# taken from somewhere

# Helper functions

# Filter directory by file name.
def lls [
    term: string # Search target.
] {
    ls | where { |it| ($it.name | str contains $term ) }
}

# Sort directory my modified.
def llm [] {
    ls | sort-by modified
}

# Filter process list by process name.
def pss [
    term: string # Search target.
] {
    ps | where { |it| ($it.name | str contains $term ) }
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

# Infomation about the active window
def "hypr active-window" [] {
    hyprctl activewindow -j | from json
}

# Data about current active keybinds
def "hypr binds" [] {
    hyprctl binds -j | from json
}

def "hypr clients" [] {
  hyprctl clients -j
    | from json
}

# List all available workspaces
def "hypr workspaces" [] {
    hyprctl workspaces -j | from json
}

# Get information about the currently active workspace
def "hypr workspaces active" [] {
    hyprctl activeworkspace -j | from json
}

# Get metadata about cureently playing music
def "player get metadata" [] {
  playerctl metadata
    | split row --regex "\\n"
    | each {|row| $row | split column --regex "\\s\\s+" }
    | each {|row| { $row.column1.0: $row.column2.0 } }
    | reduce {|a b| $a | merge $b }
}

# List all aliases in a clojure deps file
def "project clojure deps aliases" [] {
  (open deps.edn).aliases
    | columns
}

def "project devspace commands list" [] {
  (open devspace.yaml).commands
    | columns
}

def "project devspace piplines list" [] {
  (open devspace.yaml).pipelines
    | columns
}

def "project devspace ports list" [] {
  devspace list ports -o json | from json
}

def "project devspace vars row parse" [
  row # row from devspace vars list
] {
  ($row | split column '|').0
    | {
      ($in.column1 | str trim):
      ($in.column2 | str trim)
    }
}

def "project devspace vars list" [] {
  devspace list vars
    | split row --regex '\n'
    | drop 1
    | skip 3
    | each {|row| project devspace vars row parse $row }
    | reduce {|a b| $a | merge $b}
}

# List all tasks in an earthly file
def "project earthly tasks" [] {
  earthly ls
    | split row "\n"
    | each {|target| { $target: (bb get-task-args --target $target) }}
    | reduce {|a b| $a | merge $b }
    | sort
}

# Remove duplicates from history
def "platform history dedupe" [] {
  open ~/.config/nushell/history.txt
    | lines
    | reverse
    | uniq
    | reverse
    | save -f ~/.config/nushell/history.txt
}

# Return a command associated with a trove command
def "project hoard show" [
  name # The command to show
] {
  ((open trove.yml).commands | filter { $in.name == $name }).0.command
}

# Parse a mssql cli query response
def "project mssql split result" [] {
  lines
    | skip 2
    | drop 2
    | each { split column --regex " " | first }
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

# List all argo applications
def "platform argo app list" []: nothing -> table<metadata: any, spec: any, status: any> {
  argocd app list -o json
    | from json
}

# Read the argocd password from keepass
def "platform argo password get" [] {
  let kdbxPath = $"($env.HOME)/keepass/passwords.kdbx"
  let passPath = "local/argocd.dev.kronkltd.net"
  keepassxc-cli show $kdbxPath $passPath  -s --all
    | _parse keepassxc data
}

# Refresh the argocd login token
def "platform argo login" [] {
  let domain = "argocd.dev.kronkltd.net"
  let username = "admin"
  let password = (platform argo password get).Password
  argocd login $domain --username $username --password $password
}

# Completer for argo workflows templates
def "nu-complete platform argo template get" [] {
  platform argo template list | get name
}

# Fetch an argo template by name
def "platform argo template get" [
  templateName: string@"nu-complete platform argo template get" # The template to read
] {
  argo template get $templateName -o json
    | from json
}

# List all argo templates
def "platform argo template list" [] {
  argo template list -A
    | lines
    | skip 1
    | each { str trim | split column --regex '\s\s+' | first }
    | rename namespace name
}

# List all workflows
def "platform argo workflow list" [] {
  argo list -A
    | lines
    | skip 1
    | each { str trim | split column --regex '\s\s+' | first }
    | rename namespace name status age duration priority message
}

# List all k8s clusters
def "platform cluster list" [] {
  k3d cluster list -o json
    | from json
}

# List git remptes for project
#
# This is likely obsoleted by stanfard git completer
def "platform git remote" [] {
  # git remote -v
  #   | split row --regex '\n'
  #   | each {|row|
  #     $row
  #       | split column --regex '\t+'
  #       | first
  #   }
  git remote -v
    | detect columns --no-headers
    | rename name url type
}

# Create a minio alias
def "platform minio alias create" [] {
  let minioAlias = "minio"
  let minioHost = "https://minio-api.dev.kronkltd.net"
  let accessKey = $env.MINIO_ACCESS_KEY
  let secretKey = $env.MINIO_SECRET_KEY
  mc alias set $minioAlias $minioHost $accessKey $secretKey
}

# List all minio aliases
def "platform minio alias list" [] {
  mc alias list --json
    | from json --objects
}

# Get information about nix profiles
def "platform nix profile list" [] {
  nix profile list
    | split row --regex  '\n\n'
    | each {
      split row --regex '\n'
        | each {
          split column --regex ':'
            | { ($in.0.column1 | str trim): ($in.0.column2 | str trim) }
        }
        | reduce {|a b| $a | merge $b}
    }
}

# Switch home-manager to latest flake
def "switch home" [] {
  nh home switch ~/dotfiles -- --impure --show-trace
}

# Switch nixos to latest flake
def "switch os" [] {
  nh os switch ~/dotfiles -- --impure --show-trace
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
use ~/nushell/bb-completions.nu bb
use ~/nushell/devspace-completions.nu *
use ~/nushell/earthly-completions.nu *
use ~/nushell/pj_command.nu pj
use ~/nushell/nostr_module.nu *
use kubernetes *

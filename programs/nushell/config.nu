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

def "project mssql split result" [] {
  split row --regex '\n'
  | skip 2
  | drop 2
  | each {|row|
    $row
      | split column --regex " "
      | first
  }
}

def "st runme tasks list" [] {
  runme list --json
    | from json
}

def "platform argo app list" [] {
  argocd app list -o json
    | from json
}

def "platform argo template list" [] {
  argo template list --output name
    | split row --regex '\n'
}

def "platform cluster list" [] {
  k3d cluster list -o json
    | from json
}

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

def "platform minio alias create" [] {
  let minioAlias = "minio"
  let minioHost = "https://minio-api.dev.kronkltd.net"
  let accessKey = $env.MINIO_ACCESS_KEY
  let secretKey = $env.MINIO_SECRET_KEY
  mc alias set $minioAlias $minioHost $accessKey $secretKey
}

def "platform minio alias list" [] {
  mc alias list --json
    | from json --objects
}

def "platform nix profile list" [] {
  nix profile list
    | split row --regex  '\n\n'
    | each { |x|
      $x
        | split row --regex '\n'
        | each {|y|
          $y
            | split column --regex ':'
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
    | { $in.0.column1: $in.0.column2 }
}

def "git-config-data" [] {
  # TODO: better wat to do this to avoif column0?
  git config -l
    | from tsv --noheaders
    | each {|x| parse-git-config-row $x }
    | reduce {|a b| $a | merge $b}
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
use ~/nushell/pj_command.nu pj
use ~/nushell/nostr_module.nu *
use kubernetes *

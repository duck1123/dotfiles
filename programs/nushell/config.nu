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

# List all tasks in an earthly file
def "project earthly tasks" [] {
  earthly ls
    | split row "\n"
    | each {|target| { $target: (bb get-task-args --target $target) }}
    | reduce {|a b| $a | merge $b }
    | sort
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
use ~/.nix-profile/share/nu_scripts/custom-completions/bat/bat-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/cargo/cargo-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/curl/curl-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/docker/docker-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/git/git-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/nix/nix-completions.nu *
use ~/.nix-profile/share/nu_scripts/custom-completions/yarn/yarn-v4-completions.nu *
use ~/nushell/bb-completions.nu bb
use ~/nushell/pj_command.nu pj
use ~/nushell/me.nu *
use ~/nushell/nostr_module.nu *

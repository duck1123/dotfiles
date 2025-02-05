const NU_LIB_DIRS = [
  '~/.nix-profile/share/nu_scripts/modules'
]

# Nushell Config
$env.config = {
  show_banner: false,
  edit_mode: emacs,
  footer_mode: always,
  table: {
    mode: rounded,
    index_mode: always,
    header_on_separator: true,
    padding: { left: 2, right: 1 },
  },
  completions: {
    algorithm: prefix,
    quick: true,
    case_sensitive: false,
    external: {
      enable: true,
      max_results: 50,
      completer: { |spans| carapace $spans.0 nushell ...$spans | from json },
    }
  },
  history: {
    max_size: 10000,
    file_format: sqlite,
  },
  filesize: {
    metric: true,
  },
}

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

# Close tab
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

# list all tabs
def "browsers tabs list" [] {
    bt list |
    from tsv -n |
    each {|x|
      {id: $x.column0, title: $x.column1 url: $x.column2}
    }
}

def "from edn" []: string -> any {
    jet -o json | from json
}

# Infomation about the active window
def "hypr active-window" [] {
    hyprctl activewindow -j | from json
}

def "hypr binds" [] {
    hyprctl binds -j | from json
}

def "hypr workspaces" [] {
    hyprctl workspaces -j | from json
}

def "hypr workspaces active" [] {
    hyprctl activeworkspace -j | from json
}

# Switch home-manager to latest flake
def "switch home" [] {
    nh home switch ~/dotfiles -- --impure --show-trace
}

# Switch nixos to latest flake
def "switch os" [] {
    nh os switch ~/dotfiles -- --impure --show-trace
}

def "single-truth earthly task-args" [] {
  earthly ls
    | split row "\n"
    | each {|target| { $target: (bb get-task-args --target $target) }}
    | reduce {|a b| $a | merge $b }
    | sort
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

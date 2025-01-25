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

# Switch home-manager to latest flake
def "switch home" [] {
    nh home switch ~/dotfiles -- --impure --show-trace
}

def "from edn" [] {
    jet -o json | from json
}

# Get all open tabs as a table
def firefox-tabs [] {
    bt list | from tsv -n | each {|x| {id: $x.column0, title: $x.column1 url: $x.column2}}
}

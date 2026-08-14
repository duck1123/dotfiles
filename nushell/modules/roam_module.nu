# Query org-roam's SQLite cache directly rather than parsing org-mode
# syntax. Cache location set by `user-emacs-directory` in
# modules/features/emacs.nix, kept live by `org-roam-db-autosync-mode`.
def db-path []: nothing -> string {
    $env.ORG_ROAM_DB? | default ($env.HOME | path join ".cache" "emacs" "org-roam.db")
}

def open-db []: nothing -> record {
    open (db-path)
}

# emacsql prints Lisp strings with their surrounding quote characters intact,
# so every text column in the cache comes back double-quoted.
def unquote []: string -> string {
    str trim --char (char dq)
}

# Parse an emacsql-printed alist string, e.g. `(("ID" . "abc") ("TAG" . "x"))`,
# into a record. Non-alist values (e.g. link plists like `(:outline nil)`)
# yield an empty record.
def parse-alist []: string -> record {
    let pairs = ($in | parse --regex '\("(?<key>[^"]*)" \. "(?<value>[^"]*)"\)')
    $pairs | reduce -f {} {|it, acc| $acc | insert $it.key $it.value }
}

# Nodes with quoted columns unwrapped and properties parsed, excluding
# Syncthing version-history and git-internal paths.
def nodes []: nothing -> table {
    open-db
    | get nodes
    | each {|r| {
        id: ($r.id | unquote)
        file: ($r.file | unquote)
        title: ($r.title | unquote)
        properties: ($r.properties | unquote | parse-alist)
    } }
    | where {|r| not ($r.file =~ '\.stversions/') and not ($r.file =~ '/\.git/') }
}

def links []: nothing -> table {
    open-db
    | get links
    | each {|r| {
        source: ($r.source | unquote)
        dest: ($r.dest | unquote)
        type: ($r.type | unquote)
    } }
}

def tags []: nothing -> table {
    open-db
    | get tags
    | each {|r| {
        node_id: ($r.node_id | unquote)
        tag: ($r.tag | unquote)
    } }
}

# Find org-roam nodes whose title matches a substring (case-insensitive).
export def "roam find-title" [pattern: string]: nothing -> table {
    let re = ("(?i)" + $pattern)
    nodes | where {|n| $n.title =~ $re }
}

# Full org-roam node record by id, with parsed properties.
export def "roam node" [id: string]: nothing -> record {
    nodes | where id == $id | first
}

# Nodes that link to the given node id.
export def "roam backlinks" [id: string]: nothing -> table {
    let source_ids = (links | where dest == $id | get source)
    nodes | where {|n| $n.id in $source_ids }
}

# Nodes carrying the given tag.
export def "roam by-tag" [tag: string]: nothing -> table {
    let ids = (tags | where tag == $tag | get node_id)
    nodes | where {|n| $n.id in $ids }
}

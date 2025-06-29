# List all aliases in a clojure deps file
export def "project clojure deps aliases" [] {
  (open deps.edn).aliases
    | columns
}

export def "project devspace commands list" [] {
  (open devspace.yaml).commands
    | columns
}

export def "project devspace piplines list" [] {
  (open devspace.yaml).pipelines
    | columns
}

export def "project devspace ports list" [] {
  devspace list ports -o json | from json
}

export def "project devspace vars row parse" [
  row # row from devspace vars list
] {
  ($row | split column '|').0
    | {
      ($in.column1 | str trim):
      ($in.column2 | str trim)
    }
}

export def "project devspace vars list" [] {
  devspace list vars
    | split row --regex '\n'
    | drop 1
    | skip 3
    | each {|row| project devspace vars row parse $row }
    | reduce {|a b| $a | merge $b}
}

# List all tasks in an earthly file
export def "project earthly tasks" [] {
  earthly ls
    | split row "\n"
    | each {|target| { $target: (bb get-task-args --target $target) }}
    | reduce {|a b| $a | merge $b }
    | sort
}

# Return a command associated with a trove command
export def "project hoard show" [
  name # The command to show
] {
  ((open trove.yml).commands | where { $in.name == $name }).0.command
}

# Parse a mssql cli query response
export def "project mssql split result" [] {
  lines
    | skip 2
    | drop 2
    | each { split column --regex " " | first }
}

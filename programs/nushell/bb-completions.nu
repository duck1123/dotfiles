export def "bb-task-complete" [] {
  ^bb tasks
    | split row --regex "\\n"
    | skip 2
    | each {|row| $row | split row --regex "\\s\\s+" }
    | each {|row| { value: $row.0 description: (if ($row | length) > 1 {$row | get 1} else {""}) } }
}

def "nu-complete bb" [] {
  (bb-task-complete)
}

# Babashka
export extern "bb" [
  task?: string@"nu-complete bb"
  --help         # Prints help information
]

def "bb-task-data" [] {
  let config = open bb.edn
  $config.tasks
    | columns
    | each {|task|
        let fields = $config.tasks | get $task
        { name: $task ...$fields }
      }
}

export def "nu-complete bb" [] {
  ^bb tasks
    | split row --regex "\\n"
    | skip 2
    | each { split row --regex "\\s+" }
    | each {|row|
        {
          value: $row.0
          description:
            (
              if ($row | length) > 1 {
                $row | skip 1 | str join " "
              } else {
                ""
              }
            )
        }
      }
}

# Babashka
export extern "bb" [
  task?: string@"nu-complete bb"
  --help         # Prints help information
]

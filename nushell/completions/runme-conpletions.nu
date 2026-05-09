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


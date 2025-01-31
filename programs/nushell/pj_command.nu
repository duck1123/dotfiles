# Project Jump

# Inspired by the oh-my-zsh plugin (https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/pj)

def "nu-complete pj" [] {
    # Collect all subdirectories from the base directories
    (($env.PROJECT_PATHS
      | each {|base| ls $base | where type == "dir" | get name }
      | flatten)
      ++ $env.STANDALONE_PROJECTS)
    | path basename
}

# Project Jump
export def --env pj [
    subdir?: string@"nu-complete pj" # The directory to switch to
] {
  let target_dir = (
    (($env.PROJECT_PATHS | each {|base| $"($base)/($subdir)"})
      ++ $env.STANDALONE_PROJECTS)
    | where {|dir| $dir | path exists }
    | first)

  if ($target_dir | is-empty) {
    print $"Directory ($subdir) not found in any base directories."
  } else {
    cd $target_dir
  }
}

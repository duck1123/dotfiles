# Define the auto-completion function

def "nu-complete pj" [] {
    let base_dirs = [
        $"($env.HOME)/projects",
    ]

    # Collect all subdirectories from the base directories
    $base_dirs
    | each {|base| ls $base | where type == "dir" | get name }
    | flatten
    | path basename
}

export def --env pj [
    subdir?: string@"nu-complete pj",
] {
  let base_dirs = [
    $"($env.HOME)/projects",
  ]

  let target_dir = ($base_dirs
    | each {|base| $"($base)/($subdir)"}
    | where {|dir| $dir | path exists }
    | first)

  if ($target_dir | is-empty) {
    print $"Directory ($subdir) not found in any base directories."
  } else {
    # overlay use $target_dir
    cd $target_dir
    # $target_dir
  }
}

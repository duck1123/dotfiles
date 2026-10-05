# Comic library helpers. Bookorbit expects every comic file to sit in its own folder named
# after the file, e.g. `Series/Deadpool - #00/Deadpool - #00.cbz`.

const comic_extensions = [cbz cbr cb7 cbt]

# List comic files that aren't in a folder named after the file
export def "books loose" [
  root: path = /mnt/books # library root
]: nothing -> table {
  cd $root
  glob '**/*' --no-dir --exclude ['**/#recycle/**' '**/.Trash-*/**']
  | where { ($in | path parse | get extension | str lowercase) in $comic_extensions }
  | each {|file|
    let parsed = $file | path parse
    if ($parsed.parent | path basename) != $parsed.stem {
      {file: $file, target: ($parsed.parent | path join $parsed.stem ($file | path basename))}
    }
  }
  | sort-by file
}

# Move each loose comic file into its own folder named after the file
export def "books fix-loose" [
  root: path = /mnt/books # library root
  --dry-run (-n) # only show what would be moved
]: nothing -> table {
  books loose $root
  | each {|it|
    let dir = $it.target | path dirname
    let status = if ($it.target | path exists) {
      "skipped: target exists"
    } else if $dry_run {
      "would move"
    } else {
      mkdir $dir
      mv $it.file $it.target
      "moved"
    }
    $it | insert status $status
  }
}

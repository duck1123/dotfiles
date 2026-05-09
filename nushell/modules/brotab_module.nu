# Close tab from input
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

# List all browser tabs
def "browsers tabs list" [] {
    bt list |
    from tsv -n |
    each {|x|
      {id: $x.column0, title: $x.column1 url: $x.column2}
    }
}


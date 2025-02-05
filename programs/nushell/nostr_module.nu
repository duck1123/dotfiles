def "nostr bookmarks get" [] {
  algia bm-list --json
    | from json --objects
}

def "nostr profile get" [] {
  algia profile --json
    | from json
}

# A stream of random nostr events
def "nostr stream public" [] {
  algia stream
    | from json --objects
    | each {|event| $event.pubkey + " - " + $event.content}
}

# Get your timeline as events
def "nostr timeline" [
  user?: string # Show the timeline of this user
] {
  echo $user

  if $user {
    echo "got a user"
    # (algia timeline --json --extra
    #   | from json --objects).event
  } else {
    (algia timeline --json --extra
      | from json --objects).event
  }
}

export def "nostr bookmarks get" [] {
  algia bm-list --json
    | from json --objects
}

export def "nostr event get" [
  eventIdHex
] {
  let nevent = nak encode nevent $in
  let relay = "wss://nos.lol/"
  nak fetch --relay $relay $nevent
    | from json
}

export def "nostr profile get" [
  -u: string
] {
  if ($u != null) {
    algia profile -u $u --json
      | from json
  } else {
    algia profile --json
      | from json
  }
}

# A stream of random nostr events
export def "nostr stream public" [] {
  algia stream
    | from json --objects
    | each {|event| $event.pubkey + " - " + $event.content}
}

# Get your timeline as events
export def "nostr timeline" [
  user?: string # Show the timeline of this user
] {
  echo $user

  if $user != null {
    echo "got a user"
    # (algia timeline --json --extra
    #   | from json --objects).event
  } else {
    (algia timeline --json --extra
      | from json --objects).event
  }
}

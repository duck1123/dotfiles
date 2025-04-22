# the nostr army knife command-line tool
export extern "nak" []

# generates encoded REQ messages and optionally use them to talk to relays
export extern "nak req" [
  # always perform nip42 "AUTH" when facing an "auth-required: " rejection and try again
  --auth
]

# generates encoded COUNT messages and optionally use them to talk to relays
export extern "nak count" []

# generates encoded COUNT messages and optionally use them to talk to relays
export extern "nak fetch" []

# generates an encoded event and either prints it or sends it to a set of relays
export extern "nak event" []
export extern "nak decode" []
export extern "nak encode" []
export extern "nak key" []
export extern "nak verify" []
export extern "nak relay" []

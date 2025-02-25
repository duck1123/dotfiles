export extern "earthly" []

export extern "earthly account" []

# Bootstraps earthly installation including buildkit image download and optionally shell autocompletion
export extern "earthly bootstrap" []

export extern "earthly cloud" []
export extern "earthly cloud install" []
export extern "earthly cloud use" []
export extern "earthly cloud list" []
export extern "earthly cloud rm" []
export extern "earthly cloud help" []

export extern "earthly config" []

export extern "earthly docker-build" []
export extern "earthly doc" []
export extern "earthly init" []
export extern "earthly ls" []

export def "earthly list tasks" [] {
  earthly ls | lines
}

export extern "earthly org" []
export extern "earthly project" []
export extern "earthly prune" []
export extern "earthly prune-auto-skip" []
export extern "earthly registry" []
export extern "earthly satellite" []
export extern "earthly secret" []
export extern "earthly web" []
export extern "earthly billing" []
export extern "earthly gha" []
export extern "earthly help" []

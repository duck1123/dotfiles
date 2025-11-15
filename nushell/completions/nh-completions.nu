export extern "nh clean" []
export extern "nh clean all" []
export extern "nh clean profile" []
export extern "nh clean user" []

export extern "nh completions" [
  --shell: string # Name of the shell [possible values: bash, elvish, fish, powershell, zsh]
]

export extern "nh home" []
export extern "nh home build" [
  flakeRef
  --dry
  --verbose
  --ask
  --update
  --no-nom
  --diff-provider: string
  --out-link: string
  --configuration: string
  --backup-extension: string
  --help
]
export extern "nh home switch" [
  flakeRef
  --dry
  --verbose
  --ask
  --update
  --no-nom
  --diff-provider: string
  --out-link: string
  --configuration: string
  --backup-extension: string
  --help
]

# NixOS functionality
export extern "nh os" []

# Build the new configuration and make it the boot default
export extern "nh os boot" []

# Build the new configuration
export extern "nh os build" []

# Enter a Nix REPL with the target installable
export extern "nh os repl" []

# Build and activate the new configuration, and make it the boot default
export extern "nh os switch" []

# Build and activate the new configuration
export extern "nh os test" []

export extern "nh search" []

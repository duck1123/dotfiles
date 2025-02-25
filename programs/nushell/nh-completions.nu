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

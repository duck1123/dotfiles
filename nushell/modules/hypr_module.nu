# Infomation about the active window
export def "hypr active-window" [] {
    hyprctl activewindow -j | from json
}

# Data about current active keybinds
export def "hypr binds" [] {
    hyprctl binds -j | from json
}

export def "hypr clients" [] {
  hyprctl clients -j
    | from json
}

# List all available workspaces
export def "hypr workspaces" [] {
    hyprctl workspaces -j | from json
}

# Get information about the currently active workspace
export def "hypr workspaces active" [] {
    hyprctl activeworkspace -j | from json
}

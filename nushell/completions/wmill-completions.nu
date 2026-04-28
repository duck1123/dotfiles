export extern "wmill" []


# Bootstrap a windmill project with a wmill.yaml file
  init


# app related commands
  app


# flow related commands
flow


# script related commands
script


# workspace related commands
workspace, profile


# resource related commands
resource

# resource type related commands
resource-type

# user related commands
user

# variable related commands
variable


# Hub related commands. EXPERIMENTAL. INTERNAL USE ONLY.
hub

# folder related commands
folder


# schedule related commands
schedule

# trigger related commands
trigger

# Launch a dev server that watches for local file changes and auto-pushes them to the remote workspace. Provides live reload for scripts and flows during development.
dev

# sync local with a remote workspaces or the opposite (push or pull)
sync


# Validate Windmill flow, schedule, and trigger YAML files in a directory
lint                [
directory
]


# Manage git-sync settings between local wmill.yaml and Windmill backend
gitsync-settings

# sync local with a remote instance or the opposite (push or pull)
instance

# display worker groups, pull and push worker groups configs
worker-groups


# List all workers grouped by worker groups
workers

# workspace dependencies related commands
queues              [
workspace # the optional workspace to filter by (default to List all queues with their metrics all workspaces) dependencies, deps
]

# Manage jobs (import/export)
jobs

# Manage jobs (list, inspect, cancel)
job

# Manage workspace groups
group

# View audit logs (requires admin)
audit

# Manage API tokens
token

# Generate metadata (locks, schemas) for all scripts, flows, and apps
generate-metadata   [folder]

# Search Windmill documentation.
docs                <query>

# Show all available wmill.yaml configuration options
config

# Show version information
version, --version

# Upgrade wmill executable to latest or given version.
upgrade

# Generate shell completions.
completions

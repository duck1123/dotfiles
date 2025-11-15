# Devspace
export extern "devspace" [
  command?: string
]

 # add          Adds something to devspace.yaml

# Adds something to devspace.yaml
export extern "devspace add" [

]

# Adds a plugin to devspace
export extern "devspace add plugin" [

]

 #  analyze

# Analyzes a kubernetes namespace and checks for potential problems
export extern "devspace analyze" [
  --ignore-pod-restarts # If true, analyze will ignore the restart events of running pods
]

# --patient               If true, analyze will ignore failing pods and events until every deployment, statefulset, replicaset and pods are ready or the timeout is reached
# --timeout int           Timeout until analyze should stop waiting (default 120)
# --wait                  Wait for pods to get ready if they are just starting (default true)

#  attach       Attaches to a container

# Attaches to a container
export extern "devspace attach" []

# -c, --container string        Container name within pod where to execute command
# -h, --help                    help for attach
# --image-selector string   The image to search a pod for (e.g. nginx, nginx:latest, ${runtime.images.app}, nginx:${runtime.images.app.tag})
# -l, --label-selector string   Comma separated key=value selector list (e.g. release=test)
# --pick                    Select a pod (default true)
# --pod string              Pod to open a shell to

#  build        Builds all defined images and pushes them

# Builds all defined images and pushes them
export extern "devspace build" [
  --build-sequential # Builds the images one after another instead of in parallel
  --dependency: string # Deploys only the specified named dependencies
  --force-build # Forces to build every image (default true)
]

  #     --dependency strings          Deploys only the specified named dependencies
  # -b, --force-build                 Forces to build every image (default true)
  # -d, --force-deploy                Forces to deploy every deployment
  #     --force-purge                 Forces to purge every deployment even though it might be in use by another DevSpace project
  # -h, --help                        help for build
  #     --max-concurrent-builds int   The maximum number of image builds built in parallel (0 for infinite)
  #     --pipeline string             The pipeline to execute (default "build")
  #     --render                      If true will render manifests and print them instead of actually deploying them
  #     --sequential-dependencies     If set set true dependencies will run sequentially
  #     --show-ui                     Shows the ui server
  #     --skip-build                  Skips building of images
  #     --skip-dependency strings     Skips the following dependencies for deployment
  #     --skip-deploy                 If enabled will skip deploying
  #     --skip-push                   Skips image pushing, useful for minikube deployment
  #     --skip-push-local-kube        Skips image pushing, if a local kubernetes environment is detected (default true)
  # -t, --tag strings                 Use the given tag for all built images

# Cleans up resources
export extern "devspace cleanup" []

# Deletes all locally created docker images from docker
export extern "devspace cleanup images" []


#  completion   Outputs shell completion for the given shell (bash or zsh)

# Deploys the project
export extern "devspace deploy" []

# Starts the development mode
export extern "devspace dev" []

 #  enter        Open a shell to a container
 #  init         Initializes DevSpace in the current folder

# Lists configuration
export extern "devspace list" []

export extern "devspace list commands" []
export extern "devspace list contexts" []
export extern "devspace list deployments" []
export extern "devspace list namespaces" []
export extern "devspace list plugins" []
export extern "devspace list ports" []
export extern "devspace list profiles" []
export extern "devspace list sync" []
export extern "devspace list vars" []

export def "ds list commands" []: nothing -> table<name: string, description: string> {
  devspace list commands
    | lines
    | skip 3
    | drop 1
    | each { (split column '|').0 | each { str trim } }
    | rename name description
}

export def "ds list contexts" []: nothing -> table<name: string, active: bool> {
  devspace list contexts
    | lines
    | skip 3
    | drop 1
    | each { (split column '|').0 | each { str trim } }
    | rename name active
    | update cells -c ["active"] {|value| $value == 'true'}
}

export def "ds list deployments" []: nothing -> table<name: string, type: string, deploy: string, status: string> {
  devspace list deployments
    | lines
    | skip 5
    | drop 1
    | each { (split column '|').0 | each { str trim } }
    | rename name type deploy status
}

 #  logs         Prints the logs of a pod and attaches to it
 #  open         Opens the space in the browser
 #  print        Prints displays the configuration

# Deletes deployed resources
export extern "devspace purge" []

 #  remove       Removes devspace configuration
 #  render       Builds all defined images and shows the yamls that would be deployed
 #  reset        Resets an cluster token
 #  restart      Restarts containers where the sync restart helper is injected
 #  run          Executes a predefined command

def "nu-complete devspace run command" [] {
  ds list commands | rename value description
}

# Executes a predefined command from the devspace.yaml
export extern "devspace run" [
  command: string@"nu-complete devspace run command"
]

# Starts a DevSpace pipeline
export extern "devspace run-pipeline" []

 #  set          Sets global configuration changes
 #  sync         Starts a bi-directional sync between the target container and the local path

# Opens the localhost UI in the browser
export extern "devspace ui" []

#  update       Updates the current config
 #  upgrade      Upgrades the DevSpace CLI to the newest version
 #  use          Uses specific config
 #  version      Prints version of devspace

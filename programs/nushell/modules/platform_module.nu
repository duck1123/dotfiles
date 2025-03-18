# Remove duplicates from history
export def "platform history dedupe" [] {
  open ~/.config/nushell/history.txt
    | lines
    | reverse
    | uniq
    | reverse
    | save -f ~/.config/nushell/history.txt
}

# List all argo applications
export def "platform argo app list" []: nothing -> table<metadata: any, spec: any, status: any> {
  argocd app list -o json
    | from json
}

# Read the argocd password from keepass
export def "platform argo password get" [] {
  let kdbxPath = $"($env.HOME)/keepass/passwords.kdbx"
  let passPath = "local/argocd.dev.kronkltd.net"
  keepassxc-cli show $kdbxPath $passPath  -s --all
    | _parse keepassxc data
}

# Refresh the argocd login token
export def "platform argo login" [] {
  let domain = "argocd.dev.kronkltd.net"
  let username = "admin"
  let password = (platform argo password get).Password
  argocd login $domain --username $username --password $password
}

# Completer for argo workflows templates
def "nu-complete platform argo template get" [] {
  platform argo template list | get name
}

# Fetch an argo template by name
export def "platform argo template get" [
  templateName: string@"nu-complete platform argo template get" # The template to read
] {
  argo template get $templateName -o json
    | from json
}

# List all argo templates
export def "platform argo template list" [] {
  argo template list -A
    | lines
    | skip 1
    | each { str trim | split column --regex '\s\s+' | first }
    | rename namespace name
}

# List all workflows
export def "platform argo workflow list" [] {
  argo list -A
    | lines
    | skip 1
    | each { str trim | split column --regex '\s\s+' | first }
    | rename namespace name status age duration priority message
}

# List all k8s clusters
export def "platform cluster list" [] {
  k3d cluster list -o json
    | from json
}

# List git remptes for project
#
# This is likely obsoleted by stanfard git completer
export def "platform git remote" [] {
  # git remote -v
  #   | split row --regex '\n'
  #   | each {|row|
  #     $row
  #       | split column --regex '\t+'
  #       | first
  #   }
  git remote -v
    | detect columns --no-headers
    | rename name url type
}

# Create a minio alias
export def "platform minio alias create" [] {
  let minioAlias = "minio"
  let minioHost = "https://minio-api.dev.kronkltd.net"
  let accessKey = $env.MINIO_ACCESS_KEY
  let secretKey = $env.MINIO_SECRET_KEY
  mc alias set $minioAlias $minioHost $accessKey $secretKey
}

# List all minio aliases
export def "platform minio alias list" [] {
  mc alias list --json
    | from json --objects
}

# Get information about nix profiles
export def "platform nix profile list" [] {
  nix profile list
    | split row --regex  '\n\n'
    | each {
      split row --regex '\n'
        | each {
          split column --regex ':'
            | { ($in.0.column1 | str trim): ($in.0.column2 | str trim) }
        }
        | reduce {|a b| $a | merge $b}
    }
}

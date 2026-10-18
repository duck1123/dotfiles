let
  # Remote MCP servers that authenticate with a bearer token. Each token is the `<name>_token`
  # key of secrets/mcp.yaml, installed by the nixos body and read at connect time by the
  # homeManager body, so it never lands in the nix store or ~/.claude.json.
  remoteServers = {
    trilium = "https://trilium.home.kronkltd.net/mcp";
    windmill = "https://windmill.home.kronkltd.net/api/mcp/w/default/mcp";
  };

  secretName = name: "mcp-${name}-token";
  secretPath = name: "/run/secrets/${secretName name}";

  # Permission rules every host allows. Read-only tools and this repo's own
  # check/build tasks only: nothing that deploys, runs arbitrary code
  # (`nix run`, `sudo`, `kubectl exec`) or can print secret values.
  # `nur claude drift` lists the rules a host has picked up beyond these.
  sharedAllow =
    let
      bash = cmds: map (cmd: "Bash(${cmd} *)") cmds;
      mcp = server: tools: map (tool: "mcp__plugin_hm_${server}__${tool}") tools;
    in
    bash [
      # read-only shell
      "cat"
      "head"
      "tail"
      "wc"
      "ls"
      "tree"
      "file"
      "stat"
      "realpath"
      "which"
      "grep"
      "rg"
      "fd"
      "find"
      "sort"
      "uniq"
      "diff"
      "jq"
      "dig"
      "ps"
      "lsblk"
      "journalctl"
      "systemctl status"
      "systemctl --user status"
      "tailscale status"
      "hyprctl version"
      "hyprctl configerrors"
      # git (local only; pushing still asks)
      "git status"
      "git log"
      "git diff"
      "git show"
      "git branch"
      "git blame"
      "git ls-files"
      "git rev-parse"
      "git fetch"
      "git stash"
      "git mv"
      "git add"
      "git commit"
      "gh pr view"
      "gh pr list"
      "gh issue view"
      "gh issue list"
      # nix
      "nix eval"
      "nix build"
      "nom build"
      "nix flake show"
      "nix flake metadata"
      "nix flake check"
      "nix path-info"
      "nix log"
      "nix search"
      "nix derivation show"
      "nix fmt"
      "nixfmt"
      "statix"
      "tar tzf"
      "jar tf"
      "openssl x509"
      # repo tasks that don't deploy or decrypt
      "nur check"
      "nur lint"
      "nur format"
      "nur build"
      "nur diff-os"
      "nur ui test"
      "nur ui build"
      "nur secrets keys"
      "nur secrets check"
      "nur apps list"
      "nur postgres list"
      "nur vscode drift"
      "nur claude drift"
      # cluster, read-only
      "kubectl get"
      "kubectl describe"
      "kubectl logs"
      "kubectl top"
      "kubectl explain"
      "kubectl api-resources"
      "kubectl version"
      "kubectl auth can-i"
      "kubectl wait"
      "kubectl kustomize"
      "argocd app list"
      "argocd app get"
      "argocd app diff"
      "helm version"
      "helm template"
    ]
    ++ mcp "nixos" [
      "nix"
      "nix_versions"
    ]
    ++ mcp "trilium" [
      "search_notes"
      "get_note"
      "get_note_content"
      "get_subtree"
      "get_child_notes"
      "get_attributes"
      "get_attribute"
      "get_attachment"
      "get_attachment_content"
      "get_help_toc"
      "search_help"
      "search_icons"
      "load_skill"
    ]
    # Not getVariable/getResource: those can return secret values.
    ++ mcp "windmill" [
      "listScripts"
      "listFlows"
      "listJobs"
      "listQueue"
      "listWorkers"
      "listSchedules"
      "listResource"
      "listResourceType"
      "listVariable"
      "getJob"
      "getJobLogs"
      "getScriptByPath"
      "getFlowByPath"
      "getSchedule"
    ]
    ++ [
      "WebSearch"
      "Read(//nix/store/**)"
      "Read(//tmp/**)"
    ]
    ++ map (domain: "WebFetch(domain:${domain})") [
      "github.com"
      "api.github.com"
      "raw.githubusercontent.com"
      "nixos.org"
      "search.nixos.org"
      "wiki.nixos.org"
      "nix.dev"
      "docs.triliumnotes.org"
      "www.windmill.dev"
      "hub.docker.com"
      "artifacthub.io"
    ];
in
{
  description = "Claude Code with declaratively managed MCP servers";

  extraOptions =
    { lib, ... }:
    {
      allow = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          Host-specific Claude Code permission rules, added to the shared ones
          in features/claude.nix.
        '';
      };
    };

  homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      managed = pkgs.writeText "claude-settings.json" (
        builtins.toJSON {
          permissions.allow = lib.unique (sharedAllow ++ config.host.features.claude.allow);
        }
      );
      # Claude Code runs this when connecting and merges the JSON it prints into the request headers.
      headersHelper =
        name:
        pkgs.writeShellScript "mcp-${name}-headers" ''
          exec ${lib.getExe pkgs.jq} -Rc '{Authorization: "Bearer \(.)"}' ${secretPath name}
        '';
    in
    {
      programs.claude-code = {
        enable = true;

        mcpServers = {
          nixos.command = lib.getExe pkgs.mcp-nixos;

          nushell = {
            command = lib.getExe pkgs.nushell;
            args = [ "--mcp" ];
          };
        }
        // lib.mapAttrs (name: url: {
          inherit url;
          type = "http";
          headersHelper = "${headersHelper name}";
        }) remoteServers;
      };

      # No programs.claude-code.settings: that makes settings.json a read-only
      # store symlink, and "always allow" in a session couldn't save. Instead
      # the shared rules are added to the writable file below; rules a host
      # picked up locally are kept (see `nur claude drift`).
      xdg.configFile."dotfiles/claude-settings.json".source = managed;

      home.activation.claudeSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
        target=${lib.escapeShellArg "${config.home.homeDirectory}/.claude/settings.json"}
        run mkdir -p "$(dirname "$target")"
        if [ -L "$target" ]; then run rm "$target"; fi

        tmp=$(mktemp)
        if [ ! -f "$target" ]; then
          cp ${managed} "$tmp"
        elif ! ${lib.getExe pkgs.jq} --indent 2 -s '
          .[0] as $local | .[1] as $nix
          | $local * ($nix | del(.permissions))
          | .permissions.allow = ((($local.permissions.allow // []) + $nix.permissions.allow) | unique)
        ' "$target" ${managed} > "$tmp"; then
          warnEcho "Couldn't merge Claude Code settings into $target, leaving it unchanged"
          rm "$tmp"
          tmp=
        fi
        if [ -n "$tmp" ]; then
          run install -m644 "$tmp" "$target"
          rm "$tmp"
        fi
      '';
    };

  nixos =
    { config, lib, ... }:
    {
      sops.secrets = lib.mapAttrs' (
        name: _:
        lib.nameValuePair (secretName name) {
          sopsFile = ./../secrets/mcp.yaml;
          key = "${name}_token";
          path = secretPath name;
          mode = "0400";
          owner = config.host.identity.username;
          group = "users";
        }
      ) remoteServers;
    };
}

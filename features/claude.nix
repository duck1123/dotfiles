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
in
{
  description = "Claude Code with declaratively managed MCP servers";

  homeManager =
    { lib, pkgs, ... }:
    let
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

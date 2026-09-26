{
  config,
  lib,
  pkgs,
  self,
  ...
}:
with lib;
let
  inherit (pkgs.stdenv.hostPlatform) system;
  # nixkube's own package set, from the sources its umbrella pins (see the
  # `nixkube` flake input), so its store paths match nixkube.cachix.org.
  nixkube = import "${self.inputs.nixkube}/default.nix" { inherit system; };
in
{
  name = "nix-csi";
  namespace = "nix-csi";

  extraOptions = {
    authorizedKeys = mkOption {
      description = mdDoc "SSH public keys authorized to connect to nix-csi cache/builders";
      type = types.listOf types.str;
      default = [ ];
    };

    cache.storageClassName = mkOption {
      description = mdDoc "Storage class for nix-csi cache PVC (null = cluster default)";
      type = types.nullOr types.str;
      default = null;
    };
  };

  extraAppConfig =
    cfg:
    let
      # duck1123.cachix.org is fed by `cachix watch-store` on the
      # workstation, so anything built locally (e.g. attic's own
      # attic-server-bundle) is fetchable even while Attic is down. It
      # has to be asked *before* Attic (priority 41, same as this cache
      # by default): a down Attic answers 503, which nix treats as a hard
      # error rather than moving on to the next substituter.
      atticSettings = {
        substituters = [
          "https://attic.home.kronkltd.net/nixos"
          "https://duck1123.cachix.org?priority=30"
        ];
        trusted-public-keys = [
          "nixos:/5T+7JIEApx8OL/j4HhK1koV6jMPu3rZV098GsuBAi4="
          "duck1123.cachix.org-1:Cj3r3BH7Xuy0zFWy8V/VIB3F7+Gi1m9HB302E9UGV3E="
        ];
      };
      # Lets a workstation push locally-signed store paths directly via
      # `nix copy --to ssh-ng://nix@...` — otherwise nix-daemon rejects unsigned
      # paths with "lacks a signature by a trusted key" regardless of trusted-users,
      # since the stdio-spawned remote daemon doesn't resolve the SSH-authenticated
      # user as a trusted uid the way a local socket connection would.
      # Added to the pynixd controller (the cache) on top of atticSettings.
      cacheSettings = {
        trusted-users = [ "nix" ];
        trusted-public-keys = [
          "powerspecnix-workstation:QPvTlnwGiEZxUT1nOhVy0OvMoI3UmD3byuzHkVVNLGg="
        ];
      };
      # Upstream's kubenixInstance, minus the authorizedKeys it reads from
      # its own keys/ directory: only our keys get into the cache.
      nixkubeEval = nixkube.easykubenix {
        inherit (nixkube) pkgs;
        modules = [
          "${self.inputs.nixkube}/kubenix"
          { _module.args.sources = nixkube.sources; }
          {
            nixkube = {
              enable = true;
              # Kept from the nix-csi days, so the ArgoCD app, its pinned
              # volumes and every reference to the namespace stay put.
              namespace = "nix-csi";
              hostSystem = system;
              # Every node is x86_64; skip building aarch64 environments.
              systems = {
                x86_64-linux = true;
                aarch64-linux = false;
              };

              # Inherited by the node DaemonSet, the pynixd controller
              # and its builders. Lists append to upstream's defaults
              # (nixkube.cachix.org, cache.nixos.org).
              nixConfig.settings = atticSettings;

              pynixd = {
                inherit (cfg) authorizedKeys;
                inherit (cfg.cache) storageClassName;
                controller.nixConfig.settings = cacheSettings;
              };
            };
          }
        ];
      };

      resources = nixkubeEval.config.kubernetes.generated;
      pinIp =
        ip: r:
        r
        // {
          metadata = r.metadata // {
            annotations = (r.metadata.annotations or { }) // {
              "metallb.universe.tf/loadBalancerIPs" = ip;
            };
          };
        };
      # pynixd-lb replaced both nix-cache-lb (.241, port 2222) and
      # nix-proxy (.240); it keeps the cache's address and port.
      patchedResources = map (
        r: if r.kind == "Service" && r.metadata.name == "pynixd-lb" then pinIp "192.168.0.241" r else r
      ) resources;
      yaml = builtins.unsafeDiscardStringContext (
        builtins.concatStringsSep "\n---\n" (map builtins.toJSON patchedResources)
      );
    in
    {
      # Deploy nix-csi before other apps that use the CSI driver (wave 0).
      # ArgoCD waits for wave -1 to be healthy before starting wave 0.
      annotations."argocd.argoproj.io/sync-wave" = "-1";
      yamls = [ yaml ];
    };
}

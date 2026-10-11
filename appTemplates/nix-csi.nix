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
      # nasnix is tainted to keep ordinary workloads off it (see
      # allowControlPlane in mkArgoApp).
      controlPlaneToleration = {
        key = "node-role.kubernetes.io/control-plane";
        operator = "Exists";
      };
      # Added to the pynixd controller (the cache) on top of atticSettings.
      cacheSettings = {
        trusted-users = [ "nix" ];
        trusted-public-keys = [
          "powerspecnix-workstation:QPvTlnwGiEZxUT1nOhVy0OvMoI3UmD3byuzHkVVNLGg="
          # The cluster's own signing key (Secret nix-csi/nix-key, generated
          # once by the init Job). Nodes sign with it before `nix copy`ing to
          # pynixd, which otherwise refuses every path ("lacks a signature by
          # a trusted key") and the node gives up after six attempts. Update
          # this if that Secret is ever regenerated.
          "nix-cache-1:KoqtCQBvf0eHQevY1y319FFEyr8Ef5ZZU9HA3PRjR/A="
        ];
      };
      # Upstream's kubenixInstance, minus the authorizedKeys it reads from
      # its own keys/ directory: only our keys get into the cache.
      nixkubeEval = nixkube.easykubenix {
        inherit (nixkube) pkgs;
        modules = [
          "${self.inputs.nixkube}/kubenix"
          { _module.args.sources = nixkube.sources; }
          # Surfaces the package set the node and pynixd environments come
          # from (a module argument, otherwise unreachable from out here).
          (
            { csiPkgs, ... }:
            {
              options.environmentPackages = mkOption {
                type = types.raw;
                default = csiPkgs.${system};
              };
            }
          )
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

              # The CSI driver has to be on every node, tainted or not.
              node.tolerations = [ controlPlaneToleration ];

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
      # nixkube has no tolerations option for the pynixd StatefulSet.
      tolerateControlPlane =
        r:
        r
        // {
          spec = r.spec // {
            template = r.spec.template // {
              spec = r.spec.template.spec // {
                tolerations = (r.spec.template.spec.tolerations or [ ]) ++ [ controlPlaneToleration ];
              };
            };
          };
        };
      patchedResources = map (
        r:
        if r.kind == "Service" && r.metadata.name == "pynixd-lb" then
          pinIp "192.168.0.241" r
        else if cfg.allowControlPlane && r.kind == "StatefulSet" && r.metadata.name == "pynixd" then
          tolerateControlPlane r
        else
          r
      ) resources;
      # The manifests name the node and pynixd environments by store path
      # only: upstream strips their string context, and nixidy rejects a
      # yaml that has any. Nothing would then build them, no cache would
      # have them, and nodes couldn't start the driver. pathExists realises
      # them during eval instead, so every switch has them locally for
      # k8s-switch-charts.sh to push to Attic.
      environments = with nixkubeEval.config.environmentPackages; [
        nixkube-node-env
        nixkube-pynixd-env
      ];
      yaml =
        assert all (env: builtins.pathExists "${env}") environments;
        builtins.unsafeDiscardStringContext (
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

{ ... }:
{
  flake.nixidyApps.nix-csi =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      nixcsi = import "${self.inputs.nix-csi}/default.nix" {
        system = pkgs.stdenv.hostPlatform.system;
      };
    in
    self.lib.mkArgoApp
      {
        inherit
          config
          lib
          self
          pkgs
          ;
      }
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
            atticSettings = {
              substituters = [ "https://attic.home.kronkltd.net/nixos" ];
              trusted-public-keys = [ "nixos:6s8iAyKEnH2z4spigUdDmt1VwiAwrvPA9vQNUd9if1k=" ];
            };
            # Lets a workstation push locally-signed store paths directly via
            # `nix copy --to ssh-ng://nix@...` — otherwise nix-daemon rejects unsigned
            # paths with "lacks a signature by a trusted key" regardless of trusted-users,
            # since the stdio-spawned remote daemon doesn't resolve the SSH-authenticated
            # user as a trusted uid the way a local socket connection would.
            cacheSettings = atticSettings // {
              trusted-users = [ "nix" ];
              trusted-public-keys = atticSettings.trusted-public-keys ++ [
                "powerspecnix-workstation:QPvTlnwGiEZxUT1nOhVy0OvMoI3UmD3byuzHkVVNLGg="
              ];
            };
            nixcsiEval = nixcsi.kubenixInstance {
              module.imports = [
                # Override curPkgs to avoid builtins.currentSystem (unavailable in pure eval)
                { _module.args.curPkgs = mkForce nixcsi.pkgs; }
                {
                  nix-csi = {
                    authorizedKeys = cfg.authorizedKeys;
                    cache.storageClassName = cfg.cache.storageClassName;
                    # Bootstrap components (cache-init-env, proxy-env, etc.) aren't
                    # published to nix-csi.cachix.org for every commit — our own
                    # cache closes that gap so cold-starts don't depend on upstream CI.
                    cache.nixConfig.settings = cacheSettings;
                    node.nixConfig.settings = atticSettings;
                    builders.nixConfig.settings = atticSettings;
                    # Gives cache/node/proxy a local-build fallback (via SSH-ng,
                    # discovered dynamically by builders.py) for bootstrap *-init-env
                    # paths that are missing from Attic, instead of hard-failing.
                    builders.deployments.amd64.enable = true;
                  };
                }
              ];
            };

            resources = nixcsiEval.eval.config.kubernetes.generated;
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
            # builders.py (nix-csi's own code) discovers builder pods via the k8s API
            # and addresses them as `ssh-ng://nix@<pod-name>.nix-csi-builders.<ns>.svc...`
            # -- a DNS name that's only ever populated by the StatefulSet controller
            # (which sets each pod's `hostname`/`subdomain` to match its governing
            # headless Service). The `builders.deployments.amd64` option renders a
            # plain Deployment instead, whose pods get random name suffixes and no
            # such DNS record, so that address never resolves and every build that
            # needs an actual (non-substitutable) derivation fails outright. Promoting
            # it to a StatefulSet (same pattern already used for the `nix-cache` role,
            # see StatefulSet-nix-cache.yaml) gives it a stable `amd64-0` pod name that
            # matches what builders.py expects, with no other spec changes needed.
            toBuilderStatefulSet =
              r:
              r
              // {
                kind = "StatefulSet";
                spec = r.spec // {
                  serviceName = "nix-csi-builders";
                };
              };
            patchedResources = map (
              r:
              if r.kind == "Service" && r.metadata.name == "nix-cache-lb" then
                pinIp "192.168.0.241" r
              else if r.kind == "Service" && r.metadata.name == "nix-proxy" then
                pinIp "192.168.0.240" r
              else if r.kind == "Deployment" && r.metadata.name == "amd64" then
                toBuilderStatefulSet r
              else
                r
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
      };
}

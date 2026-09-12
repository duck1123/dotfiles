{ ... }:
{
  # mkArgoApp
  #
  # Takes the current config and lib and returns a function that creates a nixidy application with defaults
  flake.lib.mkArgoApp =
    {
      config,
      lib,
      self ? null,
      # The default storageClassName for this application
      storageClassName ? "longhorn",
      # The default timezone for this application
      tz ? "Etc/UTC",
      pkgs ? null,
      ...
    }:
    {
      # The name of the application (string)
      name,
      namespace ? null,
      # The chart to deploy (path)
      chart ? null,
      # A function that takes the config and returns default helm chart values
      defaultValues ? (cfg: { }),
      # A list of secrets that need to be loaded when generating this application
      neededSecrets ? [ ],
      # A function that takes the config and returns extra config merged into the app release.
      extraAppConfig ? (cfg: { }),
      # A function that takes the config and returns extra config merged into final config
      extraConfig ? (cfg: { }),
      # Additional config options
      extraOptions ? { },
      # A function that takes the config and returns extra resources to deploy with the application
      extraResources ? (cfg: { }),
      # A function that takes the config and returns an attrset of volume specs (size, and
      # optionally pvcName/pvName/accessModes/volumeAttributes/storageClassName overrides), keyed
      # by a short logical volume name (e.g. "appdata", "config"). This declares each volume's
      # *shape* only -- environment-agnostic, belongs in applications/<name>.nix. Any field here
      # (including a `volumeHandle` to pin it to a specific pre-existing Longhorn volume) can be
      # overridden per-environment via `cfg.volumeOverrides.<key>` (a plain option, set in e.g.
      # env/dev/<name>.nix, since a volumeHandle only means something on one specific cluster) --
      # a key with no `volumeHandle` anywhere just means an ordinary dynamically-provisioned PVC.
      # See docs/pinned-volumes.md.
      volumes ? (cfg: { }),
      # A function that takes the config and returns secrets to encrypt (name -> stringData attrs)
      sopsSecrets ? (cfg: { }),
      # Does this chart expose an ingress
      uses-ingress ? false,
      # Does this app have a centralized NFS mount (services.<name>.nfs.*)
      uses-nfs ? false,
      # Does this app have a centralized external database (services.<name>.database.*)
      uses-database ? false,
    }:
    with lib;
    let
      inherit (types)
        attrs
        listOf
        nullOr
        path
        str
        submodule
        unspecified
        ;
      cfg = config.services.${name};
      values = attrsets.recursiveUpdate (defaultValues cfg) cfg.values;

      # See docs/pinned-volumes.md. `volumes cfg` (environment-agnostic, from
      # applications/<name>.nix) declares each volume's shape; `cfg.volumeOverrides.<key>`
      # (set per-environment, e.g. env/dev/<name>.nix) is recursively merged on top of that --
      # not just for `volumeHandle`, any field (`size` most commonly, but also `accessModes`,
      # `storageClassName`, `volumeAttributes`, even `pvcName`) can be overridden per environment
      # without touching applications/<name>.nix. A key with no `volumeHandle` anywhere (the
      # default, and what a brand-new environment gets automatically) becomes a plain
      # dynamically-provisioned PVC; one with a `volumeHandle` becomes a pinned PV+PVC via
      # `mkPinnedVolume`. `cfg.volumes` is what extraResources/extraAppConfig/etc reference
      # instead of hand-typing the PVC name -- `cfg.volumes.<key>.volume` is a ready
      # `{ name; persistentVolumeClaim.claimName; }` for a Deployment's
      # `spec.template.spec.volumes`, regardless of whether that key ends up pinned.
      resolvedVolumes = lib.mapAttrs (
        volName: shape: lib.recursiveUpdate shape (cfg.volumeOverrides.${volName} or { })
      ) (volumes cfg);
      # Default naming follows this repo's existing convention; pass an explicit
      # `pvcName` in a volume's arg-set (or a `volumeOverrides.<key>.pvcName` override) to
      # override it -- e.g. to match a literal name a Helm chart's `existingClaim`-style
      # value already expects.
      volumePvcName = volName: args: args.pvcName or "${name}-${name}-${volName}";
      volumeFragments = lib.mapAttrs (
        volName: args:
        let
          pvcName = volumePvcName volName args;
          handle = args.volumeHandle or null;
        in
        if handle == null then
          {
            persistentVolumeClaims.${pvcName}.spec = {
              accessModes = args.accessModes or [ "ReadWriteOnce" ];
              resources.requests.storage = args.size;
              storageClassName = args.storageClassName or cfg.storageClassName;
            };
          }
        else
          self.lib.mkPinnedVolume (
            (builtins.removeAttrs args [ "pvcName" ])
            // {
              inherit pvcName;
              volumeHandle = handle;
            }
          )
      ) resolvedVolumes;
      volumeResources = lib.foldl' lib.recursiveUpdate { } (lib.attrValues volumeFragments);
      volumeInfo = lib.mapAttrs (volName: args: {
        pvcName = volumePvcName volName args;
        volume = {
          name = volName;
          persistentVolumeClaim.claimName = volumePvcName volName args;
        };
      }) resolvedVolumes;

      # Combined secrets from sopsSecrets parameter and cfg.sopsSecrets option (option overrides)
      combinedSopsSecrets = (sopsSecrets cfg) // cfg.sopsSecrets;

      # Secret specs (with plaintext values) for write-sops-secrets.sh to encrypt outside Nix.
      # Never passed to a derivation — accessed only via `nix eval` so values never enter the store.
      secretSpecsList =
        if combinedSopsSecrets == { } then
          [ ]
        else
          lib.mapAttrsToList (secretName: data: {
            inherit secretName;
            app = name;
            namespace = cfg.namespace;
            values = if data ? values then data.values else data;
          }) combinedSopsSecrets;

      # Inject hostAffinity nodeSelector into all deployment and statefulSet pod specs
      addHostAffinityToResources =
        resources: hostAffinity:
        if hostAffinity == null then
          resources
        else
          let
            nodeSelectorFragment = {
              "kubernetes.io/hostname" = hostAffinity;
            };
            addToPodSpec =
              spec:
              spec
              // {
                nodeSelector = (spec.nodeSelector or { }) // nodeSelectorFragment;
              };
            addToWorkload =
              workload:
              workload
              // {
                spec = (workload.spec or { }) // {
                  template = (workload.spec.template or { }) // {
                    spec = addToPodSpec (workload.spec.template.spec or { });
                  };
                };
              };
          in
          resources
          // {
            deployments = lib.mapAttrs (_: addToWorkload) (resources.deployments or { });
            statefulSets = lib.mapAttrs (_: addToWorkload) (resources.statefulSets or { });
          };
      tls-options = {
        enable = mkEnableOption "Enable application";

        secretName = mkOption {
          description = mdDoc "The domain to expost ${name} to";
          type = str;
          default = "${name}-tls";
        };
      };
      ingress-options = {
        clusterIssuer = mkOption {
          description = mdDoc "The cluster issuer to use for ${name}'s tls";
          type = str;
          default = "letsEncrypt-prod";
        };

        domain = mkOption {
          description = mdDoc "The domain to expost ${name} to";
          type = str;
          default = "${name}.local";
        };

        ingressClassName = mkOption {
          description = mdDoc ''
            Ingress class: `traefik` for the in-cluster Traefik controller (often exposed with `Service` type LoadBalancer and MetalLB), or `tailscale` for the Tailscale Kubernetes operator.
          '';
          type = str;
          default = "traefik";
        };

        tls = tls-options;
      };
      nfs-options = {
        enable = mkEnableOption "an NFS mount for ${name}";

        server = mkOption {
          description = mdDoc "NFS server hostname/IP.";
          type = str;
          default = "";
        };

        path = mkOption {
          description = mdDoc "NFS server path.";
          type = str;
          default = "";
        };
      };
      database-options = {
        enable = mkEnableOption "an external database for ${name}";

        host = mkOption {
          description = mdDoc "Database host.";
          type = str;
          default = "";
        };

        port = mkOption {
          description = mdDoc "Database port.";
          type = types.port;
          default = 0;
        };

        name = mkOption {
          description = mdDoc "Database name.";
          type = str;
          default = name;
        };

        username = mkOption {
          description = mdDoc "Database username.";
          type = str;
          default = name;
        };

        password = mkOption {
          description = mdDoc "Database password.";
          type = str;
          default = "";
        };
      };
      basic-options = {
        chart = mkOption {
          type = nullOr path;
          default = chart;
          description = "Helm chart to use for ${name}.";
        };

        enable = mkEnableOption "Enable ${name} app";

        extraAppConfig = mkOption {
          default = { };
          description = "Extra config merged into the app release.";
          type = attrs;
        };

        extraResources = mkOption {
          default = { };
          description = "Extra Kubernetes resources related to ${name}.";
          type = attrs;
        };

        hostAffinity = mkOption {
          description = mdDoc "The host to assign the node to";
          type = nullOr types.str;
          default = null;
        };

        ingressProvider = mkOption {
          description = mdDoc "Name of a config.ingressProviders entry supplying default ingress.{ingressClassName,clusterIssuer,domain} for ${name}.";
          type = nullOr str;
          default = null;
        };

        nfsTarget = mkOption {
          description = mdDoc "Name of a config.nfsTargets entry supplying default nfs.{server,path} for ${name}.";
          type = nullOr str;
          default = "nas";
        };

        nfsSubPath = mkOption {
          description = mdDoc "Path appended to the nfsTarget's basePath to form nfs.path.";
          type = types.str;
          default = "";
        };

        databaseTarget = mkOption {
          description = mdDoc "Name of a config.databaseProviders entry supplying default database.{host,port,username,password} for ${name}.";
          type = nullOr str;
          default = null;
        };

        libvaDriverName = mkOption {
          description = mdDoc "LIBVA_DRIVER_NAME for VAAPI (e.g. iris for Intel, radeonsi for AMD). Auto-derived from hostAffinity via nodeGpuProfiles. Empty string = do not set.";
          type = types.str;
          default = "";
        };

        vaapiRenderDevice = mkOption {
          description = mdDoc "Host DRI render device (e.g. renderD129) to mount as /dev/dri/renderD128. Auto-derived from hostAffinity. Empty = mount entire /dev/dri.";
          type = types.str;
          default = "";
        };

        renderGroupGID = mkOption {
          description = mdDoc "GID of the host render group for /dev/dri access. Auto-derived from hostAffinity (default 303 on NixOS).";
          type = types.int;
          default = 303;
        };

        ingress = mkOption {
          apply =
            val:
            assert uses-ingress || val == { };
            val;
          default = { };
          description = "Ingress Options";
          type =
            if uses-ingress then
              submodule (
                let
                  extra-ingress = extraOptions.ingress or { };
                  options = recursiveUpdate ingress-options extra-ingress;
                in
                {
                  inherit options;
                }
              )
            else
              unspecified;
        };

        namespace = mkOption {
          description = mdDoc "The namespace to install ${name} into";
          type = str;
          default = if namespace != null then namespace else name;
        };

        # Read by the `autokuma` app, which collects one entry per service where
        # this is enabled into a static AutoKuma monitor file — see
        # applications/autokuma.nix. Defaults to on for any app with an ingress
        # (opt out per-app with `monitoring.autokuma.enable = false;`); apps
        # without an ingress still default off since there's no URL to probe.
        monitoring.autokuma = {
          enable = mkOption {
            description = mdDoc "Enable an AutoKuma-managed Uptime Kuma monitor for ${name}. Defaults to true when ${name} has an ingress.";
            type = types.bool;
            default = uses-ingress;
          };

          type = mkOption {
            description = mdDoc "AutoKuma monitor type (see upstream ENTITY_TYPES.md: http, tcp, ping, port, ...).";
            type = str;
            default = "http";
          };

          url = mkOption {
            description = mdDoc "URL used for http-type monitors. Defaults to this app's ingress domain when it has one.";
            type = nullOr str;
            default = if uses-ingress then "https://${cfg.ingress.domain}" else null;
          };

          displayName = mkOption {
            description = mdDoc "Monitor name shown in Uptime Kuma.";
            type = str;
            default = name;
          };

          extraSettings = mkOption {
            description = mdDoc "Extra fields merged into the generated AutoKuma monitor definition (e.g. port, hostname, retries, tag_names).";
            type = attrs;
            default = { };
          };
        };

        # Read by the `homepage` app, which collects one entry per service where
        # this is enabled into a services.yaml dashboard group — see
        # applications/homepage.nix. Defaults to on for any app with an ingress
        # (opt out per-app with `homepage.enable = false;`); apps without an
        # ingress default off since there's no href to link to.
        homepage = {
          enable = mkOption {
            description = mdDoc "Show ${name} as a link on the homepage dashboard. Defaults to true when ${name} has an ingress.";
            type = types.bool;
            default = uses-ingress;
          };

          group = mkOption {
            description = mdDoc "Dashboard group/category ${name} is listed under. Must be one of `config.homepageGroups` (see modules/homepageGroups.nix) -- add new groups there first.";
            type = types.enum config.homepageGroups;
            default = "Apps";
          };

          displayName = mkOption {
            description = mdDoc "Name shown on the dashboard tile.";
            type = str;
            default = name;
          };

          icon = mkOption {
            description = mdDoc "Dashboard icon (dashboard-icons name, Simple Icons slug, or full URL). Empty = no icon.";
            type = str;
            default = "";
          };

          description = mkOption {
            description = mdDoc "Text shown under the tile name on the dashboard.";
            type = str;
            default = "";
          };

          href = mkOption {
            description = mdDoc "Link target. Defaults to this app's ingress URL when it has one.";
            type = nullOr str;
            default = if uses-ingress then "https://${cfg.ingress.domain}" else null;
          };

          extraSettings = mkOption {
            description = mdDoc "Extra fields merged into the generated dashboard item (e.g. widget, siteMonitor, ping).";
            type = attrs;
            default = { };
          };
        };

        neededSecrets = mkOption {
          type = listOf str;
          default = neededSecrets;
          description = "List of secrets needed by ${name}.";
        };

        # Attrset of secrets to encrypt. Key = secret name, value = stringData attrs (or { values = attrs }).
        # Encryption happens outside Nix via scripts/write-sops-secrets.sh so plaintext never enters the store.
        sopsSecrets = mkOption {
          default = { };
          description = mdDoc "Secrets to encrypt. Key = secret name, value = stringData attrs (or { values = attrs }).";
          type = types.attrsOf types.anything;
        };

        # Internal: populated by mkArgoApp for CI manifest (list of { app, secretName, namespace, keys }).
        sopsSecretsManifest = mkOption {
          default = [ ];
          type = types.listOf types.attrs;
          internal = true;
          description = "Secret manifest entries for this app (for CI).";
        };

        # Internal: full secret specs with plaintext values for write-sops-secrets.sh.
        # Exposed via `nix eval` only — never built as a derivation, never in the Nix store.
        sopsSecretsSpec = mkOption {
          default = [ ];
          type = types.listOf types.attrs;
          internal = true;
          description = "Full secret specs (secretName, namespace, values) for external sops encryption.";
        };

        storageClassName = mkOption {
          description = mdDoc "The storage class";
          type = types.str;
          default = storageClassName;
        };

        # Set per-environment (e.g. env/dev/<name>.nix) -- recursively merged on top of the
        # matching key's shape from the `volumes` parameter passed to mkArgoApp. Any field can
        # be overridden this way (`size` most commonly, but also `accessModes`,
        # `storageClassName`, `volumeAttributes`, `pvcName`) -- not just `volumeHandle`, though
        # that's the one that actually pins a volume: a key with no `volumeHandle` anywhere is
        # an ordinary dynamically-provisioned PVC (what a brand-new environment gets
        # automatically, since a Longhorn volumeHandle only identifies a volume on one specific
        # cluster and so doesn't belong in applications/<name>.nix). See docs/pinned-volumes.md.
        volumeOverrides = mkOption {
          default = { };
          type = types.attrsOf types.attrs;
          description = mdDoc "Per-environment overrides merged onto the matching key's shape from the `volumes` parameter passed to mkArgoApp. Setting `volumeHandle` here pins that volume.";
        };

        # Internal: computed from the outer `volumes` parameter -- see
        # docs/pinned-volumes.md. Reference `cfg.volumes.<key>.volume` in a
        # Deployment's `volumes` list instead of hand-typing the PVC name; the
        # matching PersistentVolumeClaim/PersistentVolume resources are already
        # merged into this app automatically, pinned or not.
        volumes = mkOption {
          internal = true;
          default = volumeInfo;
          type = types.attrsOf types.attrs;
          description = mdDoc "Computed volume info (`.pvcName`, `.volume`), one entry per key in the `volumes` parameter passed to mkArgoApp.";
        };

        tz = mkOption {
          description = mdDoc "The timezone";
          type = types.str;
          default = tz;
        };

        values = mkOption {
          description = "All the values";
          type = types.attrsOf types.anything;
          default = { };
        };
      }
      // lib.optionalAttrs uses-nfs {
        nfs = mkOption {
          default = { };
          description = "NFS Options";
          type = submodule (
            let
              extra-nfs = extraOptions.nfs or { };
              options = recursiveUpdate nfs-options extra-nfs;
            in
            {
              inherit options;
            }
          );
        };
      }
      // lib.optionalAttrs uses-database {
        database = mkOption {
          default = { };
          description = "Database Options";
          type = submodule (
            let
              extra-database = extraOptions.database or { };
              options = recursiveUpdate database-options extra-database;
            in
            {
              inherit options;
            }
          );
        };
      };
    in
    {
      options.services.${name} = lib.foldl' lib.recursiveUpdate { } [
        basic-options
        extraOptions
      ];

      config = mkIf cfg.enable (
        mkMerge (
          [
            (mkIf (cfg.hostAffinity != null) (
              let
                profile = config.nodeGpuProfiles.${cfg.hostAffinity} or { };
              in
              {
                services.${name} = {
                  libvaDriverName = mkDefault (profile.libvaDriverName or "");
                  vaapiRenderDevice = mkDefault (profile.vaapiRenderDevice or "");
                  renderGroupGID = mkDefault (profile.renderGroupGID or 303);
                };
              }
            ))
          ]
          ++ lib.optional uses-ingress (
            mkIf (cfg.ingressProvider != null) (
              let
                profile = config.ingressProviders.${cfg.ingressProvider} or { };
              in
              {
                services.${name}.ingress = {
                  ingressClassName = mkDefault (profile.ingressClassName or "traefik");
                  clusterIssuer = mkDefault (profile.clusterIssuer or "letsEncrypt-prod");
                  domain = mkDefault "${name}.${profile.domain or "local"}";
                };
              }
            )
          )
          ++ lib.optional uses-nfs (
            mkIf (cfg.nfsTarget != null) (
              let
                t = config.nfsTargets.${cfg.nfsTarget} or { };
              in
              {
                services.${name}.nfs = {
                  server = mkDefault (t.server or "");
                  path = mkDefault (
                    if cfg.nfsSubPath != "" then "${t.basePath or ""}/${cfg.nfsSubPath}" else t.basePath or ""
                  );
                };
              }
            )
          )
          ++ lib.optional uses-database (
            mkIf (cfg.databaseTarget != null) (
              let
                t = config.databaseProviders.${cfg.databaseTarget} or { };
              in
              {
                services.${name}.database = {
                  host = mkDefault (t.host or "");
                  port = mkDefault (t.port or 0);
                  username = mkDefault ((t.usernameFor or (n: n)) name);
                  password = mkDefault ((t.passwordFor or (_: "")) name);
                };
              }
            )
          )
          ++ [
            (mkIf (combinedSopsSecrets != { }) {
              services.${name} = {
                sopsSecretsManifest = lib.mapAttrsToList (sn: data: {
                  app = name;
                  secretName = sn;
                  namespace = cfg.namespace;
                  keys = lib.attrNames (if data ? values then data.values else data);
                }) combinedSopsSecrets;
                sopsSecretsSpec = secretSpecsList;
              };
            })
            {
              # This is the application config for nixidy
              applications.${name} =
                let
                  # sopsSecrets are intentionally excluded: encryption happens outside Nix via
                  # scripts/write-sops-secrets.sh so plaintext values never enter the Nix store.
                  baseResources = lib.foldl' lib.recursiveUpdate { } [
                    volumeResources
                    (extraResources cfg)
                    cfg.extraResources
                  ];
                  resources = addHostAffinityToResources (builtins.removeAttrs baseResources [
                    "sopsSecrets"
                  ]) cfg.hostAffinity;
                in
                mkMerge [
                  {
                    inherit (cfg) namespace;
                    createNamespace = true;
                    finalizer = "foreground";

                    # TODO: Should I be using some sort of overlay here?
                    inherit resources;
                    syncPolicy.finalSyncOpts = [ "CreateNamespace=true" ];
                  }
                  (mkIf (cfg.chart != null) {
                    helm.releases.${name} = {
                      inherit values;
                      inherit (cfg) chart;
                    };
                  })
                  (extraAppConfig cfg)
                  cfg.extraAppConfig
                ];
            }
            (extraConfig cfg)
          ]
        )
      );
    };

}

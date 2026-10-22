{
  lib,
  pkgs,
  self,
  config,
  ...
}:
let
  secrets = self.lib.loadSecrets { inherit pkgs; };
in
{
  # ./options.nix declares the shared devDefaults.* options; ./apps holds one
  # file per service, imported recursively. ./state.nix sets every app's
  # enable, replicas and pinned Longhorn volumes from state.json.
  imports = [
    ./options.nix
    ./state.nix
    (self.inputs.import-tree ./apps)
  ];

  _module.args = {
    inherit secrets;
  };

  # FIXME: naughty config
  ageRecipients = "age1n372e8dgautnjhecllf7uvvldw9g6vyx3kggj0kyduz5jr2upvysue242c";

  nodeGpuProfiles = {
    edgenix = {
      libvaDriverName = "radeonsi";
      # WX 3200 (VAAPI card) is the second GPU on this node, enumerated as renderD129
      vaapiRenderDevice = "renderD129";
    };
    nixmini.libvaDriverName = "iris";
    powerspecnix.libvaDriverName = "radeonsi";
  };

  # Display order for homepage dashboard groups -- see modules/homepageGroups.nix.
  # New groups go on the left (nearer the front of this list) until per-group
  # column placement exists.
  homepageGroups = [
    "Nodes"
    "Media"
    "Organization"
    "Analytics"
    "Automation"
    "Finance"
    "Notes"
    "Arr"
    "Apps"
    "Download"
    "Nostr"
    "Storage"
    "Database"
  ];

  ingressProviders = {
    traefik-lan = {
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      domain = config.devDefaults.homeDomain;
    };
    tailscale = {
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
      domain = config.devDefaults.tailDomain;
    };
    traefik-dev = {
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      domain = config.devDefaults.baseDomain;
    };
  };

  nfsTargets.nas = {
    server = config.devDefaults.nasHost;
    basePath = config.devDefaults.nasBase;
  };

  databaseProviders = {
    postgresql = {
      kind = "postgresql";
      app = "postgresql";
      superuser = "postgres";
      host = "postgresql.postgresql";
      port = 5432;
      usernameFor = appName: appName;
      passwordFor = _appName: secrets.postgresql.userPassword;
    };
    mariadb = {
      kind = "mariadb";
      app = "mariadb";
      superuser = "root";
      host = "mariadb.mariadb";
      port = 3306;
      usernameFor = appName: appName;
      passwordFor = appName: secrets.${appName}.database.password;
    };
  };

  redisProviders.redis = {
    host = "redis.redis";
    port = 6379;
    passwordFor = _appName: secrets.redis.password;
  };

  nixidy = {
    defaults.syncPolicy.autoSync = {
      enable = true;
      prune = true;
      selfHeal = true;
    };

    target = {
      branch = "master";
      repository = "git@github.com:duck1123/argo-manifests.git";
      # config.nixidy.env, not a literal "dev" -- mkEnvs sets it to this
      # environment's key (envs.dev.modules -> "dev") via mkDefault, so this
      # stays correct if the environment is ever renamed instead of silently
      # pointing at a stale subdirectory name.
      #
      # Deliberately a subdirectory, not "./" -- nixidy's activate step rsyncs
      # rootPath with --delete and no .git exclusion (see modules/build/
      # emit-environment.nix upstream), so rootPath = "./" against a git
      # checkout root deletes .git itself. Confirmed the hard way against the
      # real checkout (recovered via a clean re-clone -- nothing was pushed).
      # Activation must run from the kubernetes/manifests/ checkout directory.
      rootPath = config.nixidy.env;
    };
  };
}

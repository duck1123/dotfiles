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
  # Recursively imports every module under ./dev (one file per service, plus
  # ./dev/options.nix for the shared devDefaults.* options).
  imports = [ (self.inputs.import-tree ./dev) ];

  _module.args = {
    inherit secrets;

    # Generates main + log databases for a list of *arr app configs
    arrDatabases =
      apps:
      builtins.concatLists (
        map (app: [
          {
            name = if app.name == "prowlarr" then "${app.name}-main" else app.name;
            username = app.name;
            password = secrets.postgresql.userPassword;
          }
          {
            name = if app.name == "prowlarr" then "${app.name}-log" else "${app.name}-log";
            username = app.name;
            password = secrets.postgresql.userPassword;
          }
        ]) apps
      );
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
    "Storage"
    "Database"
    "Download"
    "Apps"
    "Arr"
    "Media"
    "Notes"
    "Finance"
    "Automation"
    "Nostr"
    "Nodes"
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
      host = "postgresql.postgresql";
      port = 5432;
      usernameFor = appName: appName;
      passwordFor = _appName: secrets.postgresql.userPassword;
    };
    mariadb = {
      host = "mariadb.mariadb";
      port = 3306;
      usernameFor = appName: appName;
      passwordFor = appName: secrets.${appName}.database.password;
    };
  };

  nixidy = {
    defaults.syncPolicy.autoSync = {
      enable = true;
      prune = true;
      selfHeal = true;
    };

    target = {
      branch = "master";
      repository = "https://github.com/duck1123/k3s-fleetops.git";
      rootPath = "./manifests/dev";
    };
  };
}

{
  pkgs,
  self,
  ...
}:
{
  # Recursively imports every module under ./dev (one file per service, plus
  # ./dev/options.nix for the shared devDefaults.* options).
  imports = [ (self.inputs.import-tree ./dev) ];

  _module.args =
    let
      secrets = self.lib.loadSecrets { inherit pkgs; };
    in
    {
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

  nixidy = {
    defaults.syncPolicy.autoSync = {
      enable = true;
      prune = true;
      selfHeal = true;
    };

    target = {
      branch = "master";
      repository = "git@github.com:duck1123/argo-manifests.git";
      # Manifests are written to <manifests-repo-checkout>/dev/
      # Activation must run from the kubernetes/manifests/ checkout directory.
      rootPath = "dev";
    };
  };
}

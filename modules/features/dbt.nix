{ ... }:
{
  flake.types.generic.feature-options.dbt =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "dbt feature";

  flake.modules.homeManager.dbt =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config =
        let
          # dbt-core (built for Python 3.12; see below) transitively depends on packages
          # whose pinned upper bounds lag behind what nixpkgs currently ships:
          # dbt-semantic-interfaces pins more-itertools<11.0 (nixpkgs has 11.1.0), and
          # dbt-common pins pathspec<1.1 (nixpkgs has 1.1.1). Relax both.
          python312-dbt =
            pkgs.python312.override {
              packageOverrides = self: super: {
                dbt-semantic-interfaces = super.dbt-semantic-interfaces.overridePythonAttrs (old: {
                  pythonRelaxDeps = (old.pythonRelaxDeps or [ ]) ++ [ "more-itertools" ];
                });
                dbt-common = super.dbt-common.overridePythonAttrs (old: {
                  pythonRelaxDeps = (old.pythonRelaxDeps or [ ]) ++ [ "pathspec" ];
                });
              };
            };
        in
        lib.mkIf config.host.features.dbt.enable {
          home = {
            file.".dbt/profiles.yml".source = (pkgs.formats.yaml { }).generate "profiles.yml" {
              default = {
                target = "dev";
                outputs = {
                  dev = {
                    type = "postgres";
                    host = "localhost";
                    user = "postgres";
                    password = "hunter2";
                    port = 5432;
                    dbname = "st";
                    schema = "schema_identifier";
                    threads = 1;
                  };

                  prod = {
                    type = "postgres";
                    host = "localhost";
                    user = "postgres";
                    password = "hunter2";
                    port = 5432;
                    dbname = "prod_st";
                    schema = "schema_identifier";
                    threads = 1;
                  };
                };
              };
            };

            packages = with pkgs; [
              (azure-cli.withExtensions [
                azure-cli-extensions.azure-devops
                azure-cli-extensions.powerbidedicated
              ])
              databricks-cli
              databricks-sql-cli
              # dbt-semantic-interfaces (a dbt-core dependency) disables itself on
              # Python 3.14, nixpkgs' current default python3; build against 3.12 instead.
              (python312-dbt.pkgs.toPythonApplication python312-dbt.pkgs.dbt-core)
            ];
          };

          xdg.configFile."fish/completions/databricks.fish".source =
            pkgs.runCommand "databricks-fish-completions" { }
              ''
                ${pkgs.databricks-cli}/bin/databricks completion fish > $out
              '';
        };
    };
}

{ pkgs, ... }: {
  home.file.".dbt/profiles.yml".text =
    inputs.k3s-fleetops.lib.x86_64-linux.toYAML {
      inherit pkgs;
      value = {
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
    };
}

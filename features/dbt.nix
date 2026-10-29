{
  homeManager =
    { pkgs, ... }:
    let
      # azure-cli depends on python3-fabric for its (unused here) servicefabric
      # support. fabric pins invoke<3.0 but nixpkgs now ships invoke 3.0.3, which
      # breaks fabric's own test suite against the new invoke API (not just an
      # eval-time version check) — see https://github.com/fabric/fabric/issues/2376.
      # Relax the pin and skip fabric's tests rather than pull in an older invoke.
      # azure-cli's own python-packages.nix calls `python3.override { packageOverrides
      # = ...; }` itself, which replaces (rather than composes with) any
      # packageOverrides set via `pkgs.python3.override`, so a fabric fix applied
      # that way never survives; pythonPackagesExtensions, folded in by nixpkgs'
      # python package set builder regardless of later `.override` calls, does.
      pkgs' = pkgs.extend (
        _final: prev: {
          pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
            (pyFinal: pySuper: {
              fabric = pySuper.fabric.overridePythonAttrs (old: {
                pythonRelaxDeps = (old.pythonRelaxDeps or [ ]) ++ [ "invoke" ];
                doCheck = false;
                meta = old.meta // {
                  broken = false;
                };
              });
            })
          ];
        }
      );

      # dbt-core (built for Python 3.12; see below) transitively depends on packages
      # whose pinned upper bounds lag behind what nixpkgs currently ships:
      # dbt-semantic-interfaces pins more-itertools<11.0 (nixpkgs has 11.1.0) and
      # click<8.4 (nixpkgs has 8.4.2), and dbt-common pins pathspec<1.1 (nixpkgs has
      # 1.1.1). Relax all three.
      python312-dbt = pkgs.python312.override {
        packageOverrides = self: super: {
          dbt-semantic-interfaces = super.dbt-semantic-interfaces.overridePythonAttrs (old: {
            pythonRelaxDeps = (old.pythonRelaxDeps or [ ]) ++ [
              "more-itertools"
              "click"
            ];
          });
          dbt-common = super.dbt-common.overridePythonAttrs (old: {
            pythonRelaxDeps = (old.pythonRelaxDeps or [ ]) ++ [ "pathspec" ];
          });
        };
      };

      # dbt-semantic-interfaces (a dbt-core dependency) disables itself on
      # Python 3.14, nixpkgs' current default python3; build against 3.12 instead.
      dbt-core = python312-dbt.pkgs.toPythonApplication python312-dbt.pkgs.dbt-core;
    in
    {
      home.packages = with pkgs; [
        (pkgs'.azure-cli.withExtensions [
          pkgs'.azure-cli-extensions.azure-devops
          pkgs'.azure-cli-extensions.powerbidedicated
        ])
        databricks-cli
        databricks-sql-cli
        # dbt-core
      ];

      xdg.configFile."fish/completions/databricks.fish".source =
        pkgs.runCommand "databricks-fish-completions" { }
          ''
            ${pkgs.databricks-cli}/bin/databricks completion fish > $out
          '';
    };
}

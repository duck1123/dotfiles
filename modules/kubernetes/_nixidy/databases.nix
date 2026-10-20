{ config, lib, ... }:
let
  inherit (lib) mkOption types;
in
{
  options = {
    databases = mkOption {
      description = ''
        Every database the cluster knows about, keyed by "<provider>/<name>"
        (or any unique key for a database outside a provider).

        Apps built with uses-database register their own databases here
        whenever databaseTarget is set (see the `databases` argument of
        mkArgoApp). The provider's app (e.g. services.postgresql) creates the
        ones with provision = true. Reporting tools (metabase, superset,
        windmill) read them through databaseGroups.

        A database with provider = null isn't created by anything: it only
        exists to be listed in groups, e.g. one running outside the cluster.
        Set its connection details and credentials (from `secrets`) by hand.
      '';
      type = types.attrsOf (
        types.submodule {
          options = {
            provider = mkOption {
              description = "databaseProviders entry this database lives on, or null for an unmanaged database.";
              type = types.nullOr types.str;
              default = null;
            };
            kind = mkOption {
              description = "Database engine.";
              type = types.enum [
                "postgresql"
                "mariadb"
              ];
              default = "postgresql";
            };
            owner = mkOption {
              description = "The app that declared this database, if any.";
              type = types.nullOr types.str;
              default = null;
            };
            host = mkOption {
              description = "Database host.";
              type = types.str;
            };
            port = mkOption {
              description = "Database port.";
              type = types.port;
            };
            name = mkOption {
              description = "Database name.";
              type = types.str;
            };
            username = mkOption {
              description = "Role that owns the database (created by the provider when provisioned).";
              type = types.str;
            };
            password = mkOption {
              description = "Password for that role.";
              type = types.str;
            };
            provision = mkOption {
              description = "Have the provider's app create the database and role. Ignored when provider is null.";
              type = types.bool;
              default = true;
            };
            extensions = mkOption {
              description = ''
                Postgres extensions to create in this database (e.g. [ "vector" ]).
                Created as the superuser: most extensions (pgvector included)
                refuse non-superusers even when they own the database.
              '';
              type = types.listOf types.str;
              default = [ ];
            };
            readAllTables = mkOption {
              description = ''
                Grant `username` SELECT on every table in the public schema, now
                and in the future. For apps that connect as postgres, so their
                tables aren't owned by `username`; otherwise reporting
                connections that use `username` can't read anything.
              '';
              type = types.bool;
              default = false;
            };
            groups = mkOption {
              description = ''
                Groups this database belongs to, for consumers that take
                databaseGroups. Every database with a provider is also in
                "admin" without listing it here.
              '';
              type = types.listOf types.str;
              default = [ ];
            };
          };
        }
      );
      default = { };
    };

    databaseGroups = mkOption {
      description = ''
        Read-only: group name -> list of `databases` entries in that group.
        "admin" holds every database with a provider, plus any that list it.
      '';
      type = types.attrsOf (types.listOf types.attrs);
      readOnly = true;
    };

    databaseConnectionsFor = mkOption {
      description = ''
        Read-only: a function from a list of group names to the connection
        details ({ name, kind, host, port, username, password }) of every
        database in any of them, without duplicates. What reporting tools use.
      '';
      type = types.functionTo (types.listOf types.attrs);
      readOnly = true;
    };
  };

  config = {
    databaseConnectionsFor =
      groups:
      map (db: {
        inherit (db)
          name
          kind
          host
          port
          username
          password
          ;
      }) (lib.unique (lib.concatMap (g: config.databaseGroups.${g} or [ ]) groups));

    databaseGroups =
      let
        dbs = lib.attrValues config.databases;
        named = lib.unique (lib.concatMap (db: db.groups) dbs);
        inGroup = g: db: lib.elem g db.groups || (g == "admin" && db.provider != null);
      in
      lib.genAttrs (lib.unique ([ "admin" ] ++ named)) (g: lib.filter (inGroup g) dbs);
  };
}

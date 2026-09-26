{ lib, ... }:
{
  options.nfsTargets = lib.mkOption {
    description = ''
      Named NFS targets. Keyed by name (i.e. the value used in
      services.<name>.nfs.target). mkArgoApp uses these as mkDefault values
      for the nfs server and (server, basePath + subPath) path.
    '';
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          server = lib.mkOption {
            description = "NFS server hostname/IP.";
            type = lib.types.str;
          };
          basePath = lib.mkOption {
            description = "Base NFS export path on the server.";
            type = lib.types.str;
          };
        };
      }
    );
    default = { };
  };
}

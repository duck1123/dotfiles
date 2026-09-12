{ lib, ... }:
{
  options.ingressProviders = lib.mkOption {
    description = ''
      Named ingress providers. Keyed by name (i.e. the value used in
      services.<name>.ingress.provider). mkArgoApp uses these as mkDefault
      values for ingressClassName, clusterIssuer, and domain.
    '';
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          ingressClassName = lib.mkOption {
            description = "Ingress class name for this provider.";
            type = lib.types.str;
          };
          clusterIssuer = lib.mkOption {
            description = "cert-manager ClusterIssuer for this provider.";
            type = lib.types.str;
          };
          domain = lib.mkOption {
            description = "Domain suffix apps on this provider are hosted under (e.g. home.kronkltd.net).";
            type = lib.types.str;
          };
        };
      }
    );
    default = { };
  };
}

{ ... }:
{
  flake.nixidyApps.cert-manager =
    {
      charts,
      config,
      crdImports,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "cert-manager";
      # https://artifacthub.io/packages/helm/cert-manager/cert-manager
      chart = charts.jetstack.cert-manager;
      defaultValues = cfg: { crds.enabled = true; };

      extraOptions = {
        cloudflare.token = mkOption {
          description = mdDoc "Cloudflare API token for DNS-01 ACME challenge (Zone:DNS:Edit + Zone:Zone:Read)";
          type = types.str;
          default = "";
        };
        email = mkOption {
          description = mdDoc "Email address for Let's Encrypt registration";
          type = types.str;
          default = "";
        };
      };

      sopsSecrets =
        cfg:
        optionalAttrs (cfg.cloudflare.token != "") {
          cloudflare-api-token = {
            api-token = cfg.cloudflare.token;
          };
        };

      extraResources =
        cfg:
        optionalAttrs (cfg.cloudflare.token != "" && cfg.email != "") {
          clusterIssuers.letsencrypt-prod.spec = {
            acme = {
              email = cfg.email;
              server = "https://acme-v02.api.letsencrypt.org/directory";
              privateKeySecretRef.name = "letsencrypt-prod-key";
              solvers = [
                {
                  dns01.cloudflare.apiTokenSecretRef = {
                    name = "cloudflare-api-token";
                    key = "api-token";
                  };
                }
              ];
            };
          };
        };

      extraConfig = cfg: { nixidy.applicationImports = [ (toString crdImports."cert-manager") ]; };
    };
}

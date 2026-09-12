{ ... }:
{
  flake.nixidyApps.kite =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } {
      name = "kite";

      # https://github.com/kite-org/kite (formerly zxh326/kite)
      chart = helm.downloadHelmChart {
        repo = "https://zxh326.github.io/kite";
        chart = "kite";
        version = "0.15.0";
        chartHash = "sha256-ZIaOXRKTWUjnQD1GVgDAT4kxx0oX+DT9LHYIbm4+3i0=";
      };

      uses-ingress = true;

      # `pvcName = "kite-storage"` to match the chart's `existingClaim` value
      # below. Shape only -- no volumeHandle here, that's environment-specific
      # (see env/dev/kite.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        data = {
          pvcName = "kite-storage";
          size = "1Gi";
        };
      };

      extraOptions = {
        encryptKey = mkOption {
          description = mdDoc "This is the key used for encrypting sensitive data";
          type = types.str;
          default = "kite-default-encryption-key-change-in-production";
        };

        jwtSecret = mkOption {
          description = mdDoc "This is the key used for signing JWT tokens";
          type = types.str;
          default = "kite-default-jwt-secret-key-change-in-production";
        };
      };

      defaultValues =
        cfg: with cfg; {
          inherit encryptKey jwtSecret;
          deploymentStrategy.type = "Recreate";
          db.sqlite.persistence.pvc = {
            enabled = true;
            accessModes = [ "ReadWriteOnce" ];
            size = "1Gi";
            storageClass = cfg.storageClassName;
            # Pinned via the `volumes` parameter above instead of letting the
            # chart create the PVC -- see docs/pinned-volumes.md.
            existingClaim = "kite-storage";
          };
          host = ingress.domain;
          ingress = with cfg.ingress; {
            annotations = optionalAttrs (clusterIssuer != "") {
              "cert-manager.io/cluster-issuer" = clusterIssuer;
            };
            className = ingressClassName;
            enabled = enable;
            hosts = [
              {
                host = domain;
                paths = [
                  {
                    path = "/";
                    pathType = "ImplementationSpecific";
                  }
                ];
              }
            ];
            tls = [
              {
                hosts = [ domain ];
                secretName = "${domain}-tls";
              }
            ];
          };
          nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;
        };
    };
}

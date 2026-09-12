{ ... }:
{
  flake.nixidyApps.longhorn =
    {
      charts,
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "longhorn";
      namespace = "longhorn-system";

      # https://artifacthub.io/packages/helm/longhorn/longhorn
      chart = charts.longhorn.longhorn;

      uses-ingress = true;

      extraOptions = {
        backupTarget = mkOption {
          description = mdDoc "Backup target URL (e.g. s3://bucket@region/ or nfs://host:/path)";
          type = types.str;
          default = "";
        };

        backupTargetCredential = {
          accessKey = mkOption {
            description = mdDoc "S3 access key for backup target";
            type = types.str;
            default = "";
          };
          secretKey = mkOption {
            description = mdDoc "S3 secret key for backup target";
            type = types.str;
            default = "";
          };
          endpoint = mkOption {
            description = mdDoc "S3-compatible endpoint URL (leave empty for AWS)";
            type = types.str;
            default = "";
          };
        };
      };

      sopsSecrets =
        cfg:
        let
          hasS3Creds = cfg.backupTargetCredential.accessKey != "";
        in
        lib.optionalAttrs hasS3Creds {
          longhorn-backup-target-secret = {
            AWS_ACCESS_KEY_ID = cfg.backupTargetCredential.accessKey;
            AWS_SECRET_ACCESS_KEY = cfg.backupTargetCredential.secretKey;
            AWS_ENDPOINTS = cfg.backupTargetCredential.endpoint;
          };
        };

      extraAppConfig =
        cfg:
        lib.optionalAttrs (cfg.backupTarget != "") {
          yamls = [
            ''
              apiVersion: longhorn.io/v1beta2
              kind: BackupTarget
              metadata:
                name: default
                namespace: longhorn-system
              spec:
                backupTargetURL: "${cfg.backupTarget}"
                credentialSecret: "${
                  if cfg.backupTargetCredential.accessKey != "" then "longhorn-backup-target-secret" else ""
                }"
                pollInterval: "5m0s"
            ''
          ];
        };

      defaultValues = cfg: {
        defaultSettings = {
          defaultReplicaCount = 1;
          replicaFileSyncHttpClientTimeout = 120;
        };

        ingress = with cfg.ingress; {
          annotations = optionalAttrs (clusterIssuer != "") {
            "cert-manager.io/cluster-issuer" = clusterIssuer;
          };
          inherit ingressClassName;
          enabled = true;
          host = domain;
          tls = tls.enable;
        };

        longhornUI.replicas = 1;

        persistence = {
          defaultClass = false;
          defaultClassReplicaCount = 1;
          reclaimPolicy = "Retain";
        };

        preUpgradeChecker.jobEnabled = false;

        # Ensure Longhorn runs on all nodes, including control-plane nodes
        longhornManager = {
          tolerations = [
            {
              key = "node-role.kubernetes.io/control-plane";
              operator = "Exists";
              effect = "NoSchedule";
            }
            {
              key = "node-role.kubernetes.io/master";
              operator = "Exists";
              effect = "NoSchedule";
            }
            {
              key = "CriticalAddonsOnly";
              operator = "Exists";
            }
          ];
        };

        # Ensure CSI plugin runs on all nodes
        csi = {
          attacherReplicas = 3;
          provisionerReplicas = 3;
          resizerReplicas = 3;
          snapshotterReplicas = 3;
          tolerations = [
            {
              key = "node-role.kubernetes.io/control-plane";
              operator = "Exists";
              effect = "NoSchedule";
            }
            {
              key = "node-role.kubernetes.io/master";
              operator = "Exists";
              effect = "NoSchedule";
            }
            {
              key = "CriticalAddonsOnly";
              operator = "Exists";
            }
          ];
        };
      };
    };
}

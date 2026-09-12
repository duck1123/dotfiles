{ ... }:
{
  flake.nixidyApps.prometheus =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "prometheus";

      extraAppConfig = cfg: {
        syncPolicy.finalSyncOpts = [
          "ServerSideApply=true"
          "Replace=true"
          "CreateNamespace=true"
        ];
      };

      chart = lib.helm.downloadHelmChart {
        repo = "https://prometheus-community.github.io/helm-charts";
        chart = "kube-prometheus-stack";
        version = "83.6.0";
        chartHash = "sha256-SGneLc9/b+VwtbLfrhB3HtFPpECc8pGmisu3XgP0cxA=";
      };

      defaultValues = cfg: {
        crds.upgradeJob.enabled = true;

        alertmanager.alertmanagerSpec = {
          nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;

          resources = {
            limits = {
              cpu = "500m";
              memory = "512Mi";
            };
            requests = {
              cpu = "100m";
              memory = "128Mi";
            };
          };

          storage.volumeClaimTemplate.spec = {
            accessModes = [ "ReadWriteOnce" ];
            resources.requests.storage = "10Gi";
            storageClassName = cfg.storageClassName;
          };
        };

        defaultRules.create = true;
        enabled = cfg.alertmanager.enabled or true;
        grafana.enabled = false;
        kubeStateMetrics.enabled = true;

        nodeExporter = {
          enabled = true;

          serviceMonitor = {
            attachMetadata.node = true;

            relabelings = [
              {
                replacement = "$$1";
                sourceLabels = [ "__meta_kubernetes_pod_node_name" ];
                targetLabel = "instance";
              }
            ];
          };
        };

        prometheus = {
          enabled = true;
          prometheusSpec = {
            additionalScrapeConfigs = cfg.additionalScrapeConfigs or [ ];
            nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;
            podMonitorSelector = { };
            podMonitorSelectorNilUsesHelmValues = false;
            retention = "30d";
            serviceMonitorSelector = { };
            serviceMonitorSelectorNilUsesHelmValues = false;

            storageSpec.volumeClaimTemplate.spec = {
              accessModes = [ "ReadWriteOnce" ];
              resources.requests.storage = cfg.prometheusStorageSize;
              storageClassName = cfg.storageClassName;
            };

            resources = {
              limits = {
                cpu = "2000m";
                memory = "4Gi";
              };
              requests = {
                cpu = "500m";
                memory = "2Gi";
              };
            };
          };
        };

        prometheusOperator = {
          enabled = true;
          nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;
          # Give ArgoCD time to recognize job completion before k8s deletes it (avoids "waiting for completion of hook" sync hang)
          admissionWebhooks.patch.ttlSecondsAfterFinished = 60;
        };
      };

      extraOptions = {
        additionalScrapeConfigs = mkOption {
          description = mdDoc "Additional Prometheus scrape configurations";
          type = types.listOf types.attrs;
          default = [ ];
        };

        prometheusStorageSize = mkOption {
          description = mdDoc "Storage size for Prometheus TSDB (e.g. 50Gi, 100Gi). Increase if you hit 'no space left on device'.";
          type = types.str;
          default = "100Gi";
        };

        alertmanager = {
          enabled = mkOption {
            description = mdDoc "Enable Alertmanager";
            type = types.bool;
            default = true;
          };
        };
      };
    };
}

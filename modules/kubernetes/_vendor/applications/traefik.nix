{ ... }:
{
  flake.nixidyApps.traefik =
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
      name = "traefik";

      # https://artifacthub.io/packages/helm/traefik/traefik
      chart = charts.traefik.traefik;

      extraOptions = {
        service.type = mkOption {
          description = mdDoc "Traefik Service type. Use LoadBalancer with MetalLB so Ingress (ingressClassName: traefik) is reachable on a LAN VIP.";
          type = types.enum [
            "ClusterIP"
            "LoadBalancer"
            "NodePort"
          ];
          default = "LoadBalancer";
        };

        service.loadBalancerIP = mkOption {
          description = mdDoc "Optional fixed IP to request from MetalLB via the metallb.universe.tf/loadBalancerIPs annotation. Leave empty to let MetalLB auto-assign.";
          type = types.str;
          default = "";
        };

        service.hostPorts = mkOption {
          description = mdDoc "Also bind web (80) and websecure (443) as hostPorts so the pod node's real IP is usable for external port forwarding, independent of MetalLB.";
          type = types.bool;
          default = false;
        };
      };

      defaultValues = cfg: {
        service.spec.type = cfg.service.type;
        service.annotations = optionalAttrs (cfg.service.loadBalancerIP != "") {
          "metallb.universe.tf/loadBalancerIPs" = cfg.service.loadBalancerIP;
        };
        ports = optionalAttrs cfg.service.hostPorts {
          web.hostPort = 80;
          websecure.hostPort = 443;
        };
        # providers.kubernetesGateway.statusAddress.hostname = "localhost";
        additionalArguments = [
          # Restrict the Kubernetes Ingress provider to ingressClassName=traefik so
          # it doesn't also match ingresses meant for other controllers (e.g. the
          # Tailscale operator's ingressClassName=tailscale) and terminate TLS for
          # them with its own default self-signed cert.
          "--providers.kubernetesingress.ingressclass=traefik"
          "--entryPoints.web.forwardedHeaders.insecure=true"
          "--entryPoints.web.proxyProtocol.insecure=true"
          "--entryPoints.web.transport.respondingTimeouts.readTimeout=600s"
          "--entryPoints.web.transport.respondingTimeouts.writeTimeout=600s"
          "--entryPoints.web.transport.respondingTimeouts.idleTimeout=600s"
        ];
      };

      extraConfig = cfg: { nixidy.applicationImports = [ (toString crdImports.traefik) ]; };
    };
}

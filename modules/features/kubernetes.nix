{ ... }:
{
  flake.types.generic.feature-options.kubernetes =
    { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    mkOption {
      type = types.submodule {
        options = {
          client = simpleFeature { inherit inputs lib; } "kubernetes client";
          server = simpleFeature { inherit inputs lib; } "kubernetes server";
          token = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Token for joining an existing k3s cluster as an agent (plain text, not recommended)";
          };
          tokenFile = mkOption {
            type = types.nullOr types.path;
            default = null;
            description = "Path to sops-encrypted secrets file containing k3s_token key";
            example = ./../../secrets/k3s-token.yaml;
          };
          serverAddr = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Server address (hostname or IP) for joining an existing k3s cluster";
          };
          gpu = mkOption {
            type = types.nullOr (
              types.enum [
                "amd"
                "nvidia"
              ]
            );
            default = null;
            description = "Enable GPU support for Kubernetes pods (amd or nvidia)";
          };
          dualStack = simpleFeature { inherit inputs lib; } "IPv6 dual-stack networking";
          argocdBootstrap = simpleFeature { inherit inputs lib; } "bootstrap ArgoCD and apply 00-master root Application on first boot";
          extraK3sFlags = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = ''
              Extra k3s CLI flags appended after the defaults. For dual-stack, you often need a
              host IPv6 address on some interface; if k3s fails with "no IPv6 address was found on node",
              enable IPv6 on the NIC or set e.g. "--node-ip=192.168.0.1,2001:db8:…" here (see k3s dual-stack docs).
            '';
          };
        };
      };
      default = { };
      description = "Kubernetes configuration";
    };

  flake.modules.nixos.kubernetes-feature =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.kubernetes.server.enable {
        environment.systemPackages =
          with pkgs;
          [
            k3s
            nfs-utils
            openiscsi
          ]
          ++ lib.optional (config.host.features.kubernetes.gpu == "nvidia") [
            nvidia-podman
          ];

        networking.firewall = {
          enable = false;
          allowedTCPPorts = [ 6443 ];
          allowedUDPPorts = [ 8472 ];
        };

        sops.secrets = lib.mkMerge [
          # ArgoCD SSH deploy key — decrypted only when bootstrap is enabled
          (lib.mkIf config.host.features.kubernetes.argocdBootstrap.enable {
            argocd-deploy-key = {
              sopsFile = ./../../secrets/k8s.enc.yaml;
              key = "argocd/sshDeployKey";
              path = "/run/secrets/argocd-deploy-key";
              mode = "0400";
            };
          })
          # k3s join token — only needed when joining an existing cluster
          (lib.mkIf (config.host.features.kubernetes.tokenFile != null) {
            k3s-token = {
              sopsFile = config.host.features.kubernetes.tokenFile;
              key = "k3s_token";
              path = "/run/secrets/k3s-token";
              mode = "0400";
              owner = "root";
              group = "root";
              restartUnits = [ "k3s.service" ];
            };
          })
        ];

        # Trim the token file using a systemd service that runs before k3s
        systemd.services."k3s-token-trim" = lib.mkIf (config.host.features.kubernetes.tokenFile != null) {
          description = "Trim k3s token file";
          wantedBy = [ "k3s.service" ];
          requiredBy = [ "k3s.service" ];
          before = [ "k3s.service" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script =
            let
              tokenPath = config.sops.secrets.k3s-token.path;
            in
            ''
              # Wait for the secret file to be available (sops-nix installs it during activation)
              for i in {1..60}; do
                if [ -f ${tokenPath} ]; then
                  # Read and trim any trailing/leading whitespace/newlines from the token
                  # Use a temp file to ensure atomic write
                  token=$(tr -d '\n\r\t ' < ${tokenPath} | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

                  # Write trimmed token back atomically using a temp file
                  tmpfile=$(mktemp)
                  echo -n "$token" > "$tmpfile"
                  chmod 400 "$tmpfile"
                  mv "$tmpfile" ${tokenPath}
                  token_len=$(echo -n "$token" | wc -c)
                  echo "Token file trimmed successfully (length: $token_len chars, starts with: $(echo -n "$token" | head -c 3))"
                  exit 0
                fi
                sleep 0.5
              done
              echo "Error: Token file ${tokenPath} not found after waiting 30 seconds"
              exit 1
            '';
        };

        # Configure GPU support if enabled
        hardware.graphics = lib.mkIf (config.host.features.kubernetes.gpu == "amd") {
          enable = true;
          enable32Bit = true;
          extraPackages = with pkgs; [
            mesa
            rocmPackages.clr.icd
            rocmPackages.rocm-device-libs
          ];
        };

        services = {
          k3s =
            let
              kubernetes = config.host.features.kubernetes;
              # Use sops secret path if tokenFile is set, otherwise use plain token
              tokenPath = if kubernetes.tokenFile != null then config.sops.secrets.k3s-token.path else null;
              isAgent = (kubernetes.token != null || tokenPath != null) && kubernetes.serverAddr != null;
              isDualStack = kubernetes.dualStack.enable;
              baseFlags =
                (
                  if isAgent then
                    [ ]
                  else
                    [
                      (
                        if isDualStack then "--cluster-cidr=10.42.0.0/16,fd42:42:42::/56" else "--cluster-cidr=10.42.0.0/16"
                      )
                      (
                        if isDualStack then
                          "--service-cidr=10.43.0.0/16,fd43:43:43::/112"
                        else
                          "--service-cidr=10.43.0.0/16"
                      )
                      "--disable=traefik"
                    ]
                    ++ lib.optional isDualStack "--flannel-ipv6-masq"
                )
                ++ kubernetes.extraK3sFlags;
            in
            {
              enable = true;
              role = if isAgent then "agent" else "server";
            }
            // lib.optionalAttrs (tokenPath != null) {
              tokenFile = tokenPath;
            }
            // lib.optionalAttrs (tokenPath == null && kubernetes.token != null) {
              token = kubernetes.token;
            }
            // lib.optionalAttrs (kubernetes.serverAddr != null) {
              serverAddr = kubernetes.serverAddr;
            }
            // {
              extraFlags = baseFlags;
            };

          openiscsi = {
            enable = true;
            name = "${config.host.hostname}-initiatorhost";
          };
        };

        # Bootstrap services — enabled via kubernetes.argocdBootstrap.enable.
        # Idempotent: safe to re-run on every boot via kubectl apply.
        # Ordering: k3s → install ArgoCD → register repo credential → apply root Application.
        systemd.services.k8s-install-argocd = lib.mkIf config.host.features.kubernetes.argocdBootstrap.enable {
          description = "Bootstrap: install ArgoCD into the k3s cluster";
          after = [
            "k3s.service"
            "network-online.target"
          ];
          wants = [ "network-online.target" ];
          wantedBy = [ "multi-user.target" ];
          environment.KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            until ${pkgs.k3s}/bin/k3s kubectl get nodes >/dev/null 2>&1; do
              echo "Waiting for k3s API..."
              sleep 5
            done
            # Skip the heavy network fetch if ArgoCD is already installed
            if ${pkgs.k3s}/bin/k3s kubectl get deployment argocd-server -n argocd >/dev/null 2>&1; then
              echo "ArgoCD already installed, skipping"
              exit 0
            fi
            ${pkgs.k3s}/bin/k3s kubectl create namespace argocd --dry-run=client -o yaml \
              | ${pkgs.k3s}/bin/k3s kubectl apply -f -
            ${pkgs.k3s}/bin/k3s kubectl apply -n argocd \
              -f https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml
          '';
        };

        systemd.services.k8s-bootstrap-argocd-repo = lib.mkIf config.host.features.kubernetes.argocdBootstrap.enable {
          description = "Bootstrap: register argo-manifests SSH credential with ArgoCD";
          after = [ "k8s-install-argocd.service" ];
          requires = [ "k8s-install-argocd.service" ];
          wantedBy = [ "multi-user.target" ];
          environment.KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            # kubectl apply is idempotent — safe to re-apply the credential on every boot
            ${pkgs.k3s}/bin/k3s kubectl apply -f - <<EOF
            apiVersion: v1
            kind: Secret
            metadata:
              name: argo-manifests-repo-creds
              namespace: argocd
              labels:
                argocd.argoproj.io/secret-type: repository
            stringData:
              type: git
              url: git@github.com:duck1123/argo-manifests.git
              sshPrivateKey: |
            $(sed 's/^/    /' /run/secrets/argocd-deploy-key)
            EOF
          '';
        };

        systemd.services.k8s-apply-master-application = lib.mkIf config.host.features.kubernetes.argocdBootstrap.enable {
          description = "Bootstrap: apply ArgoCD 00-master root Application pointing at argo-manifests";
          after = [ "k8s-bootstrap-argocd-repo.service" ];
          requires = [ "k8s-bootstrap-argocd-repo.service" ];
          wantedBy = [ "multi-user.target" ];
          environment.KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script =
            let
              masterApp = pkgs.writeText "00-master.yaml" ''
                apiVersion: argoproj.io/v1alpha1
                kind: Application
                metadata:
                  name: 00-master
                  namespace: argocd
                spec:
                  destination:
                    namespace: argocd
                    server: https://kubernetes.default.svc
                  project: default
                  source:
                    directory:
                      jsonnet: {}
                      recurse: true
                    path: dev/apps
                    repoURL: git@github.com:duck1123/argo-manifests.git
                    targetRevision: master
                  syncPolicy:
                    automated:
                      prune: true
                      selfHeal: true
                    syncOptions:
                      - CreateNamespace=true
              '';
            in
            ''
              until ${pkgs.k3s}/bin/k3s kubectl get crd applications.argoproj.io >/dev/null 2>&1; do
                echo "Waiting for ArgoCD CRDs..."
                sleep 5
              done
              ${pkgs.k3s}/bin/k3s kubectl apply -f ${masterApp}
            '';
        };

        system.activationScripts.makeUsrBinSymlinks = ''
          mkdir -p /usr/bin
          ln -sf ${pkgs.openiscsi}/bin/iscsiadm /usr/bin/iscsiadm
        '';
      };
    };
}

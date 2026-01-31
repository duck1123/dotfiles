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

        # Set up sops secret for k3s token if tokenFile is specified
        sops.secrets = lib.mkIf (config.host.features.kubernetes.tokenFile != null) {
          k3s-token = {
            sopsFile = config.host.features.kubernetes.tokenFile;
            key = "k3s_token";
            path = "/run/secrets/k3s-token";
            mode = "0400";
            owner = "root";
            group = "root";
            restartUnits = [ "k3s.service" ];
          };
        };

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
              baseFlags =
                if isAgent then
                  [ ]
                else
                  [
                    "--cluster-cidr=10.42.0.0/16"
                    "--disable=traefik"
                  ];
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
              extraFlags = toString baseFlags;
            };

          openiscsi = {
            enable = true;
            name = "${config.host.hostname}-initiatorhost";
          };
        };

        system.activationScripts.makeUsrBinSymlinks = ''
          mkdir -p /usr/bin
          ln -sf ${pkgs.openiscsi}/bin/iscsiadm /usr/bin/iscsiadm
        '';
      };
    };
}

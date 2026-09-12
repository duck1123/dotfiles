{ config, secrets, ... }:
{
  services.homepage = {
    enable = true;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    monitoring.autokuma.enable = true;
    # Would otherwise list itself (uses-ingress defaults homepage.enable to true).
    homepage.enable = false;

    settings = {
      title = "Homelab";
      theme = "dark";
    };

    # Cluster nodes run glances (dotfiles' `features.glances`, port 61208) directly
    # on the host OS -- not a k8s app, so it can't be auto-discovered like
    # mkArgoApp services and is listed here by static LAN IP instead.
    extraGroups = {
      Media = {
        # Plex runs via NixOS `services.plex` on edgenix (dotfiles'
        # `features.media.server`), not in the cluster -- not a mkArgoApp
        # service, so it's listed here by static LAN IP instead.
        plex = {
          href = "http://192.168.0.22:32400/web";
          ping = "192.168.0.22";
          siteMonitor = "http://192.168.0.22:32400";
          widget = {
            type = "plex";
            url = "http://192.168.0.22:32400";
            key = "{{HOMEPAGE_VAR_PLEX_API_KEY}}";
          };
        };
      };

      Automation = {
        # ArgoCD doesn't set `uses-ingress` (see applications/argocd.nix), so it
        # can't auto-discover a homepage tile -- listed here by its real URL.
        argocd = {
          href = "https://argocd.dev.kronkltd.net/";
        };
      };

      Nodes = {
        nixmini = {
          href = "http://192.168.0.25:61208";
          ping = "192.168.0.25";
          siteMonitor = "http://192.168.0.25:61208";
          widget = {
            type = "glances";
            url = "http://192.168.0.25:61208";
            version = 4;
            metric = "cpu";
          };
        };

        nasnix = {
          href = "http://192.168.0.16:61208";
          ping = "192.168.0.16";
          siteMonitor = "http://192.168.0.16:61208";
          widget = {
            type = "glances";
            url = "http://192.168.0.16:61208";
            version = 4;
            metric = "cpu";
          };
        };

        edgenix = {
          href = "http://192.168.0.22:61208";
          ping = "192.168.0.22";
          siteMonitor = "http://192.168.0.22:61208";
          widget = {
            type = "glances";
            url = "http://192.168.0.22:61208";
            version = 4;
            metric = "cpu";
          };
        };

        inspernix = {
          href = "http://192.168.0.24:61208";
          ping = "192.168.0.24";
          siteMonitor = "http://192.168.0.24:61208";
          widget = {
            type = "glances";
            url = "http://192.168.0.24:61208";
            version = 4;
            metric = "cpu";
          };
        };

        powerspecnix = {
          href = "http://192.168.0.29:61208";
          ping = "192.168.0.29";
          siteMonitor = "http://192.168.0.29:61208";
          widget = {
            type = "glances";
            url = "http://192.168.0.29:61208";
            version = 4;
            metric = "cpu";
          };
        };
      };
    };

    # Values for widgets that need an API key/password. Reference them from a
    # service's `homepage.extraSettings.widget.*` (or `widgets`/`extraGroups`
    # here) as `{{HOMEPAGE_VAR_<KEY>}}` -- homepage substitutes the placeholder
    # from the container env at render time, so the ConfigMap/git stay
    # plaintext-free. See applications/homepage.nix for how this is wired.
    # Each mkArgoApp service below auto-populates its own
    # `homepage.extraSettings.widget` once its `apiKey` (or, for immich,
    # `adminApiKey`) is set -- see applications/immich.nix for the pattern.
    # This just has to supply the matching env var for each widget's
    # `{{HOMEPAGE_VAR_*}}` placeholder. PLEX_API_KEY backs the static Plex
    # entry above (Plex isn't a mkArgoApp service).
    widgetSecrets = {
      IMMICH_API_KEY = config.services.immich.adminApiKey;
      AUDIOBOOKSHELF_API_KEY = config.services.audiobookshelf.apiKey;
      KOMGA_API_KEY = config.services.komga.apiKey;
      LIDARR_API_KEY = config.services.lidarr.apiKey;
      PLEX_API_KEY = secrets.plex.key;
      PROWLARR_API_KEY = config.services.prowlarr.apiKey;
      RADARR_API_KEY = config.services.radarr.apiKey;
      SABNZBD_API_KEY = config.services.sabnzbd.apiKey;
      SONARR_API_KEY = config.services.sonarr.apiKey;
      STASHAPP_API_KEY = config.services.stashapp.apiKey;
      TRILIUM_API_KEY = config.services.trilium.apiKey;
    };
  };
}

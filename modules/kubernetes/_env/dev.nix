{ pkgs, self, ... }:
let
  secrets = self.lib.loadSecrets { inherit pkgs; };
  base-domain = "dev.kronkltd.net";
  tail-domain = "bearded-snake.ts.net";
  clusterIssuer = "letsencrypt-prod";
  nas-host = "192.168.0.124";
  nas-base = "/volume1";

  # Helper function to generate database entries for *arr applications
  # Takes a list of app configs and generates main + log databases
  arrDatabases =
    apps:
    builtins.concatLists (
      map (app: [
        {
          name = if app.name == "prowlarr" then "${app.name}-main" else app.name;
          username = app.name;
          password = secrets.postgresql.userPassword;
        }
        {
          name = if app.name == "prowlarr" then "${app.name}-log" else "${app.name}-log";
          username = app.name;
          password = secrets.postgresql.userPassword;
        }
      ]) apps
    );
in
{
  # FIXME: naughty config
  ageRecipients = "age1n372e8dgautnjhecllf7uvvldw9g6vyx3kggj0kyduz5jr2upvysue242c";

  nixidy = {
    defaults.syncPolicy.autoSync = {
      enabled = true;
      prune = true;
      selfHeal = true;
    };

    target = {
      branch = "master";
      repository = "git@github.com:duck1123/argo-manifests.git";
      # Manifests are written to <manifests-repo-checkout>/dev/
      # Activation must run from the kubernetes/manifests/ checkout directory.
      rootPath = "dev";
    };
  };

  services = {
    affine = {
      enable = true;
      hostAffinity = "edgenix";

      database = {
        host = "postgresql.postgresql";
        port = 5432;
        name = "affine";
        username = "affine";
        password = secrets.postgresql.userPassword;
      };

      redis = {
        host = "redis.redis";
        port = 6379;
        password = secrets.redis.password;
      };

      serverExternalUrl = "https://affine.${tail-domain}";

      ingress = {
        domain = "affine.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      storageClassName = "longhorn";
    };

    argocd.enable = true;

    audiobookshelf = {
      enable = true;
      hostAffinity = "edgenix";

      ingress = {
        domain = "audiobookshelf.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}/Audiobooks";
      };

      storageClassName = "longhorn";
    };

    booklore = {
      enable = false;
      hostAffinity = "edgenix";

      database = {
        host = "mariadb.mariadb";
        password = secrets.booklore.database.password;
        port = 3306;
        name = "booklore";
        username = "booklore";
      };

      gid = "0";

      ingress = {
        domain = "booklore.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
        # Optional: Enable local-only ingress using Traefik
        localIngress = {
          enable = true;
          domain = "booklore.local";
          tls.enable = false; # Set to true if you have cert-manager configured for local domains
        };
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}/Books";
      };

      storageClassName = "longhorn";
      uid = "0";
    };

    cert-manager.enable = true;

    cloudbeaver = {
      enable = true;
      hostAffinity = "edgenix";

      ingress = {
        domain = "cloudbeaver.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      storageClassName = "longhorn";
    };

    demo = {
      enable = true;
      ingress = {
        domain = "demo.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };
    };

    gluetun = {
      controlServer = { inherit (secrets.gluetun) password username; };
      enable = true;
      hostAffinity = "edgenix";
      mullvadAccountNumber = secrets.mullvad.id;
      storageClassName = "longhorn";
    };

    forgejo = {
      admin = { inherit (secrets.forgejo.admin) password username; };
      enable = true;

      ingress = {
        domain = "forgejo.${tail-domain}";
        ingressClassName = "tailscale";
      };

      postgresql = {
        inherit (secrets.forgejo.postgresql)
          adminPassword
          adminUsername
          replicationPassword
          userPassword
          ;
      };

      storageClassName = "longhorn";
    };

    grafana = {
      adminPassword = secrets.grafana.password or "";
      enable = true;
      hostAffinity = "edgenix";

      ingress = {
        clusterIssuer = "tailscale";
        domain = "grafana.${tail-domain}";
        ingressClassName = "tailscale";
      };

      additionalDatasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          access = "proxy";
          url = "http://prometheus-kube-prometheus-prometheus.prometheus:9090";
          isDefault = true;
          editable = true;
          jsonData.httpMethod = "POST";
        }
        {
          name = "Loki";
          type = "loki";
          access = "proxy";
          url = "http://loki-gateway.loki.svc.cluster.local";
          editable = true;
        }
      ];

      additionalDashboardProviders = [
        {
          name = "default";
          orgId = 1;
          folder = "";
          type = "file";
          disableDeletion = false;
          editable = true;
          options.path = "/var/lib/grafana/dashboards/default";
        }
      ];
    };

    prometheus = {
      alertmanager.enabled = true;
      enable = true;
      hostAffinity = "edgenix";
    };

    homarr = {
      enable = true;
      hostAffinity = "edgenix";

      ingress = {
        domain = "homarr.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      secretEncryptionKey = secrets.homarr.secretEncryptionKey;
      storageClassName = "longhorn";
    };

    home-assistant = {
      enable = true;
      # hostAffinity = "edgenix";

      # https://github.com/AiDot-Development-Team/hass-AiDot
      installAidot.enable = true;

      ingress = {
        domain = "home-assistant.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
        tls.enable = true;
      };

      storageClassName = "longhorn";
    };

    immich = {
      enable = true;

      database = {
        inherit (secrets.immich.database) password username;
        host = "postgresql.postgresql";
        port = 5432;
        name = "immich";
      };

      hostAffinity = "nixmini";

      ingress = {
        domain = "immich.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs.enable = false;

      externalLibrary = {
        enable = true;
        server = nas-host;
        path = "${nas-base}/Photos";
      };

      redis = {
        inherit (secrets.immich.redis) password;
        host = "redis.redis";
        port = 6379;
        dbIndex = 0;
      };

      storageClassName = "longhorn";
    };

    kite = {
      inherit (secrets.kite) encryptKey jwtSecret;
      enable = true;
      hostAffinity = "edgenix";
      storageClassName = "longhorn";

      ingress = {
        domain = "kite.${tail-domain}";
        clusterIssuer = "tailscale";
        ingressClassName = "tailscale";
        tls.enable = true;
      };
    };

    komga = {
      enable = true;

      ingress = {
        domain = "komga.${tail-domain}";
        clusterIssuer = "tailscale";
        ingressClassName = "tailscale";
        localIngress.enable = true;
        tls.enable = true;
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}/Books";
      };
    };

    lidarr = {
      enable = true;

      ingress = {
        domain = "lidarr.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      vpn = {
        enable = false;
        sharedGluetunService = "gluetun.gluetun";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}";

        slskdDownloads = {
          enable = true;
          path = "${nas-base}/slskd_downloads";
        };
      };

      database = {
        enable = true;
        host = "postgresql.postgresql";
        port = 5432;
        name = "lidarr";
        username = "lidarr";
        password = secrets.postgresql.userPassword;
      };

      hostAffinity = "edgenix";

      replicas = 1;
      storageClassName = "longhorn";
    };

    listenarr = {
      database = {
        enable = true;
        host = "postgresql.postgresql";
        name = "listenarr";
        password = secrets.postgresql.userPassword;
        port = 5432;
        username = "listenarr";
      };

      enable = true;

      ingress = {
        clusterIssuer = "tailscale";
        domain = "listenarr.${tail-domain}";
        ingressClassName = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}";

        audiobooks = {
          enable = true;
          path = "${nas-base}/Audiobooks";
        };
      };

      replicas = 1;
      storageClassName = "longhorn";

      vpn = {
        enable = false;
        sharedGluetunService = "gluetun.gluetun";
      };
    };

    loki = {
      enable = true;
      hostAffinity = "edgenix";
      retention = "720h"; # 30 days
      storageClassName = "longhorn";
      storageSize = "20Gi";
    };

    longhorn = {
      enable = true;

      ingress = {
        domain = "longhorn.${tail-domain}";
        ingressClassName = "tailscale";
        tls.enable = true;
      };
    };

    mariadb = {
      auth = {
        inherit (secrets.mariadb)
          database
          password
          replicationPassword
          rootPassword
          username
          ;
      };

      enable = true;
      hostAffinity = "edgenix";
      storageClassName = "longhorn";

      extraDatabases = [
        {
          name = "booklore";
          username = "booklore";
          password = secrets.booklore.database.password;
        }
        {
          name = "romm";
          username = "mariadb";
          password = secrets.mariadb.password;
        }
      ];
    };

    mealie = {
      enable = true;
      hostAffinity = "edgenix";

      ingress = {
        domain = "mealie.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      storageClassName = "longhorn";
    };

    memos = {
      enable = true;
      hostAffinity = "edgenix";

      database = {
        host = "postgresql.postgresql";
        port = 5432;
        name = "memos";
        username = "postgres";
        password = secrets.postgresql.userPassword;
      };

      ingress = {
        domain = "memos.${tail-domain}";
        ingressClassName = "tailscale";
      };
    };

    metallb = {
      enable = true;
      l2.addresses = [ "192.168.0.240-192.168.0.250" ];
      l2.excludeNodes = [ "powerspecnix" ];
    };

    n8n = {
      enable = false;

      hostAffinity = "nasnix";

      encryptionKey = (secrets.n8n or { }).encryptionKey or "";

      ingress = {
        domain = "n8n.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };
    };

    nix-csi.enable = true;

    nocodb =
      let
        storage-backend = "rustfs";
      in
      {
        allowLocalExternalDatabases = true;
        auth.jwtSecret = (secrets.nocodb or { }).jwtSecret or "";
        enable = true;

        ingress = {
          domain = "nocodb.${tail-domain}";
          ingressClassName = "tailscale";
          clusterIssuer = "tailscale";
        };

        database = {
          host = "postgresql.postgresql";
          port = 5432;
          name = "nocodb";
          username = "nocodb";
          password = (secrets.nocodb.postgresql or { }).password or secrets.postgresql.userPassword;
        };

        redis = {
          host = "redis.redis";
          port = 6379;
          password = secrets.redis.password;
        };

        storage =
          if storage-backend == "rustfs" then
            {
              enable = true;
              backend = "rustfs";
              bucketName = (secrets.rustfs or { }).bucketName or "nocodb";
              endpoint = "http://rustfs.rustfs:9000";
              region = (secrets.rustfs or { }).region or "us-east-1";
              accessKey = (secrets.rustfs or { }).accessKey or "";
              secretKey = (secrets.rustfs or { }).secretKey or "";
            }
          else
            {
              enable = true;
              backend = "minio";
              bucketName = (secrets.nocodb.minio or { }).bucketName or "nocodb";
              endpoint = "http://minio.minio:9000";
              region = (secrets.nocodb.minio or { }).region or "us-east-1";
              accessKey = (secrets.nocodb.minio or { }).rootUser or "";
              secretKey = (secrets.nocodb.minio or { }).rootPassword or "";
            };

        publicUrl = "https://nocodb.${tail-domain}";
      };

    pihole = {
      auth = { inherit (secrets.pihole) email password; };
      enable = true;
      hostAffinity = "nasnix";

      ingress = {
        domain = "pihole.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
        tls.enable = true;
      };

      ingress.localIngress = {
        enable = true;
        serviceName = "pihole-web";
        servicePort = 80;
      };
      serviceDnsLoadBalancerIP = "192.168.0.242";
      storageClassName = "longhorn";
      # Wildcard: all *.local queries resolve to the Traefik LoadBalancer IP.
      # Requires clients to use Pi-hole as their DNS server.
      customDnsEntries = [
        "address=/.local/192.168.0.241"
        "address=/.dev.kronkltd.net/192.168.0.241"
      ];
    };

    postgresql = {
      auth = {
        inherit (secrets.postgresql)
          adminPassword
          adminUsername
          replicationPassword
          userPassword
          ;
      };

      enable = true;
      hostAffinity = "edgenix";
      storageClassName = "longhorn";

      extraDatabases =
        arrDatabases [
          { name = "prowlarr"; }
          { name = "sonarr"; }
          { name = "radarr"; }
          { name = "lidarr"; }
          { name = "whisparr"; }
          { name = "listenarr"; }
        ]
        ++ [
          {
            name = "immich";
            username = "immich";
            password = secrets.postgresql.userPassword;
          }
          {
            name = "gitea";
            username = "postgres";
            password = secrets.postgresql.userPassword;
          }
          {
            name = "affine";
            username = "affine";
            password = secrets.postgresql.userPassword;
          }
          {
            name = "memos";
            username = "postgres";
            password = secrets.postgresql.userPassword;
          }
          {
            name = "nocodb";
            username = "nocodb";
            password = secrets.postgresql.userPassword;
          }
          {
            name = "romm";
            username = "postgres";
            password = secrets.postgresql.userPassword;
          }
        ];
    };

    promtail.enable = true;

    prowlarr = {
      database = {
        enable = false;
        host = "postgresql.postgresql";
        port = 5432;
        name = "prowlarr-main";
        username = "prowlarr";
        password = secrets.postgresql.userPassword;
      };

      enable = true;
      hostAffinity = "edgenix";

      ingress = {
        domain = "prowlarr.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      replicas = 1;
      vpn.enable = false;
    };

    qbittorrent = {
      enable = false;
      hostAffinity = "nasnix";

      ingress = {
        domain = "qbittorrent.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}";
      };

      webui = { inherit (secrets.qbittorrent) password username; };
    };

    radarr = {
      database = {
        enable = true;
        host = "postgresql.postgresql";
        port = 5432;
        name = "radarr";
        username = "radarr";
        password = secrets.postgresql.userPassword;
      };

      enable = true;
      hostAffinity = "edgenix";
      image = "linuxserver/radarr:6.0.4.10291-ls295";

      ingress = {
        domain = "radarr.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}";
      };

      replicas = 1;
      storageClassName = "longhorn";
      vpn.enable = false;
    };

    redis = {
      enable = true;
      hostAffinity = "edgenix";
      password = secrets.redis.password;
    };

    romm = {
      enable = false;

      admin = {
        username = secrets.romm.admin.username;
        password = secrets.romm.admin.password;
      };

      authSecretKey = secrets.romm.authSecretKey;

      database = {
        host = "mariadb.mariadb";
        name = "romm";
        password = secrets.mariadb.password;
        port = 3306;
        username = "mariadb";
      };

      ingress = {
        domain = "romm.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
        # Optional: Enable local-only ingress using Traefik
        localIngress = {
          enable = true;
          domain = "romm.local";
          tls.enable = false; # Set to true if you have cert-manager configured for local domains
        };
      };

      nfs = {
        enable = true;
        server = nas-host;
        libraryPath = "${nas-base}/Roms";
        assetsPath = "${nas-base}/Roms/assets";
        resourcesPath = "${nas-base}/Roms/resources";
      };
    };

    rustfs = {
      accessKey = (secrets.rustfs or { }).accessKey or "";
      enable = true;
      hostAffinity = "nasnix";

      ingress = {
        domain = "rustfs.${tail-domain}";
        api-domain = "api-rustfs.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
        tls.enable = true;
      };

      mode = "standalone";
      secretKey = (secrets.rustfs or { }).secretKey or "";
      storageClassName = "longhorn";
    };

    sabnzbd = {
      enable = true;
      hostAffinity = "edgenix";

      ingress = {
        domain = "sabnzbd.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}";
      };

      replicas = 1;
      useProbes = false;
    };

    sealed-secrets.enable = true;

    slskd = {
      enable = true;

      ingress = {
        domain = "slskd.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      hostAffinity = "edgenix";

      webAuth = {
        username = (secrets.slskd or { }).username or "";
        password = (secrets.slskd or { }).password or "";
      };

      apiKey = (secrets.slskd or { }).apiKey or "";

      vpn = {
        enable = true;
        sharedGluetunService = "gluetun.gluetun";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}/slskd_downloads";
      };

      shares = {
        enable = true;
        server = nas-host;
        path = "${nas-base}/Music";
      };

      replicas = 1;
      storageClassName = "longhorn";
      useProbes = false;
    };

    sonarr = {
      database = {
        enable = true;
        host = "postgresql.postgresql";
        port = 5432;
        name = "sonarr";
        username = "sonarr";
        password = secrets.postgresql.userPassword;
      };

      enable = true;
      image = "linuxserver/sonarr:version-4.0.17.2952";
      hostAffinity = "edgenix";

      ingress = {
        domain = "sonarr.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}";
      };

      replicas = 1;
      vpn.enable = false;
    };

    sops.enable = true;

    soularr = {
      enable = true;
      # hostAffinity = "edgenix";

      lidarr = {
        host = "lidarr.lidarr";
        port = 8686;
        downloadDir = "/downloads/slskd_downloads";
        apiKey = (secrets.soularr or { }).lidarrApiKey or "";
      };

      slskd = {
        host = "slskd.slskd";
        port = 5030;
        apiKey = (secrets.slskd or { }).apiKey or "";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}/slskd_downloads";
      };

      scriptInterval = 300;
      storageClassName = "longhorn";
    };

    stashapp = {
      enable = true;

      ingress = {
        domain = "stashapp.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}/Videos";
      };

      replicas = 1;
      enableGPU = true;
    };

    tailscale = {
      enable = true;
      oauth = { inherit (secrets.tailscale) authKey clientId clientSecret; };
    };

    tdarr = {
      enable = true;
      healthcheckcpuWorkers = 0;
      healthcheckgpuWorkers = 1;
      transcodeCpuWorkers = 0;
      transcodeGpuWorkers = 0;
      hostAffinity = "edgenix";
      # hostAffinity = "powerspecnix";

      ingress = {
        domain = "tdarr.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = nas-base;
      };

      puid = 1000;
      pgid = 1000;

      replicas = 1;
      storageClassName = "longhorn";
      useProbes = false;
      vpn.enable = false;
      enableGPU = true;
      enableNvidiaGPU = false;
      # Edgenix has two cards; WX 3200 (VAAPI) is renderD129. Mount it as renderD128 so Tdarr's hardcoded path works.
      # vaapiRenderDevice = "renderD129";
      libvaDriverName = "radeonsi";
      transcodegpuWorkers = 1;
    };

    traefik = {
      enable = true;
      service.loadBalancerIP = "192.168.0.241";
      service.hostPorts = false;
    };

    whisparr = {
      database = {
        enable = true;
        host = "postgresql.postgresql";
        port = 5432;
        name = "whisparr";
        username = "whisparr";
        password = secrets.postgresql.userPassword;
      };

      enable = false;

      ingress = {
        domain = "whisparr.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      nfs = {
        enable = true;
        server = nas-host;
        path = "${nas-base}";
      };

      replicas = 1;
    };

    windmill = {
      enable = false;
      hostAffinity = "edgenix";
      image = "ghcr.io/windmill-labs/windmill-full:latest";

      ingress = {
        domain = "windmill.${tail-domain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      database = {
        host = "postgresql.postgresql";
        port = 5432;
        name = "windmill";
        username = secrets.windmill.database.username;
        password = secrets.windmill.database.password;
      };

      storageClassName = "longhorn";
      replicas = 1;
    };
  };
}

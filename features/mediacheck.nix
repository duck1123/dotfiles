let
  tokenPath = "/run/secrets/windmill-mediacheck-token";
in
{
  description = "Full-decode integrity checks of the NAS media library, reported to Windmill";

  extraOptions =
    { lib, ... }:
    {
      libraries = lib.mkOption {
        description = "Library name (as f/mediacheck/checker knows it) to its mount point on this host.";
        type = lib.types.attrsOf lib.types.str;
        default = {
          movies = "/mnt/movies";
          tv = "/mnt/tv";
        };
      };
    };

  # The checker decodes every video file in full and reports ffmpeg's errors
  # to the media integrity queue in Windmill (f/mediacheck/checker), which
  # keeps the queue and the verdicts. It runs at idle priority as a user
  # service, so `mediacheck pause` (systemctl --user stop) frees the machine
  # for a game without sudo; a stop mid-file hands the claim back on restart.
  homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.host.features.mediacheck;

      mediacheck = pkgs.writers.writeNuBin "mediacheck" {
        makeWrapperArgs = [
          "--prefix"
          "PATH"
          ":"
          (lib.makeBinPath (
            with pkgs;
            [
              coreutils
              ffmpeg-headless
              findutils
              systemd
            ]
          ))
          "--set"
          "MEDIACHECK_URL"
          "https://windmill.home.kronkltd.net/api/w/default/jobs/run_wait_result/p/f/mediacheck/checker"
          "--set"
          "MEDIACHECK_TOKEN_FILE"
          tokenPath
          "--set"
          "MEDIACHECK_LIBRARIES"
          (lib.concatStringsSep "," (lib.mapAttrsToList (name: root: "${name}=${root}") cfg.libraries))
        ];
      } ../resources/mediacheck/mediacheck.nu;
    in
    {
      home.packages = [ mediacheck ];

      systemd.user.services.mediacheck = {
        Unit.Description = "Full-decode integrity checks of the NAS media library";

        Service = {
          ExecStart = "${mediacheck}/bin/mediacheck run";
          Nice = 19;
          CPUSchedulingPolicy = "idle";
          IOSchedulingClass = "idle";
          # A runaway decode kills only ffmpeg: the exit code gets reported, so
          # the file leaves the queue instead of being claimed again on restart.
          MemoryMax = "8G";
          OOMPolicy = "continue";
          Restart = "on-failure";
          RestartSec = "5min";
        };

        Install.WantedBy = [ "default.target" ];
      };
    };

  # Token for the checker: a Windmill token that can only run
  # f/mediacheck/checker (windmill.mediacheckToken). Like wayle's capture
  # token, the k8s secrets file is also encrypted for the server host keys.
  nixos =
    { config, ... }:
    {
      sops.secrets.windmill-mediacheck-token = {
        sopsFile = ./../secrets/k8s.enc.yaml;
        key = "windmill/mediacheckToken";
        path = tokenPath;
        mode = "0400";
        owner = config.host.identity.username;
        group = "users";
      };
    };
}

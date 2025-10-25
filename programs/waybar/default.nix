{ host, lib, ... }: {
  options = {
    features.waybar.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable waybar";
    };
  };

  config = lib.mkIf host.features.waybar.enable {
    programs.waybar = {
      enable = true;

      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          modules-left = [ "hyprland/workspaces" ];
          modules-center = [ "clock" ];
          modules-right =
            [ "battery" "tray" "cpu" "memory" "pulseaudio" "network" ];

          cpu.format = "<span color='#b4befe'>🖥️ </span>{usage}%";

          clock = {
            format = "<span color='#b4befe'>🕒 </span>{:%Y-%m-%d  %H:%M}";
            tooltip = true;
            tooltip-format = "{:%Y-%m-%d %a}";
          };

          memory = {
            interval = 1;
            format =
              "<span color='#b4befe'>🧠 </span>{used:0.1f}G/{total:0.1f}G";
          };

          network = {
            interface = "wlp3s0";
            format = "{ifname}";
            format-wifi = "<span color='#b4befe'>🛜 </span>{essid}";
            format-ethernet = "{ipaddr}/{cidr} ";
            format-disconnected = "<span color='#b4befe'> </span>No Network";
            tooltip = false;
          };

          pulseaudio = {
            format = "<span color='#b4befe'>{icon}</span> {volume}%";
            format-muted = "🔇";
            tooltip = false;
            format-icons = {
              headphone = "🎧";
              default = [ "🔊" "🔉" "🔈" ];
            };
            scroll-step = 1;
            on-click = "pavucontrol";
          };
        };
      };
    };
  };
}

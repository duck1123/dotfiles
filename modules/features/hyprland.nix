_: {
  flake.types.generic.feature-options.hyprland =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "hyprland feature";

  flake.modules.homeManager.hyprland =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      mkLuaInline = lib.generators.mkLuaInline;
      toLua = lib.generators.toLua { };

      mainMod = "SUPER";

      # Renders a `hl.bind(...)` call. `mods` is a list of Hyprland modifier
      # names (e.g. [ "SUPER" "SHIFT" ]); `dispatcher` is a raw Lua
      # expression string, e.g. "hl.dsp.window.close()".
      mkBind =
        {
          mods ? [ ],
          key,
          dispatcher,
          opts ? null,
        }:
        {
          _args = [
            (if mods == [ ] then key else "${lib.concatStringsSep " + " mods} + ${key}")
            (mkLuaInline dispatcher)
          ]
          ++ lib.optional (opts != null) opts;
        };

      # Maps a Hyprlang dispatcher name + arg to the equivalent hl.dsp.*
      # Lua expression, verified against Hyprland's own Lua dispatcher
      # bindings (src/config/lua/bindings/LuaBindingsDispatchers.cpp).
      dispatcherFor =
        command: arg:
        {
          exec = "hl.dsp.exec_cmd(${toLua arg})";
          killactive = "hl.dsp.window.close()";
          fullscreen = "hl.dsp.window.fullscreen()";
          exit = "hl.dsp.exit()";
          pseudo = "hl.dsp.window.pseudo()";
          togglefloating = ''hl.dsp.window.float({ action = "toggle" })'';
          movefocus = "hl.dsp.focus({ direction = ${toLua arg} })";
          movewindow = "hl.dsp.window.move({ direction = ${toLua arg} })";
          workspace = "hl.dsp.focus({ workspace = ${toLua arg} })";
          movetoworkspace = "hl.dsp.window.move({ workspace = ${toLua arg} })";
          cyclenext = "hl.dsp.window.cycle_next()";
          bringactivetotop = "hl.dsp.window.bring_to_top()";
        }
        .${command} or (throw "hyprland: unmapped dispatcher '${command}'");

      mkKeyBind =
        {
          mods ? [ ],
          key,
          command,
          arg ? "",
        }:
        mkBind {
          inherit mods key;
          dispatcher = dispatcherFor command arg;
        };

      # Mouse binds: Hyprlang's `bindm` doesn't exist in Lua, mouse drag
      # and resize are just `hl.bind(...)` calls with `{ mouse = true }`.
      mkMouseBind =
        {
          mods ? [ ],
          key,
          command,
        }:
        mkBind {
          inherit mods key;
          dispatcher = if command == "movewindow" then "hl.dsp.window.drag()" else "hl.dsp.window.resize()";
          opts = {
            mouse = true;
          };
        };
    in
    {
      config = lib.mkIf config.host.features.hyprland.enable {
        home.packages = with pkgs; [
          cascadia-code
          font-awesome
          grim
          hyprpaper
          hyprshot
          jq
          nautilus
          ncpamixer
          nwg-dock-hyprland
          nwg-drawer
          nwg-launchers
          pamixer
          pavucontrol
          socat
          wev
          wofi
        ];

        programs.kitty.enable = true;

        wayland.windowManager.hyprland = {
          enable = true;
          configType = "lua";
          settings = {
            mainMod._var = mainMod;
            terminal._var = "kitty";
            menu._var = "wofi --show drun";
            fileManager._var = "nautiulus";

            config = {
              general = {
                gaps_in = 5;
                gaps_out = 5;
                border_size = 2;
                layout = "dwindle";
              };

              decoration.rounding = 10;

              dwindle."preserve_split" = true;

              animations.enabled = true;
            };

            curve = {
              _args = [
                "ease"
                {
                  type = "bezier";
                  points = [
                    [
                      0.4
                      0.02
                    ]
                    [
                      0.21
                      1
                    ]
                  ];
                }
              ];
            };

            animation = [
              {
                leaf = "windows";
                enabled = true;
                speed = 3.5;
                bezier = "ease";
                style = "slide";
              }
              {
                leaf = "windowsOut";
                enabled = true;
                speed = 3.5;
                bezier = "ease";
                style = "slide";
              }
              {
                leaf = "border";
                enabled = true;
                speed = 6;
                bezier = "default";
              }
              {
                leaf = "fade";
                enabled = true;
                speed = 3;
                bezier = "ease";
              }
              {
                leaf = "workspaces";
                enabled = true;
                speed = 3.5;
                bezier = "ease";
              }
            ];

            env = [
              {
                _args = [
                  "XCURSOR_SIZE"
                  "24"
                ];
              }
              {
                _args = [
                  "HYPRCURSOR_SIZE"
                  "24"
                ];
              }
            ];

            # FIXME: This is environment specific
            monitor = [
              {
                output = "HDMI-A-1";
                mode = "1920x1080";
                position = "0x0";
                scale = 1;
              }
              {
                output = "DP-3";
                mode = "1920x1080";
                position = "1920x0";
                scale = 1;
              }
            ];

            bind =
              (map mkKeyBind [
                # Letter key bindings (sorted by key)
                {
                  mods = [ mainMod ];
                  key = "a";
                  command = "exec";
                  arg = "pear-desktop";
                }
                {
                  mods = [ mainMod ];
                  key = "b";
                  command = "exec";
                  arg = "zen-beta";
                }
                {
                  mods = [ mainMod ];
                  key = "c";
                  command = "killactive";
                }
                {
                  mods = [ mainMod ];
                  key = "d";
                  command = "exec";
                  arg = ''nautilus "$(cat ~/.last_dir 2>/dev/null || echo $HOME)"'';
                }
                {
                  mods = [ mainMod ];
                  key = "e";
                  command = "exec";
                  arg = ''emacsclient -c -a "" --eval "(magit-status \"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\")"'';
                }
                {
                  mods = [ mainMod ];
                  key = "f";
                  command = "fullscreen";
                }
                {
                  mods = [ mainMod ];
                  key = "g";
                  command = "exec";
                  arg = "gossip";
                }
                {
                  mods = [ mainMod ];
                  key = "h";
                  command = "exec";
                  arg = ''kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)" htop'';
                }
                # {
                #   mods = [ mainMod ];
                #   key = "j";
                #   command = "togglesplit";
                # }
                {
                  mods = [ mainMod ];
                  key = "k";
                  command = "exec";
                  arg = ''kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)" k9s'';
                }
                {
                  mods = [ mainMod ];
                  key = "l";
                  command = "exec";
                  arg = "lens";
                }
                {
                  mods = [ mainMod ];
                  key = "m";
                  command = "exit";
                }
                {
                  mods = [ mainMod ];
                  key = "n";
                  command = "exec";
                  arg = ''kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)" pnu'';
                }
                {
                  mods = [ mainMod ];
                  key = "p";
                  command = "pseudo";
                }
                {
                  mods = [ mainMod ];
                  key = "r";
                  command = "exec";
                  arg = "rofiWindow";
                }
                {
                  mods = [ mainMod ];
                  key = "t";
                  command = "exec";
                  arg = "teams-for-linux";
                }
                {
                  mods = [ mainMod ];
                  key = "u";
                  command = "exec";
                  arg = ''kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)" jjui'';
                }
                {
                  mods = [ mainMod ];
                  key = "v";
                  command = "togglefloating";
                }
                {
                  mods = [ mainMod ];
                  key = "w";
                  command = "exec";
                  arg = "nwg-drawer";
                }
                # Direction key bindings
                {
                  mods = [ mainMod ];
                  key = "down";
                  command = "movefocus";
                  arg = "d";
                }
                {
                  mods = [
                    mainMod
                    "SHIFT"
                  ];
                  key = "down";
                  command = "movewindow";
                  arg = "d";
                }
                {
                  mods = [ mainMod ];
                  key = "left";
                  command = "movefocus";
                  arg = "l";
                }
                {
                  mods = [
                    mainMod
                    "SHIFT"
                  ];
                  key = "left";
                  command = "movewindow";
                  arg = "l";
                }
                {
                  mods = [ mainMod ];
                  key = "right";
                  command = "movefocus";
                  arg = "r";
                }
                {
                  mods = [
                    mainMod
                    "SHIFT"
                  ];
                  key = "right";
                  command = "movewindow";
                  arg = "r";
                }
                {
                  mods = [ mainMod ];
                  key = "up";
                  command = "movefocus";
                  arg = "u";
                }
                {
                  mods = [
                    mainMod
                    "SHIFT"
                  ];
                  key = "up";
                  command = "movewindow";
                  arg = "u";
                }
                # Special key bindings
                {
                  mods = [ mainMod ];
                  key = "mouse_down";
                  command = "workspace";
                  arg = "e+1";
                }
                {
                  mods = [ mainMod ];
                  key = "mouse_up";
                  command = "workspace";
                  arg = "e-1";
                }
                {
                  key = "Print";
                  command = "exec";
                  arg = "hyprshot -m region";
                }
                {
                  mods = [ "SHIFT" ];
                  key = "Print";
                  command = "exec";
                  arg = ''grim -g "$(slurp)"'';
                }
                {
                  mods = [ mainMod ];
                  key = "RETURN";
                  command = "exec";
                  arg = ''kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)"'';
                }
                {
                  mods = [ mainMod ];
                  key = "SPACE";
                  command = "exec";
                  arg = "nwg-drawer";
                }
                {
                  mods = [ mainMod ];
                  key = "Tab";
                  command = "cyclenext";
                }
                {
                  mods = [ mainMod ];
                  key = "Tab";
                  command = "bringactivetotop";
                }
                # Media keys
                {
                  key = "XF86AudioLowerVolume";
                  command = "exec";
                  arg = "pamixer -d 5";
                }
                {
                  key = "XF86AudioMicMute";
                  command = "exec";
                  arg = "pamixer --default-source -t";
                }
                {
                  key = "XF86AudioMute";
                  command = "exec";
                  arg = "pamixer -t";
                }
                {
                  key = "XF86AudioPause";
                  command = "exec";
                  arg = "playerctl play-pause";
                }
                {
                  key = "XF86AudioPlay";
                  command = "exec";
                  arg = "playerctl play-pause";
                }
                {
                  key = "XF86AudioRaiseVolume";
                  command = "exec";
                  arg = "pamixer -i 5";
                }
                {
                  key = "XF86MonBrightnessDown";
                  command = "exec";
                  arg = "light -U 20";
                }
                {
                  key = "XF86MonBrightnessUp";
                  command = "exec";
                  arg = "light -A 20";
                }
              ])
              ++ (
                # workspaces
                # binds mainMod + [shift +] {1..9} to [move to] workspace {1..9}
                builtins.concatMap (
                  i:
                  let
                    ws = i + 1;
                  in
                  [
                    (mkKeyBind {
                      mods = [ mainMod ];
                      key = "code:1${toString i}";
                      command = "workspace";
                      arg = ws;
                    })
                    (mkKeyBind {
                      mods = [
                        mainMod
                        "SHIFT"
                      ];
                      key = "code:1${toString i}";
                      command = "movetoworkspace";
                      arg = ws;
                    })
                  ]
                ) (builtins.genList (i: i) 9)
              )
              ++ (
                let
                  left-click = "mouse:272";
                  right-click = "mouse:273";
                  back-thumb = "mouse:275";
                  front-thumb = "mouse:276";
                in
                map mkMouseBind [
                  {
                    mods = [ mainMod ];
                    key = left-click;
                    command = "movewindow";
                  }
                  {
                    mods = [ "ALT" ];
                    key = left-click;
                    command = "resizewindow";
                  }
                  {
                    mods = [ mainMod ];
                    key = right-click;
                    command = "resizewindow";
                  }
                  {
                    key = back-thumb;
                    command = "movewindow";
                  }
                  {
                    key = front-thumb;
                    command = "resizewindow";
                  }
                ]
              );

            window_rule = [
              {
                match.class = "pavucontrol";
                float = true;
              }
              {
                match.class = "blueman-manager";
                float = true;
              }
              {
                match.class = "mpv";
                float = true;
                center = true;
                size = "934 525";
              }
            ];
          };
        };
      };
    };
}

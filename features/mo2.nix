{
  description = "Mod Organizer 2 for Steam games, running in each game's Proton prefix (installed by MO2-LINT)";

  # MO2 isn't in nixpkgs, and it has to live in the game's Wine prefix anyway, so each game's
  # instance is installed imperatively with MO2-LINT (github.com/furglitch/modorganizer2-linux-installer):
  #   steam-run ~/Downloads/mo2-lint install
  # Its binaries bundle their own protontricks, which runs /sbin/ldconfig, so they only work
  # inside steam-run's FHS env.
  #
  # MO2-LINT's own launch path (the game's Steam "Play" button, redirected to MO2 by a custom
  # compat tool) runs MO2 inside Steam's pressure-vessel container, and its nxm handler runs
  # Wine outside it. The two get separate wineservers, so links open a second MO2, and its
  # `steam://launch/<id>` fallback doesn't start anything. Instead, `mo2` runs both MO2 and its
  # nxmhandler.exe the same way (protontricks --no-bwrap, in the game's prefix), so they share
  # one wineserver: links reach a running MO2 or start one. It reads MO2-LINT's state file, so
  # every game installed with it works.
  #
  # LOOT, xEdit, BodySlide, Nemesis etc. are Windows tools run from inside MO2.
  homeManager =
    { lib, pkgs, ... }:
    let
      # protontricks runs Proton's wine through steam-run, whose bwrap gives every call a
      # private tmpfs /tmp. Wine's server socket lives in /tmp/.wine-$UID, so each call got
      # its own wineserver and the nxm handler started a second MO2 instead of finding the
      # running one. Share the real /tmp so every `mo2` call joins the same wineserver.
      protontricks = pkgs.protontricks.override {
        steam = pkgs.steam.override { privateTmp = false; };
      };

      mo2 = pkgs.writeShellApplication {
        name = "mo2";
        runtimeInputs = [
          protontricks
          pkgs.jq
          pkgs.zenity
        ];
        text = ''
          # mo2 [GAME]     open Mod Organizer 2 for GAME (an MO2-LINT game id or Nexus slug)
          # mo2 nxm://...  hand a Nexus "Mod Manager Download" link to that game's MO2
          state="$HOME/.config/mo2-lint/state.json"

          die() {
            printf 'mo2: %s\n' "$1" >&2
            zenity --error --title="Mod Organizer 2" --text="$1" 2>/dev/null || true
            exit 1
          }

          # Single-quote a word for the shell protontricks runs -c commands in.
          quote() { printf "'%s'" "''${1//\'/\'\\\'\'}"; }

          find_instance() {
            jq -c --arg g "$1" 'first(.instances[] | select(.game == $g or .nexus_slug == $g)) // empty' "$state"
          }

          [[ -f $state ]] || die "No MO2-LINT instances yet. Install one with: steam-run mo2-lint install"

          arg="''${1-}"
          if [[ $arg == nxm://* ]]; then
            slug="''${arg#nxm://}"
            slug="''${slug%%/*}"
            instance=$(find_instance "$slug")
            [[ -n $instance ]] || die "No MO2 instance for Nexus game '$slug'."
            exe=nxmhandler.exe
            args=("$arg")
          else
            if [[ -n $arg ]]; then
              instance=$(find_instance "$arg")
              [[ -n $instance ]] || die "No MO2 instance for '$arg'."
            elif [[ $(jq '.instances | length' "$state") == 1 ]]; then
              instance=$(jq -c '.instances[0]' "$state")
            else
              game=$(jq -r '.instances[] | .game, .display_name' "$state" |
                zenity --list --title="Mod Organizer 2" --text="Choose a game" \
                  --column=id --column=Game --hide-column=1 --print-column=1) || exit 0
              instance=$(find_instance "$game")
            fi
            exe=ModOrganizer.exe
            args=()
          fi

          [[ $(jq -r .launcher <<<"$instance") == steam ]] ||
            die "Only Steam installs are supported (this one is $(jq -r .launcher <<<"$instance"))."
          appid=$(jq -r .launcher_ids.steam <<<"$instance")
          path=$(jq -r .instance_path <<<"$instance")
          proton=$(jq -r '.proton_wrapper.proton_version // empty' <<<"$instance")

          cmd="wine $(quote "$path/$exe")"
          for a in "''${args[@]}"; do cmd+=" $(quote "$a")"; done

          # Games started from MO2 call SteamAPI_RestartAppIfNecessary; with these set they
          # don't bounce back through Steam (which would start MO2-LINT's redirected launch).
          export SteamAppId="$appid" SteamGameId="$appid"
          [[ -n $proton ]] && export PROTON_VERSION="$proton"
          exec protontricks --no-bwrap -c "$cmd" "$appid"
        '';
      };
    in
    {
      # The shared-/tmp protontricks stays private to `mo2`; `gaming` installs the stock one.
      home.packages = [
        mo2
      ]
      ++ (with pkgs; [
        curl
        p7zip
        steamtinkerlaunch
        unzip
        winetricks
        zenity
      ]);

      xdg.desktopEntries = {
        mo2 = {
          name = "Mod Organizer 2";
          genericName = "Mod Manager";
          exec = "${lib.getExe mo2}";
          terminal = false;
          categories = [ "Game" ];
        };

        mo2-nxm-handler = {
          name = "Mod Organizer 2 NXM Handler";
          exec = "${lib.getExe mo2} %u";
          mimeType = [ "x-scheme-handler/nxm" ];
          noDisplay = true;
          categories = [ "Game" ];
        };
      };

      # mimeapps.list isn't managed by home-manager here, and MO2-LINT points nxm:// at its
      # own entry (mo2lint_nxm-handler.desktop) on every install/update, so claim it back on
      # each switch.
      home.activation.mo2NxmHandler = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
        run ${pkgs.xdg-utils}/bin/xdg-mime default mo2-nxm-handler.desktop x-scheme-handler/nxm
      '';
    };

  nixos =
    { pkgs, ... }:
    {
      # GE-Proton plays Bethesda games' intro/menu videos that stock Proton can't.
      programs.steam.extraCompatPackages = [ pkgs.proton-ge-bin ];
    };
}

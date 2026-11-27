{
  description = "Skyrim modding: tools to install and run Mod Organizer 2 inside the game's Steam Proton prefix";

  # MO2 isn't in nixpkgs, and it has to live in the game's Wine prefix anyway, so it's
  # installed imperatively with MO2-LINT (github.com/furglitch/modorganizer2-linux-installer):
  #   steam-run ~/Downloads/mo2-lint install
  # Its binaries bundle their own protontricks, which runs /sbin/ldconfig, so they only work
  # inside steam-run's FHS env. MO2 opens when Skyrim is launched from Steam with the
  # "MO2 Skyrim Special Edition" compat tool it adds.
  # LOOT, xEdit, BodySlide, Nemesis etc. are Windows tools run from inside MO2.
  homeManager =
    { lib, pkgs, ... }:
    let
      # MO2-LINT's own handler (~/.local/share/mo2-lint/nxm-handler), run inside steam-run.
      nxm-handler = pkgs.writeShellScript "mo2-nxm-handler" ''
        exec ${lib.getExe pkgs.steam-run} "$HOME/.local/share/mo2-lint/nxm-handler" "$@"
      '';
    in
    {
      home.packages = with pkgs; [
        curl
        p7zip
        protontricks
        steamtinkerlaunch
        unzip
        winetricks
        zenity
      ];

      xdg.desktopEntries.mo2-nxm-handler = {
        name = "Mod Organizer 2 NXM Handler";
        exec = "${nxm-handler} %u";
        mimeType = [ "x-scheme-handler/nxm" ];
        noDisplay = true;
        categories = [ "Game" ];
      };

      # mimeapps.list isn't managed by home-manager here, and MO2-LINT points nxm:// at its
      # own entry (mo2lint_nxm-handler.desktop, which runs outside steam-run and fails) on
      # every install/update, so claim it back on each switch.
      home.activation.mo2NxmHandler = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
        run ${pkgs.xdg-utils}/bin/xdg-mime default mo2-nxm-handler.desktop x-scheme-handler/nxm
      '';
    };

  nixos =
    { pkgs, ... }:
    {
      # GE-Proton plays Skyrim's intro/menu videos that stock Proton can't, and is
      # what most MO2-on-Linux guides test against.
      programs.steam.extraCompatPackages = [ pkgs.proton-ge-bin ];
    };
}

{
  description = "Skyrim modding: tools to install and run Mod Organizer 2 inside the game's Steam Proton prefix";

  # MO2 isn't in nixpkgs, and it has to live in the game's Wine prefix anyway, so it's
  # installed imperatively by one of these:
  #   - modorganizer2-linux-installer (github.com/rockerbacon/modorganizer2-linux-installer):
  #     download a release and run `./install.sh`. It needs 7z, curl, zenity and protontricks.
  #   - steamtinkerlaunch: set it as the game's compat tool in Steam, then enable MO2 in its menu.
  # Both register the nxm:// handler so "Mod Manager Download" on Nexus goes to MO2.
  # LOOT, xEdit, BodySlide, Nemesis etc. are Windows tools run from inside MO2.
  homeManager =
    { pkgs, ... }:
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
    };

  nixos =
    { pkgs, ... }:
    {
      # GE-Proton plays Skyrim's intro/menu videos that stock Proton can't, and is
      # what most MO2-on-Linux guides test against.
      programs.steam.extraCompatPackages = [ pkgs.proton-ge-bin ];
    };
}

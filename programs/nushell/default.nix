{ pkgs, ... }: {
  home.packages = with pkgs; [
    nushell
    nu_scripts
    nufmt
    nushellPlugins.highlight
    nushellPlugins.formats
    nushellPlugins.polars
    nushellPlugins.gstat
    nushellPlugins.units
    nushellPlugins.query
    nushellPlugins.dbus
    nushellPlugins.skim
    nushellPlugins.net
  ];

  programs.nushell.enable = true;
}

{ ... }:
{
  flake.types.generic.feature-options.nushell =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "nushell feature";

  flake.modules.homeManager.nushell =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.nushell.enable {
        home = {
          file = {
            "nushell/completions".source = ../../nushell/completions;
            "nushell/me.nu".source = builtins.fetchurl {
              url = "https://raw.githubusercontent.com/ClipplerBlood/me.nu/09e4ee7fbee6a26cb8dd3041e9da1f4de2c8d119/me.nu";
              sha256 = "sha256:1l3mhcwl2mvkz9qg3yzgz7xdrkdr4xzxfxj3mhv22knbaca5dacr";
            };
            "nushell/modules".source = ../../nushell/modules;
          };

          packages = with pkgs; [
            carapace
            fish
            jc
            nushell
            nu-lint
            nu_scripts
            nufmt
            # nushellPlugins.highlight
            nushellPlugins.formats
            # nushellPlugins.polars: broken upstream, vendored ethnum 1.5.2 crate
            # fails to compile under the current rustc (unsafe transmute size mismatch)
            # nushellPlugins.polars
            nushellPlugins.gstat
            # nushellPlugins.units
            nushellPlugins.query
            # nushellPlugins.dbus
            nushellPlugins.skim
            # nushellPlugins.net
            inputs.nur-taskrunner.packages.${pkgs.stdenv.hostPlatform.system}.default
            inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.pnu
          ];

        };

        programs = {
          carapace = {
            enable = true;
            enableNushellIntegration = true;
          };

          nushell = {
            enable = true;
            configFile.source = ../../nushell/config.nu;
            envFile.source = ../../nushell/env.nu;

            extraEnv = ''
              $env.EDITOR = "emacsclient -c -a \'\'";
              $env.VISUAL = "emacsclient -c -a \'\'";
            '';

            shellAliases = {
              cat = "bat";
              hh = "hstr";
            };
          };
        };
      };
    };
}

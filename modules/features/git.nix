{ ... }: {
  flake.types.generic.feature-options.git = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "git feature";

  flake.modules.homeManager.git = { config, lib, ... }: {
    config = lib.mkIf config.host.features.git.enable {
      programs = let inherit (config.host.identity) email gpgKey name;
      in {
        git = {
          enable = true;

          lfs.enable = true;

          settings.user = { inherit email name; };

          signing = {
            signByDefault = false;
            key = gpgKey;
          };
        };
        jujutsu = {
          enable = true;
          settings.user = { inherit name email; };
        };
      };
    };
  };
}


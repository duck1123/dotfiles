{ ... }: {
  flake.types.generic.feature-options.vim = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "vim feature";

  flake.modules.homeManager.vim = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.vim.enable {
      home.packages = with pkgs; [ neovim ];

      programs = {
        vim = {
          enable = true;
          extraConfig = ''
            syntax on
            " Wrap gitcommit file types at the appropriate length
            filetype indent plugin on
          '';
        };
      };
    };
  };
}


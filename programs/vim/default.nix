{ host, lib, pkgs, ... }: {
  options = {
    features.vim.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable vim";
    };
  };

  config = lib.mkIf host.features.vim.enable {
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
}

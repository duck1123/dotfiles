{ host, lib, pkgs, ... }: {
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

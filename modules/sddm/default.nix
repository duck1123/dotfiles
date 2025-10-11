{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.sddm.enable {
    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      package = pkgs.kdePackages.sddm;
      theme = lib.mkForce "sddm-sugar-candy-nix";
      settings = {
        Theme = {
          Current = "sddm-sugar-candy-nix";
          ThemeDir = "/run/current-system/sw/share/sddm/themes";
        };
      };
      sugarCandyNix = {
        enable = true;
        settings = {
          Font = "DejaVu Sans";
          FontSize = "14";
          FormPosition = "center";
          MainColor = "blue";
          PartialBlur = true;
        };
      };
    };
  };
}

{ inputs, pkgs, ... }: {
  services = {
    displayManager.sddm = {
      enable = true;
      theme = "ocean";
      wayland.enable = true;
    };
    xserver.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };
}

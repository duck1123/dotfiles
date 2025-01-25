{ inputs, ... }: {
  services = {
    displayManager.sddm = {
      enable = true;
      theme = "ocean";
      wayland.enable = true;
    };
    xserver.windowManager.i3 = {
      enable = true;
      package = inputs.pkgs.i3-gaps;
    };
  };
}

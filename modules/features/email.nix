{ ... }: {
  flake.modules.homeManager.email = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.email.enable {
      home.packages = with pkgs; [ thunderbird ];
    };
  };
}


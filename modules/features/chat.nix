_: {
  features.chat.homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        discord
        telegram-desktop
      ];
    };
}

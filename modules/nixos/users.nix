_: {
  flake.modules.nixos.users =
    { config, pkgs, ... }:
    let
      inherit (config.host.identity) name username;
    in
    {
      # Define a user account. Don't forget to set a password with 'passwd'.
      users.users."${username}" = {
        isNormalUser = true;
        description = name;
        extraGroups = [
          "dialout"
          "wheel"
        ];
        shell = pkgs.zsh;
      };
    };
}

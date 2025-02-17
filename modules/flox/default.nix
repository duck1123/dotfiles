{ inputs, ... }:
let pkgs = inputs.pkgs;
in {
  environment.systemPackages = with pkgs;
    [ flox.packages.${pkgs.system}.default ];

  nix.settings = {
    trusted-public-keys =
      [ "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs=" ];
    trusted-substituters = [ "https://cache.flox.dev" ];
  };
}

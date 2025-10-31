{
  flake-file = {
    description = "Duck's Nix Environment";

    nixConfig = {
      allow-import-from-derivation = true;
      extra-trusted-public-keys = [
        "duck1123.cachix.org-1:Cj3r3BH7Xuy0zFWy8V/VIB3F7+Gi1m9HB302E9UGV3E="
        # "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        # "vix.cachix.org-1:hP/Lpdsi1dB3AxK9o6coWh+xHzvAc4ztdDYuG7lC6dI="
      ];
      extra-substituters = [
        "https://duck1123.cachix.org"
        # "https://nix-community.cachix.org"
        # "https://vix.cachix.org"
      ];
    };
  };
}

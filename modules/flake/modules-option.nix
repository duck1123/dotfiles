{ lib, ... }: {
  options.flake.modules = lib.mkOption {
    type = lib.types.submodule {
      options = {
        home-manager = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          description = "NixOS modules for different hosts";
          default = { };
        };

        nixos = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          description = "NixOS modules for different hosts";
          default = { };
        };
      };
    };
    description = "Flake modules for different systems";
    default = { };
  };
}

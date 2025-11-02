{ lib, ... }: {
  options.flake.modules = lib.mkOption {
    type = lib.types.submodule {
      options.nixos = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        description = "NixOS modules for different hosts";
        default = { };
      };
    };
    description = "Flake modules for different systems";
    default = { };
  };
}


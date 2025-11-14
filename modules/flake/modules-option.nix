{ lib, ... }: {
  options.flake.modules = lib.mkOption {
    type = lib.types.submodule {
      options = {
        generic = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          description = "Generic modules";
          default = { };
        };

        homeManager = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          description = "NixOS modules for home manager";
          default = { };
        };

        nixos = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          description = "NixOS modules for different hosts";
          default = { };
        };

        options = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          description = "modules for options";
          default = { };
        };
      };
    };
    description = "Flake modules for different systems";
    default = { };
  };
}

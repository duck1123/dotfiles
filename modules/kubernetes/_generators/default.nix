{ inputs, system, ... }@sharedConfig:
let
  gFiles = builtins.attrNames (builtins.readDir ./.);
  generatorFiles = builtins.filter (
    file: builtins.match ".*\\.nix" file != null && file != "default.nix"
  ) gFiles;
  crdImports = builtins.listToAttrs (
    map (file: {
      name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
      value = import (./. + "/${file}") sharedConfig;
    }) generatorFiles
  );
in
{
  inherit crdImports;
}

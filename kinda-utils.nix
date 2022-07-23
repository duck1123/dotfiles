# TODO check out actual shared utils?
let
  # copied from github:belsoft/nixos by way of efim/dotfiles
  findModules = dir:
    builtins.concatLists (builtins.attrValues (builtins.mapAttrs (name: type:
      if type == "regular" && ((builtins.match "(.*)\\.org" name) != null) then
        [ ]
      else if type == "regular" then [{
        name = builtins.elemAt (builtins.match "(.*)\\.nix" name) 0;
        value = dir + "/${name}";
      }] else if (builtins.readDir (dir + "/${name}")) ? "default.nix" then [{
        inherit name;
        value = dir + "/${name}";
      }] else
        findModules (dir + "/${name}")) (builtins.readDir dir)));
in { inherit findModules; }

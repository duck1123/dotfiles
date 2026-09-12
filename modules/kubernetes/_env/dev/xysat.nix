{ secrets, ... }:
let
  apiKey = (secrets.xysat or { }).setupApiKey or "";
in
{
  services.xysat = {
    # Flip on once secrets.xysat.setupApiKey is set (`nur secrets edit`):
    # 1. Log into xyOps, Settings -> API Keys -> create a key with only the
    #    "add_servers" privilege (this key doesn't expire, unlike the 24h
    #    token in the UI's own "Add Server" install command).
    # 2. `nur secrets edit`, add `xysat.setupApiKey: <the key>`.
    # 3. Set enable = true here and `nur switch`.
    enable = true;

    setupUrl =
      if apiKey == "" then "" else "http://xyops.xyops:5522/api/app/satellite/config?t=${apiKey}";

    # Gives jobs the satellite runs access to a small nix-provided toolset at
    # /nix/var/result/bin (on PATH) via nix-csi -- see applications/xysat.nix.
    # Add/remove packages here freely; empty string disables the mount.
    nixExpr = ''
      let
        pkgs = import (builtins.fetchTree {
          type = "github";
          owner = "nixos";
          repo = "nixpkgs";
          ref = "nixos-unstable";
        }) {};
      in
      pkgs.buildEnv {
        name = "xysat-tools";
        paths = with pkgs; [
          bash
          coreutils
          git
          curl
          jq
          nushell
          nix
        ];
      }
    '';
  };
}

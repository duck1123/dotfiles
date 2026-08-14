_: {
  # FIXME: nix-csi's own flake is currently broken when consumed as a flake
  # input (pkgs/default.nix does `builtins.pathExists ../../pynixd`, which
  # resolves outside the source tree and throws instead of returning false).
  # Disabled until that's fixed upstream.
  services.nix-csi.enable = false;
}

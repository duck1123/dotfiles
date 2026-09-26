{ lib, ... }:
{
  options.nodeGpuProfiles = lib.mkOption {
    description = ''
      Per-node GPU profiles for VAAPI hardware acceleration. Keyed by Kubernetes node hostname
      (i.e. the value used in hostAffinity). mkArgoApp uses these as mkDefault values for
      libvaDriverName, vaapiRenderDevice, and renderGroupGID when enableGPU is true.
    '';
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          libvaDriverName = lib.mkOption {
            description = "LIBVA_DRIVER_NAME for this node (e.g. iris for Intel, radeonsi for AMD).";
            type = lib.types.str;
            default = "";
          };
          vaapiRenderDevice = lib.mkOption {
            description = "Host DRI render device (e.g. renderD129) when the VAAPI GPU is not at renderD128. Empty = use /dev/dri directly.";
            type = lib.types.str;
            default = "";
          };
          renderGroupGID = lib.mkOption {
            description = "GID of the host render group for /dev/dri access (run 'getent group render' on the node; 303 on NixOS).";
            type = lib.types.int;
            default = 303;
          };
        };
      }
    );
    default = { };
  };
}

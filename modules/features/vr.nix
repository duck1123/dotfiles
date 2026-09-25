_: {
  features.vr = {
    description = "SteamVR / Steam Frame support";

    nixos =
      { inputs, pkgs, ... }:
      {
        # Pinned separately from nixpkgs so the patched kernel below isn't
        # rebuilt on every flake update (see the nixpkgs-kernel input).
        # Needs >= 7.0: the Steam Frame wireless adapter (28de:2432) is driven
        # by rtw89_8852cu, which only gained Valve's USB ID in 7.0.
        boot.kernelPackages =
          inputs.nixpkgs-kernel.legacyPackages.${pkgs.stdenv.hostPlatform.system}.linuxPackages_latest;

        # The headset hosts a hidden 6 GHz network for the adapter to join. With
        # no country set, the kernel uses the world regdomain (00), which disables
        # 6 GHz entirely, so the adapter never finds it.
        boot.extraModprobeConfig = ''
          options cfg80211 ieee80211_regdom=US
        '';

        # SteamVR's vrsetup.sh tries to `pkexec setcap CAP_SYS_NICE+ep` the
        # vrcompositor-launcher so it can request a high-priority GPU queue
        # (async reprojection). That can't work on NixOS: Steam runs inside
        # bubblewrap with no_new_privs, so file capabilities are ignored even
        # if applied. Instead, let amdgpu hand out high-priority contexts to
        # anyone. This is the fix recommended by the NixOS wiki, and it forces
        # a local kernel build.
        boot.kernelPatches = [
          {
            name = "amdgpu-ignore-ctx-privileges";
            patch = pkgs.fetchpatch {
              name = "cap_sys_nice_begone.patch";
              url = "https://github.com/Frogging-Family/community-patches/raw/master/linux61-tkg/cap_sys_nice_begone.mypatch";
              hash = "sha256-Y3a0+x2xvHsfLax/uwycdJf3xLxvVfkfDVqjkxNaYEo=";
            };
          }
        ];
      };
  };
}

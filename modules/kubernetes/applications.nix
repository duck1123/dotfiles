# Turns every appTemplates/<name>.nix into a nixidy application module by
# passing it through `mkArgoApp`. Apps that don't go through mkArgoApp at all
# define `flake.nixidyApps.<name>` directly (see ./nixidy-apps).
{ config, lib, ... }:
{
  flake.nixidyApps = lib.mapAttrs (
    _: template:
    # Advertise the template's own args (e.g. `charts`, `crdImports`) so the
    # nixidy module system supplies them too.
    lib.setFunctionArgs
      (
        args:
        args.self.lib.mkArgoApp {
          inherit (args)
            config
            lib
            pkgs
            self
            ;
        } (template args)
      )
      (
        lib.functionArgs template
        // {
          config = false;
          lib = false;
          pkgs = false;
          self = false;
        }
      )
  ) config.appTemplates;
}

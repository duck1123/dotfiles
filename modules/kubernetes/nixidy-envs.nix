{ inputs, self, ... }:
{
  perSystem =
    { pkgs, system, ... }:
    let
      # CRD generators from k3s-fleetops, using dotfiles' (shared) inputs for nixidy/nixhelm deps.
      crdImports = (import "${inputs.k3s-fleetops}/generators" { inherit inputs system pkgs; }).crdImports;

      devEnv = inputs.nixidy.lib.mkEnvs {
        inherit pkgs;
        charts = inputs.nixhelm.chartsDerivations.${system};
        envs.dev.modules = [ ./_env/dev.nix ];
        extraSpecialArgs = { inherit self crdImports; };
        modules =
          (builtins.attrValues self.nixidyApps)
          ++ [
            self.modules.generic.ageRecipients
            "${inputs.k3s-fleetops}/modules/secretManifest.nix"
            "${inputs.k3s-fleetops}/modules/secretSpecs.nix"
          ];
      };

      devSecretManifest = devEnv.dev.config.nixidy.secretManifest or [ ];
      devSecretSpecs = {
        ageRecipients = devEnv.dev.config.ageRecipients or "";
        secrets = devEnv.dev.config.nixidy.secretSpecs or [ ];
      };
    in
    {
      nixidyEnvs = devEnv;

      packages.devSecretManifest = pkgs.runCommand "dev-secret-manifest.json" {
        manifest = builtins.toJSON devSecretManifest;
      } ''echo "$manifest" > $out'';

      nixidySecretSpecs.dev = devSecretSpecs;
    };

  transposition.nixidyEnvs = {
    adHoc = true;
  };

  transposition.nixidySecretSpecs = {
    adHoc = true;
  };
}

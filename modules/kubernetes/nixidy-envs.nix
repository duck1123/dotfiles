{ inputs, self, ... }:
let
  secretsFile = builtins.getEnv "DECRYPTED_SECRET_FILE";
  secretsAvailable = secretsFile != "" && builtins.pathExists secretsFile;
in
{
  perSystem =
    { pkgs, system, ... }:
    if !secretsAvailable then
      { }
    else
      let
        # CRD generators vendored from k3s-fleetops, using dotfiles' (shared) inputs for nixidy/nixhelm deps.
        inherit (import ./_vendor/generators { inherit inputs system pkgs; }) crdImports;

        devEnv = inputs.nixidy.lib.mkEnvs {
          inherit pkgs;
          charts = inputs.nixhelm.chartsDerivations.${system};
          envs.dev.modules = [ ./_env/dev.nix ];
          extraSpecialArgs = { inherit self crdImports; };
          modules = (builtins.attrValues self.nixidyApps) ++ [
            self.modules.generic.ageRecipients
            ./_vendor/modules/secretManifest.nix
            ./_vendor/modules/secretSpecs.nix
            ./_vendor/modules/nodeProfiles.nix
            ./_vendor/modules/homepageGroups.nix
            ./_vendor/modules/ingressProviders.nix
            ./_vendor/modules/databaseProviders.nix
            ./_vendor/modules/nfsTargets.nix
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

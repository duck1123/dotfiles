{ self, ... }:
{
  flake.lib.loadSecrets =
    { pkgs }:
    let
      decryptedPath = builtins.getEnv "DECRYPTED_SECRET_FILE";
    in
    (
      if (decryptedPath != "" && builtins.pathExists decryptedPath) then
        self.lib.fromYAML {
          inherit pkgs;
          value = builtins.readFile decryptedPath;
        }
      else
        throw ''
          Secrets are only supported from the encrypted file. Set DECRYPTED_SECRET_FILE by running
          commands via:  ./scripts/with-decrypted-secrets.sh <command>
          or by setting DECRYPTED_SECRET_FILE to the path of a decrypted copy of secrets.enc.yaml.''
    );
}

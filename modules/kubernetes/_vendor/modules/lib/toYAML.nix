{ ... }:
{
  # because I want yaml, not "um, json is valid yaml actually"
  flake.lib.toYAML =
    { pkgs, value }:
    let
      json-file = builtins.toFile "input.json" (builtins.toJSON value);
    in
    builtins.readFile (
      pkgs.runCommand "convert-values-yaml"
        {
          nativeBuildInputs = [ pkgs.yq ];
        }
        ''
          cat ${json-file} | yq -y . > $out
        ''
    );
}

{ ... }:
{
  flake.lib.fromYAML =
    { pkgs, value }:
    let
      yaml-file = builtins.toFile "input.yaml" value;
    in
    builtins.fromJSON (
      builtins.readFile (
        pkgs.runCommand "convert-from-yaml" { nativeBuildInputs = [ pkgs.yq ]; } ''
          cat ${yaml-file} | yq . > $out
        ''
      )
    );
}

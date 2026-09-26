{
  inputs,
  pkgs,
  system,
  ...
}:
let
  inherit (inputs.nixidy.packages.${system}.generators) fromCRD;
in
fromCRD {
  name = "tailscale";
  src = pkgs.fetchFromGitHub {
    owner = "tailscale";
    repo = "tailscale";
    rev = "v1.88.2";
    hash = "sha256-b1o3UHotVs5/+cpMx9q8bvt6BSM2QamLDUNyBNfb58A=";
  };
  crdFiles = [ "cmd/k8s-operator/deploy/crds/tailscale.com_proxyclasses.yaml" ];
}

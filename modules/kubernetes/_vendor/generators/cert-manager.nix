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
  name = "cert-manager";
  src = pkgs.fetchFromGitHub {
    owner = "cert-manager";
    repo = "cert-manager";
    rev = "v1.17.1";
    hash = "sha256-cp4y4NULf2e9lwwO4OiAbUNfXkE20ptnSTZjOMmFKgM=";
  };
  crdFiles = [
    "deploy/crds/crd-certificaterequests.yaml"
    "deploy/crds/crd-certificates.yaml"
    "deploy/crds/crd-challenges.yaml"
    "deploy/crds/crd-clusterissuers.yaml"
    "deploy/crds/crd-issuers.yaml"
    "deploy/crds/crd-orders.yaml"
  ];
}

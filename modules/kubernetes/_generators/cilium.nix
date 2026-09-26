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
  name = "cilium";
  src = pkgs.fetchFromGitHub {
    owner = "cilium";
    repo = "cilium";
    rev = "v1.15.6";
    hash = "sha256-oC6pjtiS8HvqzzRQsE+2bm6JP7Y3cbupXxCKSvP6/kU=";
  };
  crdFiles = [
    "pkg/k8s/apis/cilium.io/client/crds/v2/ciliumnetworkpolicies.yaml"
    "pkg/k8s/apis/cilium.io/client/crds/v2/ciliumclusterwidenetworkpolicies.yaml"
  ];
}

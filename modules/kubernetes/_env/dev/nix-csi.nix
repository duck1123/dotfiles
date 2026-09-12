{ ... }:
{
  services.nix-csi.enable = true;
  services.nix-csi.cache.storageClassName = "longhorn";
  services.nix-csi.authorizedKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDW6736YDTONCvxi0JKBXpQ2XNHnUIv1yA8XDDtKrTRp duck@powerspecnix"
  ];
}

{ config, secrets, ... }:
{
  services.n8n = {
    enable = false;

    hostAffinity = "nasnix";

    encryptionKey = (secrets.n8n or { }).encryptionKey or "";

    ingressProvider = "traefik-lan";
  };
}

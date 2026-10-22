{ config, secrets, ... }:
{
  services.n8n = {
    hostAffinity = "nasnix";

    encryptionKey = (secrets.n8n or { }).encryptionKey or "";

    ingressProvider = "traefik-lan";
  };
}

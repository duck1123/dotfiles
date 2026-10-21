{ secrets, ... }:
{
  services.trilium = {
    enable = true;
    apiKey = secrets.trilium.key;
    claudeCode = {
      enable = true;
      oauthToken = secrets.trilium.claudeToken;
    };
    backendScripting.enable = true;
    sqlConsole.enable = true;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Organization";

    storageClassName = "longhorn";

  };
}

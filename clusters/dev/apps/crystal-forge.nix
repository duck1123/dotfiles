_: {
  # Proxy to the Crystal Forge server on nixmini (features/crystal-forge.nix).
  services.crystal-forge = {
    ingressProvider = "traefik-lan";
    upstream.host = "192.168.0.25";
    homepage.group = "Apps";
  };
}

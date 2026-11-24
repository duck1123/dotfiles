_: {
  # Proxy to the Crystal Forge server on nixmini (features/crystal-forge.nix).
  services.crystal-forge = {
    ingressProvider = "traefik-lan";
    upstream.host = "192.168.0.25";
    # Same host as upstream.host; see trustedProxyCidrs in
    # features/crystal-forge.nix.
    hostAffinity = "nixmini";
    homepage.group = "Apps";
  };
}

_: {
  # Storybook for the shared dashboard UI components (resources/apps/dashboard-ui/).
  # Local, unpushed changes: `nur ui dev` instead.
  services.storybook = {
    enable = true;
    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage = {
      group = "Apps";
      description = "UI components for the review dashboard";
    };
  };
}

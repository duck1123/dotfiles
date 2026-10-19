{
  sims4 = {
    profiles = {
      default.mods = [
        "acme/core"
        "acme/widgets"
        "acme/variants"
        "zed/loose"
      ];
      vanilla = { };
      lite = {
        base = "default";
        remove = [ "zed/loose" ];
      };
    };

    mods.acme = {
      variants.variant = "lite";
      # a private override of a public scalar
      core.folder = "AcmeCore";
    };
  };
}

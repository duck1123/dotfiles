# Homepage Dashboard

`applications/homepage.nix` (the [gethomepage/homepage](https://gethomepage.dev) dashboard) auto-discovers a tile for every `mkArgoApp` service that both `enable`s itself and has `homepage.enable` (default: on whenever `uses-ingress = true`). Widgets and dashboard groups have their own patterns worth knowing before adding a new one.

## Widget secrets: never put an API key in the ConfigMap

Homepage's `services.yaml`/`widgets.yaml`/etc. are rendered into a plain `ConfigMap` (`homepage-config`) — anything written there is plaintext, in git. Many "advanced" widgets (Sonarr, Immich, Plex, ...) need an API key embedded right in their `widget:` block, which would otherwise mean committing a secret in the clear.

The fix uses homepage's own built-in secret-substitution syntax instead of inventing anything new: homepage resolves `{{HOMEPAGE_VAR_<NAME>}}` placeholders in its config files from container environment variables at render time (see [gethomepage's secrets docs](https://gethomepage.dev/configs/secrets/)). So:

- `services.homepage.widgetSecrets` (an `attrsOf str`) is a sops-encrypted Kubernetes Secret (`homepage-widget-secrets`, via the same `sopsSecrets`/`write-sops-secrets.sh` pipeline every other app's secrets use — see the main CLAUDE.md's Secrets section) whose keys are injected into the homepage container as `HOMEPAGE_VAR_<KEY>` env vars (`applications/homepage.nix`).
- Anywhere in `settings`/`widgets`/`extraGroups`/`bookmarkGroups`/a service's `homepage.extraSettings`, write the literal string `"{{HOMEPAGE_VAR_<KEY>}}"` instead of a real value.
- Wire the actual value in `env/dev/homepage.nix`'s `widgetSecrets`, sourced from `secrets.enc.yaml`.

**Never** route a secret value through `self.lib.toYAML` — it round-trips through the Nix store via `builtins.toFile`/`pkgs.runCommand`, which would leave the plaintext world-readable in `/nix/store` regardless of what ends up in git. `toYAML` is fine (and used) for the non-secret parts of homepage's config; just don't let a `widgetSecrets` value anywhere near it.

## Auto-populating a service's own widget

Rather than hand-writing each widget in `env/dev/homepage.nix`, the established pattern (see `applications/immich.nix`, `applications/sonarr.nix`, etc.) is: give the app itself an `apiKey` (or, for immich, `adminApiKey`) option that auto-populates its own `homepage.extraSettings.widget` once set:

```nix
# applications/<name>.nix
apiKey = mkOption {
  type = types.str;
  default = "";
};

homepage.extraSettings = mkOption {
  default = lib.optionalAttrs (cfg.apiKey != "") {
    widget = {
      type = "<homepage-widget-type>";
      url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
      key = "{{HOMEPAGE_VAR_<NAME>_API_KEY}}";
    };
  };
};
```

This needs `cfg = config.services.<name>;` bound in the file's outer `let` (some apps already have a `let` block for a `password-secret` constant — just add `cfg` alongside it) since `extraOptions` is a plain attrset, not a `cfg: ...` function, so it can't otherwise see the app's own resolved config.

Then in `env/dev/<name>.nix`: `apiKey = secrets.<name>.key;`, and in `env/dev/homepage.nix`'s `widgetSecrets`: `<NAME>_API_KEY = config.services.<name>.apiKey;`.

Check [gethomepage's widget docs](https://gethomepage.dev/widgets/services/) for the exact `type`/field names and any `version` field before assuming a widget just needs `type`/`url`/`key` — several widgets don't map 1:1 to their app's own name (Stash's widget `type` is `"stash"`, not `"stashapp"`) and some need an explicit API-version field once the backing app crosses a version threshold (Immich's widget needs `version = 2` for Immich >= v1.118; Komga's needs it for Komga v2+; Glances needs `version = 4`) — the 404s just look like a broken URL/key if you don't know to check this.

## Out-of-cluster services (not a `mkArgoApp` service)

Some dashboard entries aren't `mkArgoApp` services at all — e.g. Plex (`services.plex` via NixOS, on `edgenix`) and per-node Glances (`features.glances` in dotfiles, one instance per cluster host). These can't auto-discover, so they're added as static entries directly in `env/dev/homepage.nix`'s `extraGroups`, keyed by static LAN IP:

```nix
extraGroups.Media.plex = {
  href = "http://192.168.0.22:32400/web";
  widget = {
    type = "plex";
    url = "http://192.168.0.22:32400";
    key = "{{HOMEPAGE_VAR_PLEX_API_KEY}}";
  };
};
```

Still goes through `widgetSecrets` the same way for the key.

## Dashboard group registry (`config.homepageGroups`)

`services.<name>.homepage.group` (default `"Apps"`) is validated against `config.homepageGroups` (`modules/homepageGroups.nix`, populated in `env/dev.nix`) via `types.enum` — an unrecognized group name fails the build with a clear "not of type" error rather than silently creating a stray one-off group. Adding a new group means adding it to the `homepageGroups` list in `env/dev.nix` *first*, then referencing it from an app's `homepage.group`.

That same list's **order also controls render order** in `services.yaml` (`applications/homepage.nix` sorts groups by registry position, not the alphabetical order Nix attrsets would otherwise iterate in) — earlier entries appear first, which today (no multi-column `layout` configured in `settings.yaml`) is the only lever for "this group should be near the top/left." If per-group column placement is ever needed, `modules/homepageGroups.nix`/`config.homepageGroups` is the natural place to extend (e.g. turning each entry into `{ column = ...; }`) without touching the enum-validation mechanism — see [gethomepage's layout docs](https://gethomepage.dev/configs/settings/#layout) for what real multi-column placement requires (nested groups in `services.yaml` itself, not just ordering).

Adding a brand-new registry option like `homepageGroups` requires wiring it into `modules/nixidyEnvs.nix`'s explicit module list — see [nixidy-module-system.md](nixidy-module-system.md).

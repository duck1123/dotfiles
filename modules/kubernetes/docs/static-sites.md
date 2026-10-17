# Serving a Nix-Built Static Site

How to put a static site (an npm/Vite build, Storybook, docs) on the cluster with no container image, using the store-path pattern from [architecture.md](architecture.md). Worked example: `storybook`, which serves the dashboard UI components' Storybook at `https://storybook.home.kronkltd.net`. `duck1123` is the same pattern behind a Cloudflare Tunnel instead of an ingress.

## 1. The source

An npm project under `resources/apps/<site>/` with a `package-lock.json` and a script that builds to a folder (`resources/apps/dashboard-ui/`: `npm run build-storybook` → `storybook-static/`). Give it a `.gitignore` for `node_modules/` and the build folder, then `git add` it.

## 2. The flake packages

A file in `modules/kubernetes/pkgs/` (auto-imported). See `dashboard-ui.nix`:

- **The site:** `pkgs.buildNpmPackage` with `npmDepsHash`, `npmBuildScript` set to the build script, and an `installPhase` copying the build folder to `$out`. Turn on `doCheck` with a `checkPhase` running the tests, so a broken build fails before it deploys.
- **The runtime:** a `symlinkJoin` of `pkgs.python3` and the site placed under `share/<site>`. Keep it under `share/` so the server doesn't also expose python's `lib/`.

Get the hash with `nix run nixpkgs#prefetch-npm-deps -- package-lock.json`. For dashboard-ui, `nur ui hash` does that and rewrites the file. A wrong hash fails the build with the right one in the error.

## 3. The app template

`appTemplates/<site>.nix` with `uses-ingress = true` and `extraResources` for a Deployment, a Service and an Ingress (copy `appTemplates/storybook.nix`):

- image `ghcr.io/lillecarl/nix-csi/scratch:1.0.1`, command `python3 -m http.server 8080 --directory /nix/var/result/share/<site>`;
- a `nixkube` CSI volume with `volumeAttributes."x86_64-linux" = "${self.packages.x86_64-linux.<runtime>}"`, mounted at `/nix` with `subPath = "nix"`;
- the Ingress needs `metadata.annotations."cert-manager.io/cluster-issuer" = cfg.ingress.clusterIssuer` and a `tls` `secretName`. Without the annotation cert-manager never issues a certificate and Traefik serves its self-signed default.

## 4. The cluster entry

`clusters/dev/apps/<site>.nix`:

```nix
_: {
  services.<site> = {
    enable = true;
    ingressProvider = "traefik-lan";   # LAN only; sets class, issuer, <site>.home.kronkltd.net
    ingress.tls.enable = true;
    homepage.group = "Apps";
  };
}
```

## 5. Check, then deploy

- `nix build .#<runtime>`, then serve it locally the way the pod will: `<out>/bin/python3 -m http.server 6108 --directory <out>/share/<site>`.
- The rendered objects can be checked without real secrets, since apps that don't use any never read them: `DECRYPTED_SECRET_FILE=<file containing {}> nix eval --impure --json .#nixidyEnvs.x86_64-linux.dev.config.applications.<site>.objects`.
- `nur switch k8s` builds the runtime, pushes it to Attic and deploys. A content change only changes the store path, so every later `nur switch k8s` rolls the pod onto the new build.

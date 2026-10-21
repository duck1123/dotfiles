# Architecture

How the flake is put together: where modules live, and how hosts, features, environments and the other registries turn into `nixosConfigurations` and `homeConfigurations`. For the Kubernetes side, see [modules/kubernetes/docs/architecture.md](../modules/kubernetes/docs/architecture.md).

## Module organization

Every file under `modules/` is a flake-parts module, auto-imported by [import-tree](https://github.com/vic/import-tree). The whole of `flake.nix`'s `outputs` is:

```nix
outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);
```

Top-level registry directories, auto-loaded (see [Registries](#registries)):

- `hosts/`: one `<hostname>.nix` per host (see [Hosts](#hosts))
- `features/`: one `<name>.nix` per feature that hosts toggle (see [Features](#features))
- `environments/`: one `<name>.nix` per desktop environment (budgie, gnome, hyprland, i3, niri, plasma6, plus the text-only `headless`) (see [Environments](#environments))
- `identities/`: per-user identities (duck, deck, drenfer)
- `appTemplates/`: one `<name>.nix` per Kubernetes app, holding the spec lambda passed to `mkArgoApp`

Inside `modules/`:

- `flake/`: the flake outputs (`nixosConfigurations`, `homeConfigurations`, `devShells`, `packages`), the registry definitions (`registries.nix`, `hosts.nix`, `features.nix`, `environments.nix`, `identities.nix`, `appTemplates.nix`, `nested-modules.nix`) and `lib/+mk-os.nix`
- `nixos/`: plain NixOS modules (`boot`, `i18n`, `users`, `sddm`, `nix-attic`, `wsl`, ...)
- `options/`: option declarations shared across classes (`host`, `hosts`, `identities`, the `simpleFeature` type)
- `types/submodules/`: the submodule types for hosts, identities and features
- `base.nix`: the `homeManager.base` / `nixos.base` entry modules; `state-version.nix`: state versions
- `kubernetes/`: the cluster integration. `lib/` holds `mkArgoApp`, `mkPinnedVolume` and the other `flake.lib.*` helpers, `pkgs/` the packages some apps use, `applications.nix` turns every `appTemplates` entry into a `flake.nixidyApps` module, and `nixidy-apps/` holds the few apps that don't use `mkArgoApp`. The `_`-prefixed dirs (`_nixidy/`, `_generators/`) aren't flake-parts modules. The `dev` nixidy environment itself lives in `clusters/dev/`: `default.nix` for cluster-wide settings, `options.nix` for `devDefaults.*`, and `apps/<name>.nix` for per-app instance config.

## Registries

Some core types live in top-level directories and are auto-loaded by `modules/flake/registries.nix`. Every `<dir>/<name>.nix` becomes a flake-parts definition of `<attr>.<name>`, so adding an entry means dropping in a file. Files starting with `_` are skipped.

> **The flake only sees git-tracked files.** `git add` a new file (staging is enough), or Nix silently acts as if it doesn't exist (`does not provide attribute ...`, no other error).

A file holds only the entry's body: a plain value, or a function of the *flake-parts* module args (`config`, `lib`, `inputs`, ...) when it needs them, e.g. to reference other entries. NixOS/home-manager args belong to the nested `nixos`/`homeManager` bodies. Relative paths are relative to the registry directory (e.g. `../resources/...`).

```nix
# identities/deck.nix
{ config, ... }: {
  inherit (config.identities.duck) email gpgKey name;   # flake-parts config
  username = "deck";
}
```

Each registry's option is declared in `modules/flake/<attr>.nix` and evaluated once at the flake level. To add a registry, add a `<attr> = ../../<dir>;` line to the table in `registries.nix` (to the `raw = true` table if its entries are themselves functions), then declare the option.

- `identities/`: published as `inputs.self.identities` and exposed read-only to generic/NixOS/home-manager modules as `config.identities` (`modules/options/identities-options.nix`). Hosts pick one with `identity = config.identities.<name>`.
- `appTemplates/`: a **raw** registry. Each file is the lambda `{ config, lib, pkgs, self, ... }: { name = ...; chart = ...; ... }` (nixidy module args to `mkArgoApp` spec), kept as-is rather than called with flake-parts args. `modules/kubernetes/applications.nix` wraps each one as `self.lib.mkArgoApp { inherit config lib pkgs self; } (template args)` into `flake.nixidyApps.<name>`. App-owned assets (dashboards, scripts, site sources) live in `resources/apps/<name>/`.

## Hosts

Each host is one file, `hosts/<hostname>.nix`, in the `hosts` registry (`modules/flake/hosts.nix`). It holds the host's data (the `hostSubmodule` options) plus its own modules:

```nix
# hosts/edgenix.nix
{ config, ... }:            # flake-parts args, for config.identities
{
  system = "x86_64-linux";  # hostname defaults to the file name, name to hostname
  identity = config.identities.duck;
  id = "...";               # Syncthing device ID
  environments.primary = "plasma6";
  features = { git.enable = true; ... };
  nixos.enable = true;      # build nixosConfigurations.<hostname>

  modules.homeManager = { pkgs, ... }: { home.packages = [ pkgs.guake ]; };
  modules.nixos = { config, lib, modulesPath, ... }: { ... };  # hardware, boot, imports nixos.base
  # homeConfigurationName = "user@HOST";  # default "<identity.username>@<hostname>"
}
```

From that the registry generates:

- `inputs.self.hosts.<hostname>`: the data, evaluated once, exposed read-only to generic/NixOS/home-manager modules as `config.hosts` (`modules/options/hosts-options.nix`)
- `modules.homeManager.<hostname>` / `modules.nixos.<hostname>`, published unwrapped so merge order matches a hand-written module
- `homeConfigurations.<homeConfigurationName>` for every host with `modules.homeManager` (`[ base <hostname> ]`, plus `host = config.hosts.<hostname>`)
- `nixosConfigurations.<hostname>` for every host with `modules.nixos` and `nixos.enable`, via `mkNixos` from `modules/flake/lib/+mk-os.nix`, plus `host = config.hosts.<hostname>`. A WSL host imports `inputs.self.modules.nixos.wsl` from its own `modules.nixos` (see `vavirl-pw0bwnq8`).

Host modules don't set `host` themselves; anything else that imports `modules.<class>.<hostname>` has to.

### Adding a host

Create `hosts/<hostname>.nix` (copy an existing one) and `git add` it; nothing else needs registering.

- `modules.nixos` + `nixos.enable = true` gives it a `nixosConfigurations` entry.
- `modules.homeManager` (even `{ }`, see `steamdeck`) gives it a `homeConfigurations` entry.
- A host Nix doesn't build at all (e.g. `pixel8`, an Android phone tracked only for feature flags and Syncthing) sets neither.

`nur` picks the host up automatically (see [nur_commands.md](nur_commands.md#hosts-and---host)). If its sops-nix secrets should decrypt there, add its host key as a recipient too (see [secrets.md](secrets.md#adding-a-recipient)).

### Host info (`hosts.json`)

Each host also has an `info` record: a JSON-friendly summary (hostname, name, system, user, nixos/android, environments, enabled `features`, `sshHost`, and the `homeConfiguration`/`nixosConfiguration` attribute names or null). Its fields default to `defaultInfo` in `modules/flake/hosts.nix` (change that to record something for every host). A host file can override or add fields with `info.<field> = ...;` (never paths or functions). A feature counts as enabled if its `enable`, or any direct sub-option's `enable` (e.g. `kubernetes.server.enable`), is true.

- `nix eval --json .#hostInfo`: all hosts' info from the working tree (what `scripts/nur.nu` uses)
- `~/.config/dotfiles/hosts.json`: `{ current, hosts: [info ...] }`, written on every host by `modules.homeManager.host-info` (imported by `homeManager.base`). It reflects the last home-manager switch and is meant as a data source for other tools (e.g. Look).
- `nushell/modules/hosts_module.nu` reads that file: `hosts list`, `hosts get [host]`, `hosts features [host]` (defaulting to the current host).

## Features

A host toggles features in its file with `features.<name>.enable` (the option is `hosts.<hostname>.features.<name>`).

Each feature is declared **once**, in `features/<name>.nix`, which holds the body of `features.<name>` in the registry defined in `modules/flake/features.nix`. That single declaration generates:

- the `hosts.<host>.features.<name>` option (via `simpleFeatureWith` in `modules/options/simpleFeature.nix`)
- `modules.homeManager.features.<name>` and `modules.nixos.features.<name>`, each wrapped in `mkIf hosts.<host>.features.<name>.enable`
- their inclusion in `homeManager.base` / `nixos.base`, which import every registered feature (there's no list to edit)

```nix
# features/vim.nix
{
  # description = "...";            # optional; defaults to "<name> feature"
  homeManager = { pkgs, ... }: { home.packages = [ pkgs.neovim ]; };  # body only: no `config =` / `mkIf`
  nixos = _: { programs.vim.enable = true; };                         # optional, either class may be omitted
}
```

Registry knobs beyond the two bodies (a body may also be a list of bodies):

- `extraOptions = { inputs, lib }: { foo = mkOption ...; };` adds options next to `enable` (see `tailscale`, `nix`, `hyprland`)
- `option = { inputs, lib }: mkOption ...;` replaces the whole generated option with a custom type (see `media`, `kubernetes`)
- `gated = false` skips the automatic `enable` gate, for a feature with no plain `enable` or one that gates differently per class. Its bodies then return their own `mkIf`/`mkMerge` (see `media`, `kubernetes`).

Gotchas:

- flake-parts types `flake.modules.<class>.<name>` as a `deferredModule`, so `features.<name>` can't be nested there. The registry sets `nestedModules.<class>.features` (`modules/flake/nested-modules.nix`), which is written into the *published* flake output through `touchup.attr.modules.finish`. So it's visible as `inputs.self.modules.<class>.features.<name>` but **not** as `config.flake.modules` inside flake-parts modules. `finish` only takes one definition, so any other nested group must go through `nestedModules` too.
- A body that sets options that only exist on some hosts (e.g. `sops.*`) can't live in a feature that WSL also imports, because `mkIf false` still errors on an undeclared option. That's why `modules/nixos/nix-attic.nix` stays a standalone `modules.nixos.nix-attic` imported directly by `nixos.base`.
- Modules that aren't features (`state-version`, `boot`, `i18n`, `nix-attic`, `sddm`, `specialisations`, `users`) are plain `flake.modules.<class>.<name>` entries, listed explicitly in `modules/base.nix`.

## Environments

Desktop environments work like features. Each is declared once in `environments/<name>.nix`, which holds the body of `environments.<name>` in the registry in `modules/flake/environments.nix`:

```nix
# environments/niri.nix
{
  features = [ "wayle" ];      # features turned on for hosts using this environment
  desktopNames = [ "niri" ];   # XDG_CURRENT_DESKTOP of the session
  nixos = { pkgs, ... }: { programs.niri.enable = true; };  # body only, like features
  # homeManager = ...;                                      # optional
}
```

A host picks its environments in its `hosts/<hostname>.nix`:

```nix
environments = {
  primary = "hyprland";   # what the host boots into by default (null = none)
  gnome.enable = true;    # every other enabled environment becomes a specialisation
  plasma6.enable = true;
};
```

How it fits together:

- `nixos.specialisations` (defined in `modules/flake/environments.nix`, imported by `nixos.base`) imports every `modules.nixos.environments.<name>` and declares `environments.active`, which defaults to `primary`. Each NixOS body is gated on `environments.active == <name>`.
- Each enabled non-primary environment becomes `specialisation.<name>` with `inheritParentConfig = true` and only `environments.active` forced to `<name>`, so host files carry no specialisation plumbing. nixpkgs drops nested specialisations itself.
- home-manager bodies are shared across specialisations, so they're gated on the host using the environment at all (`primary` or `.enable`).
- A body's `imports` are hoisted out of the gate, since imports can't be conditional. For example, niri imports the Look NixOS module unconditionally; it only acts when `programs.lookapp.enable` is set.
- An environment's `features` are set with `mkDefault true` on every host that uses it (primary or enabled), via `modules.generic.environments-host` in the host submodule. They stay ordinary features, so several environments can share one (hyprland and niri both pull in `wayle`), and a host can still set one directly or turn it off. Don't enable environment-owned features (`hyprland`, `gnome`, `i3`, `wayle`) by hand in host files.
- Because home-manager is shared across specialisations, a feature that runs a service should limit it at runtime to the environments that asked for it. `inputs.self.lib.environments.desktopsFor config.host "<feature>"` returns their `desktopNames`. See `wayle`, which uses them as `ConditionEnvironment=|XDG_CURRENT_DESKTOP=...` so it doesn't start under Plasma.
- `primary` is reserved and can't be used as an environment name.
- `headless` is the text-only environment, for servers like the k3s nodes. It doesn't remove GUI features from the host. While it's active it forces their services off (xserver, sddm, PipeWire, Bluetooth, XDG portals) and boots to `multi-user.target`. So `primary = "headless"; plasma6.enable = true;` boots to a console and keeps Plasma as a boot-menu entry, or switch to it live with `sudo /run/current-system/specialisation/plasma6/bin/switch-to-configuration switch`.
- Anything consumed as a module (like `environments-host`) must be published under `flake.modules`, not `flake.types.generic`. That option is typed `anything`, which wraps function values and forces config-dependent definitions too early (infinite recursion).

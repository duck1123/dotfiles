// The data model of the Sims mod browser: what `sims mod list` returns (simsnix,
// nushell/modules/sims_module.nu). Components get a ModSource, so they run
// against a mock (Storybook, tests) or the local `sims web` server.

/** One file of a mod version, as downloaded. */
export interface ModFile {
  name: string;
  /** Files inside it, for an archive; 0 for a loose mod file. */
  members: number;
}

export interface ModVersion {
  /** When the version was recorded (YYYY-MM-DD). */
  added: string | null;
  gameVersion: string | null;
  files: ModFile[];
}

export interface ModSummary {
  /** `creator/mod`. */
  id: string;
  title: string | null;
  downloadPage: string | null;
  tags: string[];
  /** Whether the profile the list was made for installs it. */
  enable: boolean;
  /** Folder under Mods/ it installs to; null = directly in Mods/. */
  folder: string | null;
  /** The pinned version label; null = the newest. */
  version: string | null;
  /** The version that installs. */
  selectedVersion: string | null;
  requires: string[];
  conflicts: string[];
  /** Known versions, by label. */
  versions: Record<string, ModVersion>;
  /** Defined in JSON, so its fields can be edited (a Nix-defined mod can't). */
  editable: boolean;
  /** Profiles that list it directly. */
  profiles: string[];
  /** The catalog's problems that name it. */
  problems: string[];
}

export interface ModList {
  /** The profile `enable` was computed for. */
  profile: string;
  profiles: string[];
  /** Every problem the catalog has, including those naming no mod. */
  problems: string[];
  mods: ModSummary[];
}

export interface ModSource {
  /** The catalog as `profile` sees it (default: the last one switched to). */
  list(profile?: string, signal?: AbortSignal): Promise<ModList>;
}

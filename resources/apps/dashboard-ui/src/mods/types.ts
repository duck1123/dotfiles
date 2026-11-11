// The data model of the Sims mod browser: what `sims mod list` returns (simsnix,
// nushell/modules/sims_module.nu). Components get a ModSource, so they run
// against a mock (Storybook, tests) or the local `sims web` server.

/** One file of a mod version, as downloaded. */
export interface ModFile {
  name: string;
  /** SHA-256 of the file (hex), the name of its blob. */
  sha256: string;
  /** Files inside it, for an archive; empty for a loose mod file. */
  members: ModMember[];
}

/** A file inside an archive. */
export interface ModMember {
  /** Its path inside the archive. */
  path: string;
  sha256: string;
}

export interface ModVersion {
  /** When the version was recorded (YYYY-MM-DD). */
  added: string | null;
  gameVersion: string | null;
  files: ModFile[];
}

/** A file the mod's selected version installs. */
export interface ModInstall {
  /** Its path before `folder` and `flatten` apply (an archive member's after stripComponents). */
  source: string;
  /** Where it goes, relative to Mods/. */
  target: string;
  sha256: string;
  /** For an archive member: the archive's sha256 and the member's path inside it; null (or absent) for a loose file. */
  archive?: string | null;
  member?: string | null;
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
  /** The folder it gets when none is set: `<creator>_<mod>`. */
  defaultFolder: string;
  /** Every file goes directly in `folder`, without the path it was imported at. */
  flatten: boolean;
  /** Globs, relative to its folder, of files the mod writes. */
  stateFiles: string[];
  /** The files its selected version installs, and where. */
  installs: ModInstall[];
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

/** Catalog fields to change; a field left out stays as it is, null clears it. */
export interface ModChanges {
  downloadPage?: string | null;
  title?: string | null;
  tags?: string[];
  /** Where it installs: "default" for `<creator>_<mod>`, "" for Mods/ itself, else a folder under Mods/. */
  folder?: string;
  flatten?: boolean;
  /** Pin this version label; "" unpins it, so the newest installs. */
  version?: string;
}

export interface WriteResult {
  /** Short hash of the commit the change made, or null when nothing changed. */
  commit: string | null;
  /** Things to know about the change (e.g. that files move at the next switch). */
  notes?: string[];
}

/** What `sims mod rename` does (or, before `apply`, would do). */
export interface RenameResult {
  from: string;
  to: string;
  /** Catalog files moved, "old -> new". */
  moved: string[];
  /** The install folder changes too, for a mod on its default folder. */
  folder: { from: string; to: string } | null;
  /** Files whose references to the id are rewritten. */
  rewritten: { file: string; refs: number }[];
  /** Other mentions of the id, which aren't changed. */
  mentions: { file: string; line: number; text: string }[];
  applied: boolean;
  commit?: string | null;
}

export interface ModSource {
  /** The catalog as `profile` sees it (default: the last one switched to). */
  list(profile?: string, signal?: AbortSignal): Promise<ModList>;
  /** Change a mod's catalog fields. A source without it is read-only. */
  update?(id: string, changes: ModChanges): Promise<WriteResult>;
  /** Rename a mod (its id); without `apply`, only say what would change. */
  rename?(id: string, to: string, apply: boolean): Promise<RenameResult>;
  /** Add a mod to a profile or take it out. */
  setInProfile?(profile: string, id: string, enabled: boolean): Promise<WriteResult>;
}

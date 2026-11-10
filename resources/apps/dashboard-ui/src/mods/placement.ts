// Where a mod's files install, for previewing a new folder or flatten before it's
// saved. Mirrors manifest.nix in simsnix (modules/sims4/_simsnix).
import type { ModInstall } from "./types";

export interface Placement {
  /** Folder under Mods/; null = Mods/ itself. */
  folder: string | null;
  flatten: boolean;
}

export interface PlannedFile {
  source: string;
  sha256: string;
  from: string;
  to: string;
}

const baseName = (path: string) => path.slice(path.lastIndexOf("/") + 1);

/** Where a file at `source` goes under `placement`. */
export function placeTarget(source: string, { folder, flatten }: Placement): string {
  const path = flatten ? baseName(source) : source;
  return folder === null ? path : `${folder}/${path}`;
}

/** Each file's current and new target; a file the new placement would install twice (same target and hash) once. */
export function planPlacement(installs: ModInstall[], placement: Placement): PlannedFile[] {
  const seen = new Set<string>();
  const planned: PlannedFile[] = [];
  for (const f of installs) {
    const to = placeTarget(f.source, placement);
    const key = `${to.toLowerCase()}\t${f.sha256}`;
    if (seen.has(key)) continue;
    seen.add(key);
    planned.push({ source: f.source, sha256: f.sha256, from: f.target, to });
  }
  return planned;
}

/**
 * Why a folder name can't be used, or null when it can. Like `sims mod set`: a path
 * relative to Mods/, without . or .. parts.
 */
export function folderProblem(folder: string): string | null {
  const f = folder.trim().replace(/^\/+|\/+$/g, "");
  if (!f) return "Give a folder name.";
  if (folder.trim().startsWith("/") || f.split("/").some((c) => c === "." || c === "..")) {
    return "A folder is a path inside Mods/, without . or .. parts.";
  }
  if (f.split("/").some((c) => c === "")) return "The folder has an empty part (//).";
  return null;
}

/** What the game won't load, and files that would land on the same path. Targets compare case-insensitively, as under Wine. */
export function placementProblems(planned: PlannedFile[]): string[] {
  const problems: string[] = [];
  const depth = (t: string) => t.split("/").length - 1;
  for (const f of planned) {
    const lower = f.to.toLowerCase();
    if (lower.endsWith(".ts4script") && depth(f.to) > 1) {
      problems.push(`${f.to}: .ts4script files load at most one folder below Mods/`);
    }
    if (lower.endsWith(".package") && depth(f.to) > 5) {
      problems.push(`${f.to}: .package files load at most five folders below Mods/`);
    }
  }
  const byTarget = new Map<string, PlannedFile[]>();
  for (const f of planned) {
    const k = f.to.toLowerCase();
    byTarget.set(k, [...(byTarget.get(k) ?? []), f]);
  }
  for (const files of byTarget.values()) {
    if (files.length > 1) {
      problems.push(`${files[0].to}: ${files.length} different files would install here (${files.map((f) => f.source).join(", ")})`);
    }
  }
  return problems;
}

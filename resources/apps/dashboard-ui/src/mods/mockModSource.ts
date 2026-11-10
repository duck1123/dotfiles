import { planPlacement } from "./placement";
import { sampleModList } from "./sampleMods";
import type { ModList, ModSource, RenameResult } from "./types";

export interface MockModOptions {
  list?: ModList;
  /** Milliseconds each call waits, to show loading states. */
  delay?: number;
  /** Make every call fail with this message. */
  fail?: string;
  /** Make only writes fail with this message. */
  failWrites?: string;
  /** Leave out update and setInProfile, as a read-only source. */
  readOnly?: boolean;
}

function wait(ms: number, signal?: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    const t = setTimeout(resolve, ms);
    signal?.addEventListener("abort", () => {
      clearTimeout(t);
      reject(new DOMException("Aborted", "AbortError"));
    });
  });
}

let commits = 0;
const fakeCommit = () => (0xabc0000 + ++commits).toString(16);

/** A ModSource over sample data, for stories and tests. Writes change its own copy. */
export function createMockModSource({
  list = sampleModList,
  delay = 0,
  fail,
  failWrites,
  readOnly = false,
}: MockModOptions = {}): ModSource {
  const state = structuredClone(list);

  async function write(): Promise<void> {
    await wait(delay);
    if (fail ?? failWrites) {
      throw new Error(fail ?? failWrites);
    }
  }

  function find(id: string) {
    const mod = state.mods.find((m) => m.id === id);
    if (!mod) {
      throw new Error(`${id} isn't in the catalog`);
    }
    return mod;
  }

  const source: ModSource = {
    async list(profile, signal) {
      await wait(delay, signal);
      if (fail) {
        throw new Error(fail);
      }
      if (profile && !state.profiles.includes(profile)) {
        throw new Error(`unknown profile ${profile}`);
      }
      // Only the profile changes here: the mock doesn't recompute `enable` for another one.
      return structuredClone({ ...state, profile: profile ?? state.profile });
    },
  };
  if (readOnly) {
    return source;
  }

  source.update = async (id, changes) => {
    await write();
    const mod = find(id);
    const before = JSON.stringify(mod);
    const { folder, flatten, ...fields } = changes;
    Object.assign(mod, fields);
    if (folder !== undefined) {
      mod.folder = folder === "default" ? mod.defaultFolder : folder.trim().replace(/^\/+|\/+$/g, "") || null;
    }
    if (flatten !== undefined) {
      mod.flatten = flatten;
    }
    const moved = folder !== undefined || flatten !== undefined;
    if (moved) {
      mod.installs = planPlacement(mod.installs, mod).map((f) => ({ source: f.source, sha256: f.sha256, target: f.to }));
    }
    const changed = JSON.stringify(mod) !== before;
    const notes = changed && moved ? ["the files move at the next `sims switch`"] : [];
    return { commit: changed ? fakeCommit() : null, notes };
  };
  source.rename = async (id, to, apply) => {
    await write();
    const mod = find(id);
    if (!/^[a-z0-9][a-z0-9-]*\/[a-z0-9][a-z0-9-]*$/.test(to)) {
      throw new Error(`${to}: an id is creator/mod, in lowercase letters, digits and -`);
    }
    if (state.mods.some((m) => m.id === to)) {
      throw new Error(`${to} is already in the catalog`);
    }
    const onDefault = mod.folder === mod.defaultFolder;
    const newDefault = to.replace("/", "_");
    const referrers = state.mods.filter((m) => m.requires.includes(id) || m.conflicts.includes(id));
    const result: RenameResult = {
      from: id,
      to,
      moved: [`mods/${id}.json -> mods/${to}.json`],
      folder: onDefault ? { from: mod.defaultFolder, to: newDefault } : null,
      rewritten: [
        ...mod.profiles.map((p) => ({ file: `profiles/${p}.json`, refs: 1 })),
        ...referrers.map((m) => ({ file: `mods/${m.id}.json`, refs: 1 })),
      ],
      mentions: [],
      applied: apply,
    };
    if (!apply) {
      return result;
    }
    for (const m of referrers) {
      m.requires = m.requires.map((r) => (r === id ? to : r));
      m.conflicts = m.conflicts.map((r) => (r === id ? to : r));
    }
    mod.id = to;
    mod.defaultFolder = newDefault;
    if (onDefault) {
      mod.folder = newDefault;
      mod.installs = planPlacement(mod.installs, mod).map((f) => ({ source: f.source, sha256: f.sha256, target: f.to }));
    }
    state.mods.sort((a, b) => a.id.localeCompare(b.id));
    return { ...result, commit: fakeCommit() };
  };
  source.setInProfile = async (profile, id, enabled) => {
    await write();
    const mod = find(id);
    const listed = mod.profiles.includes(profile);
    if (listed === enabled) {
      return { commit: null };
    }
    mod.profiles = enabled ? [...mod.profiles, profile].sort() : mod.profiles.filter((p) => p !== profile);
    if (profile === state.profile) {
      mod.enable = enabled;
    }
    return { commit: fakeCommit() };
  };
  return source;
}

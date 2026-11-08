import { sampleModList } from "./sampleMods";
import type { ModList, ModSource } from "./types";

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
    Object.assign(mod, changes);
    return { commit: JSON.stringify(mod) === before ? null : fakeCommit() };
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

import { sampleModList } from "./sampleMods";
import type { ModList, ModSource } from "./types";

export interface MockModOptions {
  list?: ModList;
  /** Milliseconds each call waits, to show loading states. */
  delay?: number;
  /** Make every call fail with this message. */
  fail?: string;
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

/** A ModSource over sample data, for stories and tests. */
export function createMockModSource({ list = sampleModList, delay = 0, fail }: MockModOptions = {}): ModSource {
  return {
    async list(profile, signal) {
      await wait(delay, signal);
      if (fail) {
        throw new Error(fail);
      }
      if (profile && !list.profiles.includes(profile)) {
        throw new Error(`unknown profile ${profile}`);
      }
      // Only the profile changes here: the mock doesn't recompute `enable`.
      return structuredClone({ ...list, profile: profile ?? list.profile });
    },
  };
}

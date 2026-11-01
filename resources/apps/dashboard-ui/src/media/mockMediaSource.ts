import { sampleFiles } from "./sampleFiles";
import type { MediaDecision, MediaFile, MediaSource, MediaView } from "./types";

export interface MockMediaOptions {
  files?: MediaFile[];
  latencyMs?: number;
  /** Reject list() with this message. */
  failList?: string;
  /** Reject decide() with this message. */
  failDecide?: string;
}

const wait = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

const STATUS: Record<MediaView, (file: MediaFile) => boolean> = {
  open: (f) => ["suspect", "unclear", "minor"].includes(f.status),
  replace: (f) => f.status === "replace_requested",
  good: (f) => f.status === "marked_good",
};

/** An in-memory MediaSource: decisions move files between the views. */
export function createMockMediaSource(options: MockMediaOptions = {}): MediaSource {
  let files = (options.files ?? sampleFiles).map((file) => ({ ...file }));
  const latencyMs = options.latencyMs ?? 0;

  function find(id: number): MediaFile {
    const file = files.find((f) => f.id === id);
    if (!file) {
      throw new Error(`no media file ${id}`);
    }
    return file;
  }

  return {
    async list(view: MediaView) {
      await wait(latencyMs);
      if (options.failList) {
        throw new Error(options.failList);
      }
      return files.filter(STATUS[view]).map(({ log: _log, ...rest }) => ({ ...rest }));
    },

    async get(id: number) {
      await wait(latencyMs);
      return { ...find(id) };
    },

    async decide(id: number, decision: MediaDecision) {
      await wait(latencyMs);
      if (options.failDecide) {
        throw new Error(options.failDecide);
      }
      const file = find(id);
      const status = decision === "good" ? "marked_good" : decision === "replace" ? "replace_requested" : (file.verdict ?? "suspect");
      const updated = { ...file, status };
      files = files.map((f) => (f.id === id ? updated : f));
      return updated;
    },
  };
}

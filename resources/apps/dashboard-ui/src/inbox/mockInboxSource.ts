import { sampleItems } from "./sampleItems";
import type { InboxItem, InboxSource, ReviewChanges } from "./types";

export interface MockInboxOptions {
  items?: InboxItem[];
  latencyMs?: number;
  /** Reject list() with this message. */
  failList?: string;
  /** Reject save() with this message. */
  failSave?: string;
}

const wait = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

/** An in-memory InboxSource: saves change the list it serves. */
export function createMockInboxSource(options: MockInboxOptions = {}): InboxSource {
  let items = (options.items ?? sampleItems).map((item) => ({ ...item }));
  const latencyMs = options.latencyMs ?? 0;

  function find(noteId: string): InboxItem {
    const item = items.find((i) => i.noteId === noteId);
    if (!item) {
      throw new Error(`no inbox item ${noteId}`);
    }
    return item;
  }

  return {
    async list() {
      await wait(latencyMs);
      if (options.failList) {
        throw new Error(options.failList);
      }
      return items.map(({ preview: _preview, ...rest }) => ({ ...rest }));
    },

    async get(noteId: string) {
      await wait(latencyMs);
      return { ...find(noteId) };
    },

    async save(noteId: string, changes: ReviewChanges) {
      await wait(latencyMs);
      if (options.failSave) {
        throw new Error(options.failSave);
      }
      const updated = { ...find(noteId), title: changes.title, kind: changes.kind, topicIds: changes.topicIds };
      items = changes.done ? items.filter((i) => i.noteId !== noteId) : items.map((i) => (i.noteId === noteId ? updated : i));
      return updated;
    },
  };
}

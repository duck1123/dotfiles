import { sampleItems } from "./sampleItems";
import type { InboxItem, InboxSource, ReviewChanges } from "./types";

export interface MockInboxOptions {
  items?: InboxItem[];
  latencyMs?: number;
  /** Reject list() with this message. */
  failList?: string;
  /** Reject save() with this message. */
  failSave?: string;
  /** Reject capture() with this message. */
  failCapture?: string;
}

const wait = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

/** An in-memory InboxSource: saves change the list it serves. */
export function createMockInboxSource(options: MockInboxOptions = {}): InboxSource {
  let items = (options.items ?? sampleItems).map((item) => ({ ...item }));
  const latencyMs = options.latencyMs ?? 0;
  let captured = 0;

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
      return items.map(({ preview: _preview, notes: _notes, ...rest }) => ({ ...rest }));
    },

    async get(noteId: string) {
      await wait(latencyMs);
      return { notes: "", ...find(noteId) };
    },

    async save(noteId: string, changes: ReviewChanges) {
      await wait(latencyMs);
      if (options.failSave) {
        throw new Error(options.failSave);
      }
      const item = find(noteId);
      const updated = { ...item, title: changes.title, kind: changes.kind, topicIds: changes.topicIds, notes: changes.notes ?? item.notes };
      items = changes.done ? items.filter((i) => i.noteId !== noteId) : items.map((i) => (i.noteId === noteId ? updated : i));
      return updated;
    },

    // Kind detection like the capture script's, without fetching page titles.
    async capture(text: string) {
      await wait(latencyMs);
      if (options.failCapture) {
        throw new Error(options.failCapture);
      }
      const prefix = text.trim().match(/^(task|watch|shopping):\s*/i);
      const rest = prefix ? text.trim().slice(prefix[0].length) : text.trim();
      const url = rest.match(/https?:\/\/\S+/)?.[0];
      const kind = prefix ? { task: "task", watch: "watchlist", shopping: "shopping" }[prefix[1].toLowerCase()] : url ? "link" : "journal";
      const item: InboxItem = {
        noteId: `mockCapture${++captured}`,
        title: rest.replace(url ?? "", "").trim().split("\n")[0] || url!,
        kind,
        created: new Date().toISOString().slice(0, 19),
        url,
        topicIds: [],
        tagIds: [],
        preview: text.trim(),
      };
      items = [...items, item];
      return { ...item };
    },
  };
}

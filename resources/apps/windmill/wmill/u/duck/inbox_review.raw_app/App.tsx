// Inbox review: work through the Trilium #inbox queue, setting kind, title and
// ~topic relations, then mark each item reviewed. The UI is dashboard-ui's
// InboxReview (see https://storybook.home.kronkltd.net); this file only wires
// it to the `trilium` and `capture` backend runnables.
import { InboxReview, type InboxSource, type NoteSource } from "dashboard-ui";
import "dashboard-ui/theme.css";
import "./index.css";
import { backend } from "./wmill";

const TRILIUM_URL = "https://trilium.home.kronkltd.net";
const TOPICS = { query: "note.ancestors.noteId = 969cd62d38c65e2acaae97e48a24165e", noun: "topic" };

function call<T>(action: string, args: Record<string, unknown> = {}): Promise<T> {
  return backend.trilium({ action, args }) as Promise<T>;
}

// The backend knows the Topics scope itself; the picker's scope is only used
// for its wording.
const notes: NoteSource = {
  search: (text, _scope, options) => call("search_topics", { text, limit: options?.limit ?? 20 }),
  getNotes: (noteIds) => call("get_notes", { noteIds }),
  noteUrl: (noteId) => `${TRILIUM_URL}/#root/${noteId}`,
};

const inbox: InboxSource = {
  list: () => call("list_inbox"),
  get: (noteId) => call("get_item", { noteId }),
  save: (noteId, changes) => call("save_item", { noteId, ...changes, apply: true }),
  // f/fleetops/capture (the phone share's script) creates the note; the queue
  // gets it back in the same shape as the others.
  capture: async (text) => {
    const { noteId } = (await backend.capture({ text })) as { noteId: string };
    return call("get_item", { noteId });
  },
};

// On a phone the page is mostly for capturing, so the capture box gets the
// focus rather than the first item's topic search.
const narrow = window.matchMedia("(max-width: 720px)").matches;

export default function App() {
  return (
    <div className="app">
      <InboxReview inbox={inbox} notes={notes} topicScope={TOPICS} focusCapture={narrow} />
    </div>
  );
}

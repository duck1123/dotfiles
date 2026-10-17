import type { NoteScope, NoteSource, NoteSummary, SearchOptions } from "./types";

// Made-up notes for stories and tests. Never put real note titles here: this
// package is in a public repo.
export const sampleNotes: NoteSummary[] = [
  { noteId: "mockTech0001", title: "Tech", icon: "bx bx-chip", path: ["Topics"] },
  { noteId: "mockFood0001", title: "Food", icon: "bx bx-bowl-hot", path: ["Topics"] },
  { noteId: "mockSci00001", title: "Science", icon: "bx bx-atom", path: ["Topics"] },
  { noteId: "mockHobby001", title: "Hobbies", icon: "bx bx-palette", path: ["Topics"] },
  { noteId: "mockRustLang", title: "Rust", icon: "bx bx-code-alt", path: ["Topics", "Tech", "Programming Languages"] },
  { noteId: "mockRustCorr", title: "Rust", icon: "bx bx-test-tube", path: ["Topics", "Science", "Chemistry"] },
  { noteId: "mockSourdgh1", title: "Sourdough", path: ["Topics", "Food", "Baking"] },
  { noteId: "mockBaking01", title: "Baking", path: ["Topics", "Food"] },
  { noteId: "mockGarden01", title: "Gardening", path: ["Topics", "Hobbies"] },
  { noteId: "mockTomato01", title: "Tomatoes", path: ["Topics", "Hobbies", "Gardening"] },
  { noteId: "mockKeybrd01", title: "Mechanical Keyboards", path: ["Topics", "Tech", "Hardware"] },
  { noteId: "mockOrigami1", title: "Origami", path: ["Topics", "Hobbies"] },
  {
    noteId: "mockLongT001",
    title: "A Deliberately Long Topic Title That Keeps Going Well Past Where Most Titles Would Stop",
    path: ["Topics", "Science", "Some Rather Long Bucket Name", "And A Sub-Bucket"],
  },
];

export interface MockSourceOptions {
  notes?: NoteSummary[];
  /** Delay before each call resolves, to show loading states. */
  latencyMs?: number;
  /** Reject every search with this message. */
  failWith?: string;
}

function delay(ms: number, signal?: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    if (signal?.aborted) {
      reject(signal.reason);
      return;
    }
    const timer = setTimeout(resolve, ms);
    signal?.addEventListener("abort", () => {
      clearTimeout(timer);
      reject(signal.reason);
    });
  });
}

/** A NoteSource over an in-memory list. The scope's query is ignored. */
export function createMockSource(options: MockSourceOptions = {}): NoteSource {
  const notes = options.notes ?? sampleNotes;
  const latencyMs = options.latencyMs ?? 0;

  return {
    async search(text: string, _scope: NoteScope, searchOptions: SearchOptions = {}) {
      await delay(latencyMs, searchOptions.signal);
      if (options.failWith) {
        throw new Error(options.failWith);
      }
      const needle = text.trim().toLowerCase();
      return notes
        .filter((note) => note.title.toLowerCase().includes(needle))
        .slice(0, searchOptions.limit ?? 20);
    },

    async getNotes(noteIds: string[]) {
      await delay(latencyMs);
      return noteIds.flatMap((id) => notes.filter((note) => note.noteId === id));
    },

    noteUrl(noteId: string) {
      return `#mock/${noteId}`;
    },
  };
}

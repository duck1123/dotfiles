// The data model the components work with. Components never talk to Trilium
// directly: they get a NoteSource, so the same component can run against a
// mock (Storybook, tests), a Windmill backend runnable (the dashboard) or
// Trilium's own frontend API (a widget).

/** One note, as much as a picker needs to show it. */
export interface NoteSummary {
  noteId: string;
  title: string;
  /** Trilium icon class, e.g. "bx bx-folder" (from #iconClass). */
  icon?: string;
  /** Titles of the note's ancestors, nearest last, for telling apart notes with the same title. */
  path: string[];
}

/** Which notes a picker may offer. */
export interface NoteScope {
  /**
   * A Trilium search expression every result must also match, such as
   * `note.ancestors.noteId = <Topics root>`.
   */
  query: string;
  /** What the scope's notes are called in the UI, e.g. "topic". */
  noun?: string;
}

export interface SearchOptions {
  limit?: number;
  signal?: AbortSignal;
}

export interface NoteSource {
  /** Notes in `scope` whose title contains `text`. */
  search(text: string, scope: NoteScope, options?: SearchOptions): Promise<NoteSummary[]>;
  /** The notes with these IDs, in the same order; IDs that don't exist are left out. */
  getNotes(noteIds: string[]): Promise<NoteSummary[]>;
  /** Where a note opens in Trilium. */
  noteUrl(noteId: string): string;
}

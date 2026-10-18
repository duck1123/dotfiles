// An item waiting in the review queue (a note with #inbox), as the review
// components need it. Fetching and saving is the host's job.

export interface InboxItem {
  noteId: string;
  title: string;
  /** #kind: link, video, journal, task, ... */
  kind?: string;
  /** #created, as stored (ISO-ish date or date-time). */
  created?: string;
  /** #url */
  url?: string;
  /** Targets of the note's ~topic relations. */
  topicIds: string[];
  /** Targets of older ~tag relations (shown, not edited). */
  tagIds: string[];
  /** Plain-text start of the note's content, if loaded. */
  preview?: string;
}

/** What the reviewer changed; sent to the host to write back. */
export interface ReviewChanges {
  title: string;
  kind: string;
  topicIds: string[];
  /** Clear #inbox, i.e. mark reviewed. */
  done: boolean;
}

/** The kinds the capture script sets. */
export const defaultKinds = ["link", "video", "journal", "task", "watchlist", "shopping"];

/** Where an inbox review gets its queue and saves to. */
export interface InboxSource {
  /** Every item still waiting, oldest first. */
  list(): Promise<InboxItem[]>;
  /** One item with its preview loaded. */
  get(noteId: string): Promise<InboxItem>;
  /** Write the changes; returns the updated item. */
  save(noteId: string, changes: ReviewChanges): Promise<InboxItem>;
}

// Shared Trilium (ETAPI) helpers for Windmill scripts and apps. Import with an
// absolute path, which works from any folder and from app backends:
//
//   import { connect } from "/f/lib/trilium.ts";
//   const trilium = await connect();
//
// connect() reads f/fleetops/trilium_base_url and trilium_etapi_token. Each
// client keeps its own note cache, so make one per job.
//
// "Topics" means notes under the Topics root, minus #topicScope=exclude (the
// note and its descendants) and everything below #topicScope=excludeChildren.
// A clone is in scope if any of its placements is. topicPath() is the one
// place that rule lives; the inbox review app and the photo tag sync both use
// it.
//
// main() is a diagnostic: for each note ID, whether it's a topic and its path.
import * as wmill from "windmill-client";

export const TOPICS_ROOT = "969cd62d38c65e2acaae97e48a24165e";
export const TIME_ZONE = "America/New_York";

export type Attribute = { attributeId: string; noteId: string; type: "label" | "relation"; name: string; value: string };
export type EtapiNote = {
  noteId: string;
  title: string;
  type: string;
  parentNoteIds: string[];
  parentBranchIds: string[];
  childNoteIds: string[];
  attributes: Attribute[];
};

// ETAPI also lists attributes a note inherits (e.g. from its template); these
// helpers only look at the note's own ones.
export const own = (note: EtapiNote) => note.attributes.filter((a) => a.noteId === note.noteId);
export const label = (note: EtapiNote, name: string) => own(note).find((a) => a.type === "label" && a.name === name);
export const labels = (note: EtapiNote, name: string) => own(note).filter((a) => a.type === "label" && a.name === name);
export const relations = (note: EtapiNote, name: string) =>
  own(note).filter((a) => a.type === "relation" && a.name === name);

/** The calendar day (for /calendar/days/<day>) and a #created stamp, in local time. */
export function localStamp(date: Date): { day: string; created: string } {
  const parts = Object.fromEntries(
    new Intl.DateTimeFormat("en-CA", {
      timeZone: TIME_ZONE, year: "numeric", month: "2-digit", day: "2-digit",
      hour: "2-digit", minute: "2-digit", hourCycle: "h23",
    }).formatToParts(date).map((p) => [p.type, p.value]),
  );
  const day = `${parts.year}-${parts.month}-${parts.day}`;
  return { day, created: `${day} ${parts.hour}:${parts.minute}` };
}

export type Trilium = ReturnType<typeof client>;

export async function connect(): Promise<Trilium> {
  return client(
    await wmill.getVariable("f/fleetops/trilium_base_url"),
    await wmill.getVariable("f/fleetops/trilium_etapi_token"),
  );
}

export function client(base: string, token: string) {
  /** One ETAPI call. `raw` sends `body` as text/plain (note content). */
  async function etapi(method: string, path: string, body?: unknown, raw = false): Promise<any> {
    const res = await fetch(`${base}/etapi${path}`, {
      method,
      headers: {
        Authorization: token,
        ...(body === undefined ? {} : { "Content-Type": raw ? "text/plain" : "application/json" }),
      },
      body: body === undefined ? undefined : raw ? (body as string) : JSON.stringify(body),
    });
    if (!res.ok) throw new Error(`ETAPI ${method} ${path}: ${res.status} ${await res.text()}`);
    const text = await res.text();
    return text && res.headers.get("content-type")?.includes("json") ? JSON.parse(text) : text;
  }

  const notes = new Map<string, Promise<EtapiNote>>();
  /** A note, cached for the client's lifetime unless `fresh`. */
  function getNote(noteId: string, fresh = false): Promise<EtapiNote> {
    if (fresh || !notes.has(noteId)) notes.set(noteId, etapi("GET", `/notes/${noteId}`));
    return notes.get(noteId)!;
  }

  /** Notes matching a Trilium search; results are added to the note cache. */
  async function search(query: string, params: Record<string, string> = {}): Promise<EtapiNote[]> {
    const qs = new URLSearchParams({ search: query, ...params });
    const results: EtapiNote[] = (await etapi("GET", `/notes?${qs}`)).results;
    results.forEach((n) => notes.set(n.noteId, Promise.resolve(n)));
    return results;
  }

  // Titles from "Topics" down to the note's parent, or null when the note isn't
  // a topic (outside the tree, or excluded by #topicScope).
  const paths = new Map<string, Promise<string[] | null>>();
  function topicPath(noteId: string, visiting: Set<string> = new Set()): Promise<string[] | null> {
    if (!paths.has(noteId)) {
      paths.set(
        noteId,
        (async () => {
          if (visiting.has(noteId)) return null;
          const note = await getNote(noteId);
          if (label(note, "topicScope")?.value === "exclude") return null;
          for (const parentId of note.parentNoteIds) {
            if (parentId === TOPICS_ROOT) return ["Topics"];
            if (parentId === "root" || parentId === "none") continue;
            const parent = await getNote(parentId);
            const rule = label(parent, "topicScope")?.value;
            if (rule === "exclude" || rule === "excludeChildren") continue;
            const above = await topicPath(parentId, new Set([...visiting, noteId]));
            if (above) return [...above, parent.title];
          }
          return null;
        })(),
      );
    }
    return paths.get(noteId)!;
  }

  return { etapi, getNote, search, topicPath };
}

export async function main(noteIds: string[]) {
  const trilium = await connect();
  return Promise.all(
    noteIds.map(async (noteId) => {
      const note = await trilium.getNote(noteId);
      const path = await trilium.topicPath(noteId);
      return { noteId, title: note.title, topic: path !== null, path };
    }),
  );
}

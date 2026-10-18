// Trilium access for the inbox review app, over ETAPI. One runnable with an
// `action`, so the helpers below are shared instead of copied into each.
//
// Actions:
// - list_inbox: every #inbox note, oldest first
// - get_item { noteId }: one item with a plain-text preview of its content
// - search_topics { text, limit }: topics whose title contains `text`
// - get_notes { noteIds }: topic summaries for chips
// - save_item { noteId, title, kind, topicIds, done, apply }: write the
//   review back (~topic relations, #kind, title; done clears #inbox).
//   Without apply=true it only returns the planned changes.
//
// "Topics" means notes under the Topics root, minus #photoTags=exclude (the
// note and its descendants) and everything below #photoTags=excludeChildren.
// A clone is in scope if any of its placements is.
import * as wmill from "windmill-client";

const TOPICS_ROOT = "969cd62d38c65e2acaae97e48a24165e";
const PREVIEW_CHARS = 1200;

type Attribute = { attributeId: string; noteId: string; type: "label" | "relation"; name: string; value: string };
type EtapiNote = { noteId: string; title: string; parentNoteIds: string[]; attributes: Attribute[] };

type NoteSummary = { noteId: string; title: string; icon?: string; path: string[] };
type InboxItem = {
  noteId: string;
  title: string;
  kind?: string;
  created?: string;
  url?: string;
  topicIds: string[];
  tagIds: string[];
  preview?: string;
};

let base = "";
let token = "";

async function etapi(method: string, path: string, body?: unknown): Promise<any> {
  const res = await fetch(`${base}/etapi${path}`, {
    method,
    headers: { Authorization: token, ...(body === undefined ? {} : { "Content-Type": "application/json" }) },
    body: body === undefined ? undefined : JSON.stringify(body),
  });
  if (!res.ok) throw new Error(`ETAPI ${method} ${path}: ${res.status} ${await res.text()}`);
  const text = await res.text();
  return text && res.headers.get("content-type")?.includes("json") ? JSON.parse(text) : text;
}

const notes = new Map<string, Promise<EtapiNote>>();
function getNote(noteId: string, fresh = false): Promise<EtapiNote> {
  if (fresh || !notes.has(noteId)) notes.set(noteId, etapi("GET", `/notes/${noteId}`));
  return notes.get(noteId)!;
}

// ETAPI also lists attributes a note inherits (e.g. from its template); only
// the note's own ones are edited here.
const own = (note: EtapiNote) => note.attributes.filter((a) => a.noteId === note.noteId);
const label = (note: EtapiNote, name: string) => own(note).find((a) => a.type === "label" && a.name === name);
const relations = (note: EtapiNote, name: string) => own(note).filter((a) => a.type === "relation" && a.name === name);

function search(query: string, params: Record<string, string> = {}): Promise<EtapiNote[]> {
  const qs = new URLSearchParams({ search: query, ...params });
  return etapi("GET", `/notes?${qs}`).then((r) => r.results);
}

// Titles from "Topics" down to the note's parent, or null when the note isn't
// a topic (outside the tree, or excluded by #photoTags).
const paths = new Map<string, Promise<string[] | null>>();
function topicPath(noteId: string, visiting: Set<string> = new Set()): Promise<string[] | null> {
  if (!paths.has(noteId)) {
    paths.set(
      noteId,
      (async () => {
        if (visiting.has(noteId)) return null;
        const note = await getNote(noteId);
        if (label(note, "photoTags")?.value === "exclude") return null;
        for (const parentId of note.parentNoteIds) {
          if (parentId === TOPICS_ROOT) return ["Topics"];
          if (parentId === "root" || parentId === "none") continue;
          const parent = await getNote(parentId);
          const rule = label(parent, "photoTags")?.value;
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

function summary(note: EtapiNote, path: string[]): NoteSummary {
  return { noteId: note.noteId, title: note.title, icon: label(note, "iconClass")?.value, path };
}

function toItem(note: EtapiNote): InboxItem {
  return {
    noteId: note.noteId,
    title: note.title,
    kind: label(note, "kind")?.value,
    created: label(note, "created")?.value,
    url: label(note, "url")?.value,
    topicIds: relations(note, "topic").map((a) => a.value),
    tagIds: relations(note, "tag").map((a) => a.value),
  };
}

function htmlToText(html: string): string {
  return html
    .replace(/<(br|\/p|\/li|\/h[1-6]|\/div|\/blockquote)[^>]*>/gi, "\n")
    .replace(/<[^>]+>/g, "")
    .replace(/&nbsp;/g, " ")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
    .replace(/&amp;/g, "&")
    .replace(/[ \t]+\n/g, "\n")
    .replace(/\n{3,}/g, "\n\n")
    .trim();
}

// Exact title first, then titles starting with the text, then the rest;
// shorter titles first within each group.
function rank(text: string, title: string): number {
  const t = title.toLowerCase();
  const q = text.toLowerCase();
  return (t === q ? 0 : t.startsWith(q) ? 1 : 2) * 1000 + Math.min(title.length, 999);
}

async function listInbox(): Promise<InboxItem[]> {
  return (await search("#inbox orderBy #created", { limit: "1000" })).map(toItem);
}

async function getItem(noteId: string): Promise<InboxItem> {
  const [note, content] = await Promise.all([getNote(noteId), etapi("GET", `/notes/${noteId}/content`)]);
  const text = htmlToText(String(content ?? ""));
  return { ...toItem(note), preview: text.length > PREVIEW_CHARS ? `${text.slice(0, PREVIEW_CHARS)}…` : text || undefined };
}

async function searchTopics(text: string, limit = 20): Promise<NoteSummary[]> {
  // Quotes and backslashes would end the search string early.
  const clean = text.replace(/["'`\\]/g, " ").replace(/\s+/g, " ").trim();
  if (!clean) return [];
  const found = (
    await search(`note.title *=* "${clean}"`, { ancestorNoteId: TOPICS_ROOT, fastSearch: "true", limit: "60" })
  ).filter((n) => n.noteId !== TOPICS_ROOT);
  found.forEach((n) => notes.set(n.noteId, Promise.resolve(n)));
  const withPaths = await Promise.all(found.map(async (n) => ({ n, path: await topicPath(n.noteId) })));
  return withPaths
    .filter((r): r is { n: EtapiNote; path: string[] } => r.path !== null)
    .sort((a, b) => rank(clean, a.n.title) - rank(clean, b.n.title))
    .slice(0, limit)
    .map(({ n, path }) => summary(n, path));
}

async function getNotes(noteIds: string[]): Promise<NoteSummary[]> {
  const found = await Promise.all(
    noteIds.map(async (id) => {
      try {
        const note = await getNote(id);
        return summary(note, (await topicPath(id)) ?? []);
      } catch {
        return null; // deleted notes are left out
      }
    }),
  );
  return found.filter((n): n is NoteSummary => n !== null);
}

type Change = { what: string; method: string; path: string; body?: unknown };

async function saveItem(args: {
  noteId: string;
  title: string;
  kind: string;
  topicIds: string[];
  done: boolean;
  apply?: boolean;
}) {
  const note = await getNote(args.noteId, true);
  const changes: Change[] = [];
  const title = args.title.trim();

  if (title && title !== note.title) {
    changes.push({ what: `title → ${title}`, method: "PATCH", path: `/notes/${note.noteId}`, body: { title } });
  }

  const kind = label(note, "kind");
  if (args.kind && !kind) {
    changes.push({ what: `#kind=${args.kind}`, method: "POST", path: "/attributes", body: { noteId: note.noteId, type: "label", name: "kind", value: args.kind } });
  } else if (kind && args.kind && kind.value !== args.kind) {
    changes.push({ what: `#kind ${kind.value} → ${args.kind}`, method: "PATCH", path: `/attributes/${kind.attributeId}`, body: { value: args.kind } });
  } else if (kind && !args.kind) {
    changes.push({ what: `remove #kind=${kind.value}`, method: "DELETE", path: `/attributes/${kind.attributeId}` });
  }

  const existing = relations(note, "topic");
  for (const target of args.topicIds) {
    if (!existing.some((a) => a.value === target)) {
      changes.push({ what: `add ~topic ${target}`, method: "POST", path: "/attributes", body: { noteId: note.noteId, type: "relation", name: "topic", value: target } });
    }
  }
  for (const attr of existing) {
    if (!args.topicIds.includes(attr.value)) {
      changes.push({ what: `remove ~topic ${attr.value}`, method: "DELETE", path: `/attributes/${attr.attributeId}` });
    }
  }

  if (args.done) {
    for (const attr of own(note).filter((a) => a.type === "label" && a.name === "inbox")) {
      changes.push({ what: "remove #inbox", method: "DELETE", path: `/attributes/${attr.attributeId}` });
    }
  }

  if (!args.apply) {
    return { applied: false, changes: changes.map((c) => c.what) };
  }
  for (const change of changes) {
    await etapi(change.method, change.path, change.body);
  }
  return toItem(await getNote(note.noteId, true));
}

export async function main(action: string, args: Record<string, any> = {}) {
  base = await wmill.getVariable("f/fleetops/trilium_base_url");
  token = await wmill.getVariable("f/fleetops/trilium_etapi_token");

  switch (action) {
    case "list_inbox":
      return listInbox();
    case "get_item":
      return getItem(args.noteId);
    case "search_topics":
      return searchTopics(String(args.text ?? ""), Number(args.limit ?? 20));
    case "get_notes":
      return getNotes(args.noteIds ?? []);
    case "save_item":
      return saveItem(args as Parameters<typeof saveItem>[0]);
    default:
      throw new Error(`unknown action: ${action}`);
  }
}

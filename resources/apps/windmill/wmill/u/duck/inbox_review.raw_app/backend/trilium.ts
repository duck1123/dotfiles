// Trilium access for the inbox review app, over ETAPI. One runnable with an
// `action`, so the helpers below are shared instead of copied into each.
//
// Actions:
// - list_inbox: every #inbox note, oldest first
// - get_item { noteId }: one item with a plain-text preview of its content
//   and the reviewer's own notes
// - search_topics { text, limit }: topics whose title contains `text`
// - get_notes { noteIds }: topic summaries for chips
// - save_item { noteId, title, kind, topicIds, notes?, done, apply }: write
//   the review back (~topic relations, #kind, title, the notes section; done
//   clears #inbox). Without apply=true it only returns the planned changes.
//
// The reviewer's notes are a "My notes" <h2> section in the note's content,
// kept apart from what capture and youtube_ingest write there: the preview
// leaves it out, and saving notes rewrites only that section.
//
// "Topics" (and the ETAPI client) come from the shared f/lib/trilium module:
// notes under the Topics root, minus #topicScope exclusions, per placement.
import { connect, label, own, relations, TOPICS_ROOT, type EtapiNote, type Trilium } from "/f/lib/trilium.ts";

const PREVIEW_CHARS = 1200;
const NOTES_HEADING = "<h2>My notes</h2>";
// The section runs from its heading to the next h2/h3 (youtube_ingest's
// description heading is an h3) or the end.
const NOTES_SECTION = /<h2[^>]*>\s*My notes\s*<\/h2>[\s\S]*?(?=<h[23][\s>]|$)/i;

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
  notes?: string;
};

let etapi: Trilium["etapi"];
let getNote: Trilium["getNote"];
let search: Trilium["search"];
let topicPath: Trilium["topicPath"];

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

function escape(text: string): string {
  return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

// Split content into the reviewer's notes (plain text) and everything else.
function splitNotes(html: string): { notes: string; rest: string } {
  const match = html.match(NOTES_SECTION);
  if (!match) return { notes: "", rest: html };
  // A blank line between paragraphs, as withNotes reads them.
  const body = match[0].replace(/^<h2[^>]*>[\s\S]*?<\/h2>/i, "").replace(/<\/p>/gi, "</p>\n");
  return { notes: htmlToText(body), rest: html.replace(match[0], "") };
}

// Content with the notes section replaced (or added at the end, or removed
// when the notes are empty). Blank lines separate paragraphs.
function withNotes(html: string, notes: string): string {
  const paras = notes
    .trim()
    .split(/\n\s*\n/)
    .filter((p) => p.trim())
    .map((p) => `<p>${escape(p.trim()).replace(/\n/g, "<br>")}</p>`);
  const section = paras.length ? NOTES_HEADING + paras.join("") : "";
  return NOTES_SECTION.test(html) ? html.replace(NOTES_SECTION, section) : html + section;
}

async function listInbox(): Promise<InboxItem[]> {
  return (await search("#inbox orderBy #created", { limit: "1000" })).map(toItem);
}

async function getItem(noteId: string): Promise<InboxItem> {
  const [note, content] = await Promise.all([getNote(noteId), etapi("GET", `/notes/${noteId}/content`)]);
  const { notes, rest } = splitNotes(String(content ?? ""));
  const text = htmlToText(rest);
  return {
    ...toItem(note),
    preview: text.length > PREVIEW_CHARS ? `${text.slice(0, PREVIEW_CHARS)}…` : text || undefined,
    notes,
  };
}

async function searchTopics(text: string, limit = 20): Promise<NoteSummary[]> {
  // Quotes and backslashes would end the search string early.
  const clean = text.replace(/["'`\\]/g, " ").replace(/\s+/g, " ").trim();
  if (!clean) return [];
  const found = (
    await search(`note.title *=* "${clean}"`, { ancestorNoteId: TOPICS_ROOT, fastSearch: "true", limit: "60" })
  ).filter((n) => n.noteId !== TOPICS_ROOT);
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

type Change = { what: string; method: string; path: string; body?: unknown; raw?: boolean };

async function saveItem(args: {
  noteId: string;
  title: string;
  kind: string;
  topicIds: string[];
  /** Replaces the notes section; left out, the content isn't touched. */
  notes?: string;
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

  if (args.notes !== undefined) {
    const content = String((await etapi("GET", `/notes/${note.noteId}/content`)) ?? "");
    if (splitNotes(content).notes !== splitNotes(withNotes("", args.notes)).notes) {
      changes.push({ what: args.notes.trim() ? "set notes" : "remove notes", method: "PUT", path: `/notes/${note.noteId}/content`, body: withNotes(content, args.notes), raw: true });
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
    await etapi(change.method, change.path, change.body, change.raw);
  }
  return toItem(await getNote(note.noteId, true));
}

export async function main(action: string, args: Record<string, any> = {}) {
  ({ etapi, getNote, search, topicPath } = await connect());

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

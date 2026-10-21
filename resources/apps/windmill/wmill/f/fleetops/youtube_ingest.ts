// Index YouTube videos in Trilium.
//
// - url: ingest one new video. Creates a video note in the calendar day of
//   `watchedAt` (default now), from the YouTube Video template, with ~channel
//   pointing at the channel note (created from the YouTube Channel template if
//   none has its #youtubeChannelId yet).
// - no url: backfill existing #kind=video / #kind=link notes with a YouTube
//   video #url that don't have #youtubeVideoId yet (or just `noteIds`), up to
//   `limit`. Links become #kind=video. Same labels and ~channel; the ~tag
//   pointing at the channel is replaced by ~channel, and the old ~creator /
//   clone under Youtube Videos are cleaned up.
//
// dryRun (default true) only reports what would change.
import * as wmill from "windmill-client";
import { connect, localStamp, type Attribute, type EtapiNote, type Trilium } from "/f/lib/trilium.ts";

const VIDEO_TEMPLATE = "TR59IiYZa5V2";
const CHANNEL_TEMPLATE = "MEwPXJNfzb5F";
const CHANNEL_PARENT = "ded6b81aed13ee81b3008e65fc49b3a4"; // YouTube Channels
const OLD_VIDEOS_PARENT = "9bf7c7fbb1e2170e792ddff6b221a9f2"; // Youtube Videos
const DESCRIPTION_HEADING = "<h3>YouTube description</h3>";

type Note = Pick<EtapiNote, "noteId" | "title" | "attributes" | "parentBranchIds">;
type Attr = Attribute;
type Video = {
  id: string;
  title: string;
  channelId: string;
  publishedAt: string;
  duration: number | null;
  description: string;
  tags: string[];
  thumbnail: string | null;
};

let trilium: Trilium;
let etapi: Trilium["etapi"];
let ytKey = "";
let dryRun = true;

const search = (query: string, limit = 100000): Promise<Note[]> => trilium.search(query, { limit: String(limit) });

async function youtube(endpoint: string, ids: string[], part: string): Promise<any[]> {
  const items: any[] = [];
  for (let i = 0; i < ids.length; i += 50) {
    const qs = new URLSearchParams({ part, id: ids.slice(i, i + 50).join(","), key: ytKey, maxResults: "50" });
    const res = await fetch(`https://www.googleapis.com/youtube/v3/${endpoint}?${qs}`);
    const data = await res.json();
    if (!res.ok) throw new Error(`YouTube ${endpoint}: ${data.error?.message ?? res.status}`);
    items.push(...(data.items ?? []));
  }
  return items;
}

export function videoId(url: string): string | null {
  let u: URL;
  try {
    u = new URL(url);
  } catch {
    return null;
  }
  const host = u.hostname.replace(/^(www|m)\./, "");
  if (host === "youtu.be") return u.pathname.split("/")[1] || null;
  if (!host.endsWith("youtube.com")) return null;
  return u.searchParams.get("v") ?? u.pathname.match(/^\/(?:shorts|live|embed|v)\/([\w-]{11})/)?.[1] ?? null;
}

function seconds(iso: string | undefined): number | null {
  const m = iso?.match(/^P(?:(\d+)D)?T?(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)S)?$/);
  if (!m) return null;
  const [d, h, min, s] = m.slice(1).map((x) => Number(x ?? 0));
  return ((d * 24 + h) * 60 + min) * 60 + s;
}

function thumb(t: any): string | null {
  return (t?.maxres ?? t?.high ?? t?.medium ?? t?.default)?.url ?? null;
}

async function videos(ids: string[]): Promise<Map<string, Video>> {
  const out = new Map<string, Video>();
  for (const item of await youtube("videos", ids, "snippet,contentDetails")) {
    const s = item.snippet;
    out.set(item.id, {
      id: item.id,
      title: s.title,
      channelId: s.channelId,
      publishedAt: s.publishedAt,
      duration: seconds(item.contentDetails?.duration),
      description: s.description ?? "",
      tags: s.tags ?? [],
      thumbnail: thumb(s.thumbnails),
    });
  }
  return out;
}

const escape = (s: string) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

function descriptionHtml(text: string): string {
  const paras = text.split("\n").filter((l) => l.trim()).map((l) => `<p>${escape(l)}</p>`);
  return paras.length ? DESCRIPTION_HEADING + paras.join("") : "";
}

// Planned/applied changes for one note, reported back to the caller.
class Changes {
  ops: string[] = [];
  constructor(public note: Note) {}

  private own(type: string, name: string) {
    return this.note.attributes.filter((a) => a.noteId === this.note.noteId && a.type === type && a.name === name);
  }

  async label(name: string, value: string | number | null) {
    if (value === null || value === "") return;
    const v = String(value);
    const cur = this.own("label", name);
    if (cur[0]?.value === v) return;
    this.ops.push(`#${name}=${v.length > 60 ? v.slice(0, 57) + "..." : v}`);
    if (dryRun) return;
    if (cur[0]) await etapi("PATCH", `/attributes/${cur[0].attributeId}`, { value: v });
    else await etapi("POST", "/attributes", { noteId: this.note.noteId, type: "label", name, value: v });
  }

  async relation(name: string, target: string, label = target) {
    if (this.own("relation", name).some((a) => a.value === target)) return;
    this.ops.push(`~${name}=${label}`);
    if (!dryRun) await etapi("POST", "/attributes", { noteId: this.note.noteId, type: "relation", name, value: target });
  }

  async remove(attr: Attr, why: string) {
    this.ops.push(`-${attr.type === "relation" ? "~" : "#"}${attr.name}=${attr.value} (${why})`);
    if (!dryRun) await etapi("DELETE", `/attributes/${attr.attributeId}`);
  }

  relations(name: string) {
    return this.own("relation", name);
  }
}

// Channel notes by #youtubeChannelId, loaded once per run.
let channels: Map<string, { noteId: string; title: string }> | null = null;

async function loadChannels() {
  if (channels) return channels;
  channels = new Map();
  for (const n of await search("#youtubeChannelId")) {
    const id = n.attributes.find((a) => a.name === "youtubeChannelId" && a.noteId === n.noteId)?.value;
    if (id) channels.set(id, { noteId: n.noteId, title: n.title });
  }
  return channels;
}

async function channelNote(channelId: string): Promise<{ noteId: string; title: string; created: boolean }> {
  const found = (await loadChannels()).get(channelId);
  if (found) return { ...found, created: false };

  const [ch] = await youtube("channels", [channelId], "snippet");
  const title = (ch?.snippet?.title ?? channelId).trim();
  if (dryRun) return { noteId: `(new: ${title})`, title, created: true };
  const { note } = await etapi("POST", "/create-note", {
    parentNoteId: CHANNEL_PARENT, title, type: "text", content: descriptionHtml(ch?.snippet?.description ?? ""),
  });
  const c = new Changes({ ...note, attributes: [] });
  await c.relation("template", CHANNEL_TEMPLATE);
  await c.label("youtubeChannelId", channelId);
  await c.label("youtubeHandle", ch?.snippet?.customUrl ?? null);
  await c.label("thumbnail", thumb(ch?.snippet?.thumbnails));
  channels!.set(channelId, { noteId: note.noteId, title });
  return { noteId: note.noteId, title, created: true };
}

// A video that's gone from YouTube: its old ~creator is the channel, or else
// the one ~tag that points at a channel note, if there's exactly one.
async function fallbackChannel(c: Changes) {
  const byNote = new Map([...(await loadChannels()).values()].map((ch) => [ch.noteId, ch.title]));
  const creators = c.relations("creator");
  const tags = c.relations("tag").filter((a) => byNote.has(a.value));
  const pick = creators[0] ?? (tags.length === 1 ? tags[0] : undefined);
  if (!pick) return;
  await c.relation("channel", pick.value, byNote.get(pick.value) ?? pick.value);
  for (const cr of creators) {
    if (cr.value !== pick.value) await c.relation("topic", cr.value);
    await c.remove(cr, cr.value === pick.value ? "now ~channel" : "now ~topic");
  }
  for (const t of c.relations("tag").filter((a) => a.value === pick.value)) await c.remove(t, "now ~channel");
}

async function videoLabels(c: Changes, v: Video | undefined, id: string) {
  await c.relation("template", VIDEO_TEMPLATE, "YouTube Video");
  if (v) {
    const ch = await channelNote(v.channelId);
    await c.relation("channel", ch.noteId, ch.title + (ch.created ? " (new channel note)" : ""));
    for (const t of c.relations("tag").filter((a) => a.value === ch.noteId)) await c.remove(t, "now ~channel");
    for (const cr of c.relations("creator")) {
      if (cr.value !== ch.noteId) await c.relation("topic", cr.value);
      await c.remove(cr, cr.value === ch.noteId ? "now ~channel" : "now ~topic");
    }
    await c.label("published", v.publishedAt.slice(0, 10));
    await c.label("duration", v.duration);
    await c.label("thumbnail", v.thumbnail);
    await c.label("youtubeTags", v.tags.join(", "));
  } else {
    await fallbackChannel(c);
  }
  await c.label("youtubeVideoId", id); // last: marks the note as done
}

async function appendDescription(c: Changes, v: Video | undefined) {
  const html = v && descriptionHtml(v.description);
  if (!html) return;
  const content: string = await etapi("GET", `/notes/${c.note.noteId}/content`);
  if (content.includes(DESCRIPTION_HEADING)) return;
  c.ops.push("+description");
  if (!dryRun) await etapi("PUT", `/notes/${c.note.noteId}/content`, content + html, true);
}

async function dropOldClone(c: Changes) {
  if (c.note.parentBranchIds.length < 2) return;
  for (const b of c.note.parentBranchIds) {
    const branch = await etapi("GET", `/branches/${b}`);
    if (branch.parentNoteId !== OLD_VIDEOS_PARENT) continue;
    c.ops.push("-clone under Youtube Videos");
    if (!dryRun) await etapi("DELETE", `/branches/${b}`);
  }
}

// Load the variables and reset per-run state. main() calls this; scripts that
// import ingest() (f/fleetops/capture) call it first too.
export async function setup(dry: boolean) {
  trilium = await connect();
  etapi = trilium.etapi;
  ytKey = await wmill.getVariable("f/fleetops/youtube_api_key");
  dryRun = dry;
  channels = null;
}

export async function ingest(url: string, watchedAt?: string) {
  const id = videoId(url);
  if (!id) throw new Error(`not a YouTube video URL: ${url}`);
  const v = (await videos([id])).get(id);
  const { day, created } = localStamp(watchedAt ? new Date(watchedAt) : new Date());

  const dayNote = await etapi("GET", `/calendar/days/${day}`);
  const existing = (await search(`#youtubeVideoId="${id}"`)).find((n) => n.parentBranchIds.length &&
    n.attributes.some((a) => a.name === "created" && a.value.startsWith(day)));
  if (existing) return { status: "exists", noteId: existing.noteId, title: existing.title, day };

  const link = `<p><a href="${escape(url)}">${escape(url)}</a></p>`;
  const title = v?.title ?? url;
  let note: Note = { noteId: "(new)", title, attributes: [], parentBranchIds: [] };
  if (!dryRun) {
    const res = await etapi("POST", "/create-note", {
      parentNoteId: dayNote.noteId, title, type: "text", content: link + descriptionHtml(v?.description ?? ""),
    });
    note = { ...res.note, attributes: [] };
  }
  const c = new Changes(note);
  await c.label("kind", "video");
  await c.label("created", created);
  await c.label("url", url);
  await videoLabels(c, v, id);
  return { status: dryRun ? "would create" : "created", noteId: note.noteId, title, day, found: !!v, ops: c.ops };
}

// Video and link notes with a YouTube #url that aren't indexed yet. Links that
// point at a video become #kind=video; other YouTube links (channels,
// playlists) stay links and are reported separately.
// (The OR group must come last: leading it, Trilium drops the other filters.)
const PENDING = `#url *=* youtu #!youtubeVideoId (#kind=video or #kind=link)`;

const urlOf = (n: Note) => n.attributes.find((a) => a.name === "url" && a.noteId === n.noteId)?.value ?? "";

async function pending() {
  const all = (await search(PENDING)).map((note) => ({ note, id: videoId(urlOf(note)) }));
  return { todo: all.filter((t) => t.id), notVideos: all.filter((t) => !t.id) };
}

async function backfill(noteIds: string[] | undefined, limit: number) {
  const todo = noteIds?.length
    ? (await Promise.all(noteIds.map((id) => etapi("GET", `/notes/${id}`)))).map((note: Note) => ({ note, id: videoId(urlOf(note)) }))
    : (await pending()).todo.slice(0, limit);
  const found = await videos(todo.flatMap((t) => (t.id ? [t.id] : [])));

  const results = [];
  for (const { note, id } of todo) {
    if (!id) {
      results.push({ noteId: note.noteId, title: note.title, status: "skipped: no YouTube video id in #url" });
      continue;
    }
    const v = found.get(id);
    const c = new Changes(note);
    await c.label("kind", "video");
    await videoLabels(c, v, id);
    await appendDescription(c, v);
    await dropOldClone(c);
    const status = v ? "ok" : c.ops.some((o) => o.startsWith("~channel")) ? "video gone (channel from tags)" : "video gone (no channel)";
    results.push({ noteId: note.noteId, title: note.title, status, ops: c.ops });
  }
  return results;
}

export async function main(
  url?: string,
  watchedAt?: string,
  noteIds?: string[],
  limit: number = 25,
  dry_run: boolean = true,
) {
  await setup(dry_run);

  if (url) return await ingest(url, watchedAt);
  const results = await backfill(noteIds, limit);
  const counts: Record<string, number> = {};
  for (const r of results) counts[r.status] = (counts[r.status] ?? 0) + 1;
  const { todo, notVideos } = await pending();
  return {
    dryRun, counts, remaining: todo.length,
    notVideos: notVideos.map(({ note }) => ({ noteId: note.noteId, title: note.title, url: urlOf(note) })),
    results,
  };
}

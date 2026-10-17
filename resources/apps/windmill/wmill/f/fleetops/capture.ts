// Capture one item into Trilium's inbox: a note of its own under the day's
// calendar note, with #created and #inbox (the review marker).
//
// - "task: ...", "watch: ...", "shopping: ..." set #kind=task / watchlist /
//   shopping.
// - Otherwise a YouTube video link goes through f/fleetops/youtube_ingest
//   (#kind=video, channel, metadata); any other link is #kind=link, titled from
//   the page; plain text is #kind=journal, titled from its first line.
// - Text sent along with a link is kept in the note's content.
import * as wmill from "windmill-client";
// Imported rather than run as a child job: a parent waiting on a child job
// deadlocks when the worker has no free slot for the child.
import { ingest as ingestYoutube, setup as setupYoutube } from "./youtube_ingest.ts";

const PREFIXES: Record<string, string> = { task: "task", watch: "watchlist", shopping: "shopping" };
// Sites whose page title is only the site's name; the URL is a better title.
const GENERIC_TITLES = new Set(["reddit", "youtube", "x", "twitter", "facebook", "instagram"]);
const TIME_ZONE = "America/New_York";
const URL_RE = /https?:\/\/[^\s"<>]+/;

let base = "";
let token = "";

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

async function label(noteId: string, name: string, value: string) {
  await etapi("POST", "/attributes", { noteId, type: "label", name, value });
}

const escape = (s: string) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

const paragraphs = (text: string) =>
  text.split("\n").filter((l) => l.trim()).map((l) => `<p>${escape(l)}</p>`).join("");

function decodeEntities(s: string): string {
  const named: Record<string, string> = { amp: "&", lt: "<", gt: ">", quot: '"', apos: "'", nbsp: " " };
  return s.replace(/&(#x[0-9a-f]+|#\d+|\w+);/gi, (m, e: string) =>
    e[0] === "#" ? String.fromCodePoint(parseInt(e.slice(e[1] === "x" ? 2 : 1), e[1] === "x" ? 16 : 10)) : named[e.toLowerCase()] ?? m);
}

function localStamp(date: Date): { day: string; created: string } {
  const parts = Object.fromEntries(
    new Intl.DateTimeFormat("en-CA", {
      timeZone: TIME_ZONE, year: "numeric", month: "2-digit", day: "2-digit",
      hour: "2-digit", minute: "2-digit", hourCycle: "h23",
    }).formatToParts(date).map((p) => [p.type, p.value]),
  );
  const day = `${parts.year}-${parts.month}-${parts.day}`;
  return { day, created: `${day} ${parts.hour}:${parts.minute}` };
}

function isYoutubeVideo(url: string): boolean {
  return /^https?:\/\/(www\.|m\.)?(youtube\.com\/(watch|shorts\/|live\/)|youtu\.be\/)/.test(url);
}

// The page's og:title or <title>; a direct file link's file name; else the URL.
async function linkTitle(url: string): Promise<string> {
  const file = decodeURIComponent(new URL(url).pathname.replace(/\/$/, "").split("/").pop() ?? "");
  try {
    const res = await fetch(url, {
      headers: { "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) Trilium-capture" },
      signal: AbortSignal.timeout(15000),
    });
    if (res.headers.get("content-type")?.includes("html")) {
      const page = (await res.text()).slice(0, 300_000);
      const m = page.match(/<meta[^>]+property=["']og:title["'][^>]+content=["']([^"']+)/i) ??
        page.match(/<title[^>]*>([\s\S]*?)<\/title>/i);
      const title = m && decodeEntities(m[1]).trim();
      if (title && !GENERIC_TITLES.has(title.toLowerCase())) return title;
    } else if (file.includes(".")) {
      return file;
    }
  } catch {
    // unreachable or slow: fall back to the URL
  }
  return url;
}

function firstLine(text: string, max = 100): string {
  const line = text.split("\n").find((l) => l.trim())?.trim() ?? "";
  if (line.length <= max) return line;
  const cut = line.slice(0, max);
  return cut.slice(0, cut.lastIndexOf(" ") > 40 ? cut.lastIndexOf(" ") : max) + "…";
}

export async function main(text: string = "", url?: string, createdAt?: string, dry_run: boolean = false) {
  base = await wmill.getVariable("f/fleetops/trilium_base_url");
  token = await wmill.getVariable("f/fleetops/trilium_etapi_token");

  let rest = text.trim();
  const prefix = rest.match(/^(task|watch|shopping):\s*/i);
  if (prefix) rest = rest.slice(prefix[0].length);
  const link = url?.trim() || rest.match(URL_RE)?.[0];
  if (link && !url) rest = rest.replace(link, "").trim();
  if (!rest && !link) throw new Error("nothing to capture");

  const when = createdAt ? new Date(createdAt) : new Date();
  const { day, created } = localStamp(when);

  // YouTube videos: youtube_ingest makes the note; mark it and keep any text.
  if (link && !prefix && isYoutubeVideo(link)) {
    await setupYoutube(dry_run);
    const res: any = await ingestYoutube(link, when.toISOString());
    if (!dry_run) {
      await label(res.noteId, "inbox", "");
      if (rest) {
        const content: string = await etapi("GET", `/notes/${res.noteId}/content`);
        await etapi("PUT", `/notes/${res.noteId}/content`, paragraphs(rest) + content, true);
      }
    }
    return { kind: "video", ...res, inbox: true };
  }

  const kind = prefix ? PREFIXES[prefix[1].toLowerCase()] : link ? "link" : "journal";
  const title = rest ? firstLine(rest) : await linkTitle(link!);
  const textHtml = rest && (rest.includes("\n") || title !== rest) ? paragraphs(rest) : "";
  const content = (link ? `<p><a href="${escape(link)}">${escape(link)}</a></p>` : "") + textHtml;
  const result = { kind, title, day, created, url: link ?? null };
  if (dry_run) return { status: "would create", ...result };

  const dayNote = await etapi("GET", `/calendar/days/${day}`);
  const { note } = await etapi("POST", "/create-note", { parentNoteId: dayNote.noteId, title, type: "text", content });
  await label(note.noteId, "kind", kind);
  await label(note.noteId, "created", created);
  if (link) await label(note.noteId, "url", link);
  await label(note.noteId, "inbox", "");
  return { status: "created", noteId: note.noteId, ...result };
}

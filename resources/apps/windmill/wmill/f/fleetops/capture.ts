// Capture one item into Trilium's inbox: a note of its own under the day's
// calendar note, with #created and #inbox (the review marker).
//
// - "task: ...", "watch: ...", "shopping: ..." set #kind=task / watchlist /
//   shopping.
// - Otherwise a YouTube video link goes through f/fleetops/youtube_ingest
//   (#kind=video, channel, metadata); any other link is #kind=link, titled from
//   the page; plain text is #kind=journal, titled from its first line.
// - Text sent along with a link is kept in the note's content.
import { connect, localStamp, type Trilium } from "/f/lib/trilium.ts";
// Imported rather than run as a child job: a parent waiting on a child job
// deadlocks when the worker has no free slot for the child.
import { ingest as ingestYoutube, setup as setupYoutube } from "./youtube_ingest.ts";

const PREFIXES: Record<string, string> = { task: "task", watch: "watchlist", shopping: "shopping" };
// Sites whose page title is only the site's name; the URL is a better title.
const GENERIC_TITLES = new Set(["reddit", "youtube", "x", "twitter", "facebook", "instagram"]);
const URL_RE = /https?:\/\/[^\s"<>]+/;

let etapi: Trilium["etapi"];

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

function isYoutubeVideo(url: string): boolean {
  return /^https?:\/\/(www\.|m\.)?(youtube\.com\/(watch|shorts\/|live\/)|youtu\.be\/)/.test(url);
}

// Reddit blocks scripted page fetches, but its oEmbed endpoint answers. Share
// links (/r/<sub>/s/<id>) redirect to the post first.
async function redditTitle(url: string): Promise<string | null> {
  const res = await fetch(url, {
    redirect: "manual",
    headers: { "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) Trilium-capture" },
    signal: AbortSignal.timeout(15000),
  });
  const post = (res.headers.get("location") ?? url).split("?")[0];
  const oembed = await fetch(`https://www.reddit.com/oembed?url=${encodeURIComponent(post)}`, {
    headers: { "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) Trilium-capture" },
    signal: AbortSignal.timeout(15000),
  });
  return oembed.ok ? ((await oembed.json()).title ?? null) : null;
}

// The page's og:title or <title>; a direct file link's file name; else the URL.
async function linkTitle(url: string): Promise<string> {
  const file = decodeURIComponent(new URL(url).pathname.replace(/\/$/, "").split("/").pop() ?? "");
  try {
    if (/(^|\.)reddit\.com$/.test(new URL(url).hostname)) return (await redditTitle(url)) ?? url;
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
  ({ etapi } = await connect());

  let rest = text.trim();
  const prefix = rest.match(/^(task|watch|shopping):\s*/i);
  if (prefix) rest = rest.slice(prefix[0].length);
  const link = url?.trim() || rest.match(URL_RE)?.[0];
  if (link && !url) rest = rest.replace(link, "").trim();
  // Share apps join the page title and text with separators ("Title - URL");
  // with no title, only the separator is left.
  rest = rest.replace(/^[\s\-–—|·:]+|[\s\-–—|·:]+$/g, "");
  if (!/[\p{L}\p{N}]/u.test(rest)) rest = "";
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

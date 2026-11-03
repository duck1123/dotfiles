// Replaces the files Duck asked to replace in the review app. Runs on a
// schedule; the only mediacheck script that touches files or Sonarr/Radarr.
//
// Each run:
// 1. Deletes the quarantined copies of repairs Duck chose to clean up, and the
//    leftovers he chose to remove.
// 2. Files whose new copy has been decoded: clean or minor means replaced, and
//    the original waits for Duck. So does unclear, which isn't known damage, and
//    the new file goes to To review. Damaged goes to quarantine and is tried
//    again, without blocklisting: the same releases have come back intact, so
//    the damage was our write path. MAX_ATTEMPTS failures need Duck.
// 3. Files being searched for: an import queues the new file ahead of the rest.
//    Nothing downloading after SEARCH_IDLE_HOURS puts the original back.
// 4. Starts new repairs, up to MAX_ACTIVE, while the NAS keeps MIN_FREE_BYTES.
//    A file with no usable release stays in place and is retried in a week.
//
// A file Sonarr/Radarr doesn't track can't be replaced, so it stays in place and
// waits for Duck. It's only a leftover if another copy of the same episode or
// movie is tracked; plenty of untracked files are extras, the only copy they have.
//
// A move is a rename inside the library's export, so it needs no space. Nothing
// here overwrites a file, and only step 1 deletes one. Schema:
// f/mediacheck/schema.pg.sql.
import * as wmill from "windmill-client";
import { SQL } from "bun";
import { mkdirSync, statSync, writeFileSync } from "node:fs";
import { rename, unlink } from "node:fs/promises";
import { dirname, join, normalize } from "node:path";

const MAX_ACTIVE = 3;
const MAX_ATTEMPTS = 3;
const MIN_FREE_BYTES = 100e9;
const SEARCH_IDLE_HOURS = 3;
const SEARCH_STUCK_HOURS = 48;
const RETRY_DAYS = 7;
const LOCK_KEY = 7204411;
const QUARANTINE = ".quarantine";
const STILL_LISTED = "Sonarr/Radarr still lists a file after the rescan, and it isn't on disk";

type Row = Record<string, any>;
type Library = "tv" | "movies";

let db: SQL;

class Arr {
  constructor(
    readonly library: Library,
    readonly base: string,
    readonly key: string,
    readonly mount: string,
    readonly root: string,
  ) {}

  async call(path: string, body?: unknown): Promise<any> {
    const res = await fetch(`${this.base}/api/v3/${path}`, {
      method: body === undefined ? "GET" : "POST",
      headers: { "X-Api-Key": this.key, "Content-Type": "application/json" },
      body: body === undefined ? undefined : JSON.stringify(body),
      signal: AbortSignal.timeout(180_000),
    });
    if (!res.ok) throw new Error(`${this.library} ${path}: ${res.status}`);
    const text = await res.text();
    return text ? JSON.parse(text) : null;
  }

  async command(body: Row): Promise<string> {
    const { id } = await this.call("command", body);
    for (let i = 0; i < 60; i++) {
      const { status, message } = await this.call(`command/${id}`);
      if (status === "failed" || status === "aborted") throw new Error(`${body.name} ${status}: ${message ?? ""}`);
      if (status === "completed") return status;
      await Bun.sleep(3000);
    }
    throw new Error(`${body.name} didn't finish`);
  }

  file(rel: string): string {
    const path = normalize(join(this.mount, rel));
    if (!path.startsWith(`${this.mount}/`)) throw new Error(`path leaves the library: ${rel}`);
    return path;
  }

  /** The Sonarr series and episodes, or the Radarr movie, that this file belongs to. */
  async lookup(rel: string): Promise<{ parent: number; items: number[] } | null> {
    const [group, ...rest] = rel.split("/");
    const inside = rest.join("/");
    if (this.library === "tv") {
      const series = (await this.call("series")).find((s: Row) => s.path === `${this.root}/${group}`);
      if (!series) return null;
      const files = await this.call(`episodefile?seriesId=${series.id}`);
      const file = files.find((f: Row) => f.relativePath === inside);
      if (!file) return null;
      const episodes = await this.call(`episode?seriesId=${series.id}`);
      const items = episodes.filter((e: Row) => e.episodeFileId === file.id).map((e: Row) => e.id);
      return items.length ? { parent: series.id, items } : null;
    }
    const movie = (await this.call("movie")).find((m: Row) => m.path === `${this.root}/${group}`);
    return movie?.movieFile?.relativePath === inside ? { parent: movie.id, items: [movie.id] } : null;
  }

  /** The copy tracked for the same episode or movie as this untracked file, or null. */
  async trackedCopy(rel: string): Promise<string | null> {
    const [group, ...rest] = rel.split("/");
    if (this.library === "movies") {
      const movie = (await this.call("movie")).find((m: Row) => m.path === `${this.root}/${group}`);
      return movie?.movieFile ? `${group}/${movie.movieFile.relativePath}` : null;
    }
    const episode = (name: string) => {
      const match = name.match(/S(\d+)E(\d+)/i);
      return match ? `${Number(match[1])}x${Number(match[2])}` : null;
    };
    const wanted = episode(rest[rest.length - 1] ?? "");
    const series = (await this.call("series")).find((s: Row) => s.path === `${this.root}/${group}`);
    if (!wanted || !series) return null;
    const files = await this.call(`episodefile?seriesId=${series.id}`);
    const file = files.find((f: Row) => episode(f.relativePath.split("/").pop()) === wanted);
    return file ? `${group}/${file.relativePath}` : null;
  }

  /** The file a replaced item has now, relative to the library root, or null. */
  async currentFile(parent: number, items: number[]): Promise<string | null> {
    if (this.library === "tv") {
      const episode = await this.call(`episode/${items[0]}`);
      if (!episode.hasFile) return null;
      const file = await this.call(`episodefile/${episode.episodeFileId}`);
      return file.path.slice(this.root.length + 1);
    }
    const movie = await this.call(`movie/${parent}`);
    return movie.movieFile ? join(movie.path, movie.movieFile.relativePath).slice(this.root.length + 1) : null;
  }

  // The database hands a bigint back as a string, and a command's ids have to be numbers.
  rescan(parent: number | string) {
    const id = Number(parent);
    return this.command(this.library === "tv" ? { name: "RescanSeries", seriesId: id } : { name: "RescanMovie", movieId: id });
  }

  /** Queued, not waited for: a search can outlast command()'s wait, and
   * checkSearch follows what comes of it. */
  search(items: number[]) {
    return this.call("command", this.library === "tv" ? { name: "EpisodeSearch", episodeIds: items } : { name: "MoviesSearch", movieIds: items });
  }

  /** Whether an indexer has a release we'd take. With the file still there every
   * release is rejected for that alone ("Existing file meets cutoff"), which
   * won't apply once it's moved. */
  async hasRelease(items: number[]): Promise<boolean> {
    const query = this.library === "tv" ? `episodeId=${items[0]}` : `movieId=${items[0]}`;
    const releases = await this.call(`release?${query}`);
    return releases.some((r: Row) => (r.rejections ?? []).every((j: string) => /^Existing file/.test(j)));
  }

  async queued(parent: number, items: number[]): Promise<Row[]> {
    const details =
      this.library === "tv"
        ? await this.call(`queue/details?seriesId=${parent}&${items.map((e) => `episodeIds=${e}`).join("&")}`)
        : await this.call(`queue/details?movieId=${parent}`);
    return details ?? [];
  }

  async freeBytes(): Promise<number> {
    const [folder] = await this.call("rootfolder");
    return folder.freeSpace;
  }
}

/** Whether a file is there. Unlike existsSync, a NAS error (EIO, ESTALE) throws
 * rather than reading as a missing file. */
function exists(path: string): boolean {
  try {
    statSync(path);
    return true;
  } catch (e: any) {
    if (e.code === "ENOENT") return false;
    throw e;
  }
}

/** A rename that won't overwrite. A soft mount can report failure for a rename
 * that happened, so the result is what's on disk. */
async function move(from: string, to: string) {
  if (exists(to)) throw new Error(`already exists: ${to}`);
  mkdirSync(dirname(to), { recursive: true });
  try {
    await rename(from, to);
  } catch (e) {
    if (exists(from) || !exists(to)) throw e;
  }
}

async function update(id: number, fields: Row) {
  await db`update media_repair set ${db({ ...fields, updated_at: new Date() })} where id = ${id}`;
}

// JSON goes in as text cast to jsonb, as in checker.ts.
async function setQuarantined(repair: Row, list: string[]) {
  await db`update media_repair set quarantined = ${JSON.stringify(list)}::text::jsonb where id = ${repair.id}`;
  repair.quarantined = list;
}

async function needsDuck(id: number, note: string) {
  await update(id, { state: "attention", note });
}

async function quarantine(arr: Arr, repair: Row, rel: string, suffix = ""): Promise<void> {
  const name = `${QUARANTINE}/${rel}${suffix}`;
  const marker = arr.file(`${QUARANTINE}/.plexignore`);
  mkdirSync(dirname(marker), { recursive: true });
  if (!exists(marker)) writeFileSync(marker, "*\n");
  await move(arr.file(rel), arr.file(name));
  await setQuarantined(repair, [...repair.quarantined, name]);
}

async function removeOriginals(arrs: Record<Library, Arr>) {
  const rows = await db`
    select r.*, m.library from media_repair r join media_file m on m.id = r.media_file_id
    where r.state = 'replaced' and r.remove_requested_at is not null`;
  for (const repair of rows) {
    const arr = arrs[repair.library as Library];
    for (const name of repair.quarantined) {
      if (!name.startsWith(`${QUARANTINE}/`)) throw new Error(`not a quarantined path: ${name}`);
      const path = arr.file(name);
      if (exists(path)) await unlink(path);
    }
    await update(repair.id, { state: "removed", note: null });
  }
  return rows.length;
}

async function removeLeftovers(arrs: Record<Library, Arr>) {
  const rows = await db`
    select r.*, m.library from media_repair r join media_file m on m.id = r.media_file_id
    where r.state = 'untracked' and r.tracked_copy is not null and r.remove_requested_at is not null`;
  let removed = 0;
  for (const repair of rows) {
    const arr = arrs[repair.library as Library];
    const path = arr.file(repair.original_path);
    // Only as Duck saw it: still untracked, unchanged, and the tracked copy still there.
    const safe =
      !(await arr.lookup(repair.original_path)) &&
      (await arr.lookup(repair.tracked_copy)) &&
      exists(arr.file(repair.tracked_copy)) &&
      exists(path) &&
      statSync(path).size === Number(repair.original_size);
    if (!safe) {
      await update(repair.id, { remove_requested_at: null, note: "This changed since you asked, so it wasn't removed." });
      continue;
    }
    await unlink(path);
    await db`update media_file set status = 'missing', check_first = false where id = ${repair.media_file_id}`;
    await update(repair.id, { state: "removed", note: null });
    removed++;
  }
  return removed;
}

/** Queue a newly imported file for its decode, ahead of the rest. */
async function queueDecode(arr: Arr, repair: Row, rel: string) {
  const stat = statSync(arr.file(rel));
  const mtime = new Date(Math.floor(stat.mtimeMs / 1000) * 1000);
  const [file] = await db`
    insert into media_file (library, path, size, mtime, status, check_first)
    values (${arr.library}, ${rel}, ${stat.size}, ${mtime}, 'queued', true)
    on conflict (library, path) do update set
      check_first = true,
      status = case
        when media_file.size <> excluded.size or media_file.mtime <> excluded.mtime
          or media_file.status in ('missing', 'replace_requested') then 'queued'
        else media_file.status end,
      size = excluded.size,
      mtime = excluded.mtime
    returning id`;
  await update(repair.id, { state: "verifying", replacement_file_id: file.id, note: null });
}

async function retrySearch(arr: Arr, repair: Row) {
  await arr.rescan(repair.arr_parent_id);
  const rel = await arr.currentFile(repair.arr_parent_id, repair.arr_item_ids);
  if (rel) {
    // The original is in quarantine, so a file on disk here is a new one.
    if (exists(arr.file(rel))) {
      await queueDecode(arr, repair, rel);
      return;
    }
    // Its mount may not have seen the move yet, so this gets one more run.
    if (repair.note === STILL_LISTED) {
      await needsDuck(repair.id, STILL_LISTED);
      return;
    }
    throw new Error(STILL_LISTED);
  }
  await arr.search(repair.arr_item_ids);
  await update(repair.id, { state: "searching", search_started_at: new Date(), note: null });
}

async function checkDecoded(arr: Arr, repair: Row) {
  const [file] = await db`select id, path, status, findings from media_file where id = ${repair.replacement_file_id}`;
  if (file.status === "ok" || file.status === "minor" || file.status === "marked_good") {
    await update(repair.id, { state: "replaced", note: null });
    return;
  }
  if (file.status === "unclear") {
    await update(repair.id, { state: "replaced", note: "The new file's decode logged errors the checker doesn't recognize, so it's in To review." });
    return;
  }
  if (file.status !== "suspect") {
    if (file.status === "missing") await needsDuck(repair.id, `The replacement disappeared: ${file.path}`);
    return;
  }
  const attempts = repair.attempts + 1;
  if (attempts >= MAX_ATTEMPTS) {
    await update(repair.id, { attempts });
    await needsDuck(repair.id, `${attempts} replacements failed their decode. The latest is still in the library: ${file.path}`);
    return;
  }
  await quarantine(arr, repair, file.path, `.bad-${attempts}`);
  await db`update media_file set status = 'missing', check_first = false where id = ${file.id}`;
  // Back to searching first, so a failed rescan or search resumes from checkSearch.
  await update(repair.id, { state: "searching", attempts, replacement_file_id: null, search_started_at: null });
  await retrySearch(arr, repair);
}

/** Put the original back and try again in a week. Only into an empty slot. */
async function restore(arr: Arr, repair: Row, note: string) {
  const name = `${QUARANTINE}/${repair.original_path}`;
  // Already back if a run stopped after the move; skipped only then.
  if (exists(arr.file(name)) || !exists(arr.file(repair.original_path))) {
    await move(arr.file(name), arr.file(repair.original_path));
  }
  await arr.rescan(repair.arr_parent_id);
  await db`update media_file set status = 'replace_requested', check_first = false where id = ${repair.media_file_id}`;
  await setQuarantined(repair, repair.quarantined.filter((q: string) => q !== name));
  await update(repair.id, { state: "no_release", note, retry_at: new Date(Date.now() + RETRY_DAYS * 86_400_000) });
}

/** Finishes the move of a run that stopped partway, going by what's on disk.
 * Whether the search can go ahead. */
async function finishMove(arr: Arr, repair: Row): Promise<boolean> {
  const name = `${QUARANTINE}/${repair.original_path}`;
  if (!repair.quarantined.includes(name)) {
    if (exists(arr.file(name))) {
      await setQuarantined(repair, [...repair.quarantined, name]);
    } else {
      if (statSync(arr.file(repair.original_path)).size !== Number(repair.original_size)) {
        await needsDuck(repair.id, `The file changed before it could be moved, so it's still in the library: ${repair.original_path}`);
        return false;
      }
      await quarantine(arr, repair, repair.original_path);
    }
  }
  await db`update media_file set status = 'missing', check_first = false where id = ${repair.media_file_id}`;
  return true;
}

async function checkSearch(arr: Arr, repair: Row) {
  // A run that stopped before the search picks it up here, wherever the move got to.
  if (!repair.search_started_at) {
    if (await finishMove(arr, repair)) await retrySearch(arr, repair);
    return;
  }
  const rel = await arr.currentFile(repair.arr_parent_id, repair.arr_item_ids);
  if (rel) {
    await queueDecode(arr, repair, rel);
    return;
  }
  const hours = (Date.now() - new Date(repair.search_started_at).getTime()) / 3_600_000;
  const queue = await arr.queued(repair.arr_parent_id, repair.arr_item_ids);
  if (queue.length) {
    const stuck = queue.find((q) => q.trackedDownloadStatus === "warning" || q.status === "warning");
    const message = stuck?.statusMessages?.flatMap((m: Row) => m.messages)?.join(" ") || stuck?.errorMessage;
    if (hours >= SEARCH_STUCK_HOURS) {
      await needsDuck(repair.id, `Still in the download queue after ${SEARCH_STUCK_HOURS} hours${message ? `: ${message}` : ""}`);
    } else {
      await update(repair.id, { note: message || null });
    }
  } else if (hours >= SEARCH_IDLE_HOURS) {
    await restore(arr, repair, "The search didn't download anything, so the original went back.");
  }
}

/** The file's repair row, created or reused. */
async function openRepair(file: Row, fields: Row): Promise<Row> {
  const values = { ...fields, updated_at: new Date() };
  const [row] = file.repair_id
    ? await db`update media_repair set ${db(values)} where id = ${file.repair_id} returning *`
    : await db`
        insert into media_repair ${db({ media_file_id: file.id, original_path: file.path, original_size: Number(file.size), ...values })}
        returning *`;
  await db`update media_repair set original_findings = ${JSON.stringify(file.findings)}::text::jsonb where id = ${row.id}`;
  return row;
}

/** Whether this file's repair started. */
async function startOne(arr: Arr, file: Row): Promise<boolean> {
  const path = arr.file(file.path);
  const stat = exists(path) ? statSync(path) : null;
  const mtime = stat ? Math.floor(stat.mtimeMs / 1000) : 0;
  if (!stat || stat.size !== Number(file.size) || mtime !== Math.floor(new Date(file.mtime).getTime() / 1000)) {
    // It changed since the check, so the request no longer applies to it.
    await db`update media_file set status = 'queued' where id = ${file.id}`;
    return false;
  }
  const found = await arr.lookup(file.path);
  if (!found) {
    const app = arr.library === "tv" ? "Sonarr" : "Radarr";
    const tracked_copy = await arr.trackedCopy(file.path);
    await openRepair(file, {
      state: "untracked",
      tracked_copy,
      note: tracked_copy
        ? `${app} tracks a different copy: ${tracked_copy}`
        : `${app} doesn't track this file, so it can't download a replacement. It may be an extra, or a show or movie that isn't in ${app}.`,
    });
    return false;
  }
  if (!(await arr.hasRelease(found.items))) {
    await openRepair(file, {
      state: "no_release",
      note: "No usable release right now. Trying again in a week.",
      retry_at: new Date(Date.now() + RETRY_DAYS * 86_400_000),
    });
    return false;
  }
  // Written before the move, so a run that dies after it can't lose track of the file.
  const repair = await openRepair(file, {
    state: "searching",
    note: null,
    retry_at: null,
    search_started_at: null,
    arr_parent_id: found.parent,
  });
  await db`update media_repair set arr_item_ids = ${JSON.stringify(found.items)}::text::jsonb where id = ${repair.id}`;
  repair.arr_item_ids = found.items;
  await quarantine(arr, repair, file.path);
  await db`update media_file set status = 'missing', check_first = false where id = ${file.id}`;
  await retrySearch(arr, repair);
  return true;
}

async function start(arrs: Record<Library, Arr>, slots: number, errors: string[]) {
  const candidates = await db`
    select m.*, r.id as repair_id from media_file m
    left join media_repair r on r.media_file_id = m.id and r.state not in ('replaced', 'removed')
    where m.status = 'replace_requested'
      and (r.id is null or (r.state = 'no_release' and r.retry_at <= now()))
    order by m.reviewed_at
    limit 20`;
  if (!candidates.length || slots === 0) return { started: 0, waiting: "" };
  let free = await arrs.tv.freeBytes();
  let started = 0;
  for (const file of candidates) {
    if (started >= slots) break;
    const size = Number(file.size);
    // Downloading and importing across exports holds two copies for a while.
    if (free - 2 * size < MIN_FREE_BYTES) {
      return { started, waiting: `not enough free space: ${Math.round(free / 1e9)} GB` };
    }
    try {
      if (await startOne(arrs[file.library as Library], file)) {
        free -= size;
        started++;
      }
    } catch (e) {
      errors.push(`${file.path}: ${e instanceof Error ? e.message : String(e)}`);
    }
  }
  return { started, waiting: "" };
}

export async function main() {
  const r: any = await wmill.getResource("f/fleetops/db_mediacheck");
  // One connection: the advisory lock belongs to it.
  db = new SQL({ hostname: r.host, port: r.port, username: r.user, password: r.password, database: r.dbname, tls: false, max: 1 });
  try {
    const [{ locked }] = await db`select pg_try_advisory_lock(${LOCK_KEY}) as locked`;
    if (!locked) return { skipped: "another run is still going" };

    const [pending] = await db`
      select
        exists (select 1 from media_file where status = 'replace_requested') as requested,
        exists (select 1 from media_repair where state in ('searching', 'verifying')
          or (state in ('replaced', 'untracked') and remove_requested_at is not null)) as active`;
    if (!pending.requested && !pending.active) return { idle: true };

    const arrs: Record<Library, Arr> = {
      tv: new Arr("tv", "http://sonarr.sonarr:8989", await wmill.getVariable("f/mediacheck/sonarr_api_key"), "/media/TV", "/tv"),
      movies: new Arr("movies", "http://radarr.radarr:7878", await wmill.getVariable("f/mediacheck/radarr_api_key"), "/media/Movies", "/movies"),
    };

    const removed = (await removeOriginals(arrs)) + (await removeLeftovers(arrs));
    const errors: string[] = [];
    const active = await db`
      select r.*, m.library from media_repair r join media_file m on m.id = r.media_file_id
      where r.state in ('searching', 'verifying') order by r.started_at`;
    for (const repair of active) {
      const arr = arrs[repair.library as Library];
      try {
        if (repair.state === "verifying") await checkDecoded(arr, repair);
        else await checkSearch(arr, repair);
      } catch (e) {
        // Likely a NAS or API hiccup, and the next run tries again. It's shown in the app meanwhile.
        const message = e instanceof Error ? e.message : String(e);
        await update(repair.id, { note: message });
        errors.push(`${repair.original_path}: ${message}`);
      }
    }
    const [{ busy }] = await db`select count(*)::int as busy from media_repair where state in ('searching', 'verifying')`;
    const { started, waiting } = await start(arrs, Math.max(0, MAX_ACTIVE - busy), errors);
    return { removed, checked: active.length, started, waiting, errors };
  } finally {
    await db.close();
  }
}

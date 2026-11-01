// The media integrity queue's side of the powerspecnix checker (features/
// mediacheck.nix), which calls it with a token that can run only this script.
// One runnable with an `action`, like the inbox review backend.
//
// Actions:
// - sync { library, scanStarted, files: [{ path, size, mtime }] }: upsert one
//   batch of a library scan. New files, changed ones (size or mtime, epoch
//   seconds) and missing ones that came back are queued.
// - sync_done { library, scanStarted }: files of that library the scan didn't
//   see become missing.
// - claim { host }: the next file to check, or null: replacements waiting on
//   their decode first (f/mediacheck/repair), then files of a whole number of
//   MiB (a failed copy leaves that size, so they're likely truncated), then
//   newest first. A claim older than CLAIM_HOURS is taken over.
// - release { host }: hand that host's claims back, for a checker that was
//   stopped mid-file.
// - report { id, size, mtime, checkSeconds, duration, decoded, exitCode,
//   lineCount, log }: store a decode's result and its verdict. A file whose
//   size or mtime changed meanwhile is queued again instead.
// - reclassify {}: rerun f/mediacheck/classify on every stored log after a
//   rule change. Files marked good are left alone.
// - stats {}: file count per status and library.
//
// Schema: f/mediacheck/schema.pg.sql. Paths are relative to the library root.
// JSON goes in as text cast to jsonb: a bare ::jsonb makes Bun bind the
// already-serialised string as JSON again, storing a jsonb string.
import * as wmill from "windmill-client";
import { SQL } from "bun";
import { classify, type LogLine } from "./classify.ts";

const CLAIM_HOURS = 6;
const LIBRARIES = ["movies", "tv"];

type ScanFile = { path: string; size: number; mtime: number };

let db: SQL;

function library(name: unknown): string {
  if (!LIBRARIES.includes(String(name))) throw new Error(`unknown library: ${name}`);
  return String(name);
}

async function sync(args: { library: string; scanStarted: string; files: ScanFile[] }) {
  const lib = library(args.library);
  const seenAt = new Date(args.scanStarted);
  if (!args.files?.length) return { files: 0, new: 0, queued: 0 };
  const rows = args.files.map((f) => ({
    library: lib,
    path: f.path,
    size: f.size,
    mtime: new Date(f.mtime * 1000),
    seen_at: seenAt,
  }));
  const result: { inserted: boolean; status: string }[] = await db`
    insert into media_file ${db(rows, "library", "path", "size", "mtime", "seen_at")}
    on conflict (library, path) do update set
      seen_at = excluded.seen_at,
      status = case
        when media_file.size <> excluded.size or media_file.mtime <> excluded.mtime
          or media_file.status = 'missing' then 'queued'
        else media_file.status end,
      size = excluded.size,
      mtime = excluded.mtime
    returning (xmax::text = '0') as inserted, status`;
  return {
    files: result.length,
    new: result.filter((r) => r.inserted).length,
    queued: result.filter((r) => r.status === "queued").length,
  };
}

async function syncDone(args: { library: string; scanStarted: string }) {
  const lib = library(args.library);
  const missing = await db`
    update media_file set status = 'missing', claimed_by = null, claimed_at = null
    where library = ${lib} and seen_at < ${new Date(args.scanStarted)} and status <> 'missing'
    returning path`;
  return { missing: missing.map((r: { path: string }) => r.path) };
}

async function claim(args: { host: string }) {
  const [row] = await db`
    update media_file set status = 'checking', claimed_by = ${args.host}, claimed_at = now()
    where id = (
      select id from media_file
      where status = 'queued'
        or (status = 'checking' and claimed_at < now() - make_interval(hours => ${CLAIM_HOURS}))
      order by check_first desc, (size % 1048576 = 0) desc, mtime desc
      limit 1
      for update skip locked)
    returning id, library, path, size, extract(epoch from mtime)::bigint as mtime`;
  return row ? { ...row, mtime: Number(row.mtime), size: Number(row.size) } : null;
}

async function release(args: { host: string }) {
  const rows = await db`
    update media_file set status = 'queued', claimed_by = null, claimed_at = null
    where status = 'checking' and claimed_by = ${args.host}
    returning path`;
  return { released: rows.length };
}

type Report = {
  id: number;
  size: number;
  mtime: number;
  checkSeconds: number;
  duration: number | null;
  decoded: number | null;
  exitCode: number;
  lineCount: number;
  log: LogLine[];
};

async function report(r: Report) {
  const [row] = await db`select size, extract(epoch from mtime)::bigint as mtime from media_file where id = ${r.id}`;
  if (!row) throw new Error(`no media_file ${r.id}`);
  if (Number(row.size) !== r.size || Number(row.mtime) !== r.mtime) {
    await db`
      update media_file set status = 'queued', size = ${r.size}, mtime = ${new Date(r.mtime * 1000)},
        claimed_by = null, claimed_at = null
      where id = ${r.id}`;
    return { id: r.id, status: "queued", reason: "changed during the check" };
  }

  const findings = classify(r.log, { duration: r.duration, decoded: r.decoded, exitCode: r.exitCode });
  await db`
    update media_file set
      status = ${findings.verdict}, claimed_by = null, claimed_at = null, checked_at = now(), check_first = false,
      check_seconds = ${r.checkSeconds}, duration = ${r.duration}, decoded = ${r.decoded},
      exit_code = ${r.exitCode}, line_count = ${r.lineCount},
      log = ${JSON.stringify(r.log)}::text::jsonb, findings = ${JSON.stringify(findings)}::text::jsonb
    where id = ${r.id}`;
  return { id: r.id, status: findings.verdict, damage: findings.damage, unknown: findings.unknown };
}

async function reclassify() {
  const rows = await db`
    select id, log, duration, decoded, exit_code from media_file
    where status in ('ok', 'minor', 'suspect', 'unclear') and log is not null`;
  const changed: Record<string, number> = {};
  for (const row of rows) {
    const findings = classify(row.log, { duration: row.duration, decoded: row.decoded, exitCode: row.exit_code ?? 0 });
    const [before] = await db`
      update media_file m set status = ${findings.verdict}, findings = ${JSON.stringify(findings)}::text::jsonb
      from (select status from media_file where id = ${row.id}) old
      where m.id = ${row.id}
      returning old.status as old`;
    if (before.old !== findings.verdict) {
      const key = `${before.old} -> ${findings.verdict}`;
      changed[key] = (changed[key] ?? 0) + 1;
    }
  }
  return { files: rows.length, changed };
}

async function stats() {
  return db`select library, status, count(*)::int as files from media_file group by library, status order by library, status`;
}

export async function main(action: string, args: Record<string, any> = {}) {
  const r: any = await wmill.getResource("f/fleetops/db_mediacheck");
  db = new SQL({ hostname: r.host, port: r.port, username: r.user, password: r.password, database: r.dbname, tls: false });
  try {
    switch (action) {
      case "sync":
        return await sync(args as Parameters<typeof sync>[0]);
      case "sync_done":
        return await syncDone(args as Parameters<typeof syncDone>[0]);
      case "claim":
        return await claim(args as Parameters<typeof claim>[0]);
      case "release":
        return await release(args as Parameters<typeof release>[0]);
      case "report":
        return await report(args as Report);
      case "reclassify":
        return await reclassify();
      case "stats":
        return await stats();
      default:
        throw new Error(`unknown action: ${action}`);
    }
  } finally {
    await db.close();
  }
}

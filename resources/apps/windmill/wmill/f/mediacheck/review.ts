// The review side of the media integrity queue, for the inbox review app's
// Media tab. Separate from f/mediacheck/checker so the powerspecnix token, which
// can run only that script, can't change a review decision.
//
// Actions:
// - list { view }: files waiting on a decision (view "open"), those already
//   decided ("replace", "good"), or those with a healthy replacement whose
//   original is still kept ("replaced"). Suspect files first, then unclear,
//   then minor.
// - get { id }: one file with its error log.
// - decide { id, decision }: "good" keeps the file and stops flagging it until
//   it changes, "replace" asks for a replacement, "reopen" undoes either one
//   until the repair has moved the file. "remove_original" has the next repair
//   run delete a replaced file's quarantined copies, and "remove_leftover" an
//   untracked file that has a tracked copy. "recheck" puts a file waiting on
//   review back in the checker's queue, ahead of the rest, for a decode that
//   crashed or was cut short for some reason other than the file.
//
// Nothing here touches the files. f/mediacheck/repair acts on these decisions.
import * as wmill from "windmill-client";
import { SQL } from "bun";

const VIEWS: Record<string, string[]> = {
  open: ["suspect", "unclear", "minor"],
  replace: ["replace_requested"],
  good: ["marked_good"],
};

let db: SQL;

type Row = Record<string, any>;

function toFile(row: Row) {
  const findings = row.findings ?? {};
  return {
    id: Number(row.id),
    library: row.library,
    path: row.path,
    status: row.status,
    size: Number(row.size),
    duration: row.duration,
    decoded: row.decoded,
    checkedAt: row.checked_at,
    lineCount: row.line_count,
    verdict: findings.verdict,
    damage: findings.damage ?? 0,
    unknown: findings.unknown ?? 0,
    spots: findings.spots ?? [],
    unknownSamples: findings.unknownSamples ?? [],
    truncated: findings.truncated,
    fatal: findings.fatal,
    ...(row.log ? { log: row.log } : {}),
    ...(row.repair_state
      ? {
          repair: {
            state: row.repair_state,
            note: row.repair_note ?? undefined,
            attempts: row.repair_attempts,
            quarantined: row.repair_quarantined ?? [],
            trackedCopy: row.tracked_copy ?? undefined,
            replacement: row.replacement_path ?? undefined,
            replacementVerdict: row.replacement_status ?? undefined,
            removing: row.remove_requested_at !== null,
          },
        }
      : {}),
  };
}

// A file's latest repair, with the findings from before any replacement took
// its row. A replacement at a new path has a row of its own, which stays out of
// the review list until its repair is done with it.
const OPEN_REPAIR_STATES = ["searching", "verifying", "no_release", "attention", "untracked"];

async function list(args: { view?: string }) {
  const view = args.view ?? "open";
  if (view === "replaced") {
    return (await repairRows(db`r.state = 'replaced'`)).map(toFile);
  }
  if (view === "replace") {
    return (await repairRows(db`r.state in ${db(OPEN_REPAIR_STATES)} or m.status = 'replace_requested'`)).map(toFile);
  }
  const statuses = VIEWS[view];
  if (!statuses) throw new Error(`unknown view: ${args.view}`);
  const rows = await db`
    select m.id, m.library, m.path, m.status, m.size, m.duration, m.decoded, m.checked_at, m.line_count, m.findings
    from media_file m
    where m.status in ${db(statuses)}
      and not exists (select 1 from media_repair r where r.replacement_file_id = m.id and r.state in ('searching', 'verifying'))
    order by array_position(array['suspect', 'unclear', 'minor']::text[], m.status), m.library, m.path`;
  return rows.map(toFile);
}

function repairRows(where: unknown): Promise<Row[]> {
  return db`
    select distinct on (m.id)
      m.id, m.library, coalesce(r.original_path, m.path) as path, m.status,
      coalesce(r.original_size, m.size) as size, m.duration, m.decoded, m.checked_at, m.line_count,
      coalesce(r.original_findings, m.findings) as findings,
      r.state as repair_state, r.note as repair_note, r.attempts as repair_attempts,
      r.quarantined as repair_quarantined, r.remove_requested_at, r.tracked_copy,
      n.path as replacement_path, n.status as replacement_status
    from media_file m
    left join media_repair r on r.media_file_id = m.id
    left join media_file n on n.id = r.replacement_file_id
    where ${where}
    order by m.id, r.started_at desc`;
}

async function get(args: { id: number }) {
  const [row] = await db`
    select m.id, m.library, coalesce(r.original_path, m.path) as path, m.status,
      coalesce(r.original_size, m.size) as size, m.duration, m.decoded, m.checked_at, m.line_count,
      coalesce(r.original_findings, m.findings) as findings, m.log,
      r.state as repair_state, r.note as repair_note, r.attempts as repair_attempts,
      r.quarantined as repair_quarantined, r.remove_requested_at, r.tracked_copy,
      n.path as replacement_path, n.status as replacement_status
    from media_file m
    left join lateral (
      select * from media_repair where media_file_id = m.id order by started_at desc limit 1
    ) r on true
    left join media_file n on n.id = r.replacement_file_id
    where m.id = ${args.id}`;
  if (!row) throw new Error(`no media_file ${args.id}`);
  return toFile(row);
}

async function decide(args: { id: number; decision: string }) {
  if (args.decision === "remove_original") {
    const removing = await db`
      update media_repair set remove_requested_at = now(), updated_at = now()
      where id = (select id from media_repair where media_file_id = ${args.id} and state = 'replaced' order by started_at desc limit 1)
      returning id`;
    if (!removing.length) throw new Error(`media_file ${args.id} has no replaced original`);
    return get({ id: args.id });
  }
  if (args.decision === "remove_leftover") {
    const removing = await db`
      update media_repair set remove_requested_at = now(), updated_at = now()
      where media_file_id = ${args.id} and state = 'untracked' and tracked_copy is not null
      returning id`;
    if (!removing.length) throw new Error(`media_file ${args.id} isn't a leftover`);
    return get({ id: args.id });
  }
  if (args.decision === "recheck") {
    const rows = await db`
      update media_file set status = 'queued', check_first = true, claimed_by = null, claimed_at = null, reviewed_at = null
      where id = ${args.id} and status in ${db(VIEWS.open)}
      returning id`;
    if (!rows.length) throw new Error(`media_file ${args.id} isn't waiting on review`);
    return get({ id: args.id });
  }
  // Once the repair has moved the file, it has to run its course.
  const [moving] = await db`
    select 1 from media_repair where media_file_id = ${args.id} and state in ('searching', 'verifying', 'attention')`;
  if (moving) throw new Error(`media_file ${args.id} is being replaced, so it can't take that decision now`);
  await db`delete from media_repair where media_file_id = ${args.id} and state in ('no_release', 'untracked')`;
  const decided = [...VIEWS.replace, ...VIEWS.good];
  const rows =
    args.decision === "good" || args.decision === "replace"
      ? await db`
          update media_file set reviewed_at = now(),
            status = ${args.decision === "good" ? "marked_good" : "replace_requested"}
          where id = ${args.id} and status in ${db([...VIEWS.open, ...decided])}
          returning id`
      : args.decision === "reopen"
        ? await db`
            update media_file set reviewed_at = null, status = findings->>'verdict'
            where id = ${args.id} and status in ${db(decided)} and findings->>'verdict' is not null
            returning id`
        : null;
  if (!rows) throw new Error(`unknown decision: ${args.decision}`);
  // A file that changed meanwhile went back to the queue, and needs a new check first.
  if (!rows.length) throw new Error(`media_file ${args.id} can't take that decision now`);
  return get({ id: args.id });
}

export async function main(action: string, args: Record<string, any> = {}) {
  const r: any = await wmill.getResource("f/fleetops/db_mediacheck");
  db = new SQL({ hostname: r.host, port: r.port, username: r.user, password: r.password, database: r.dbname, tls: false });
  try {
    switch (action) {
      case "list":
        return await list(args);
      case "get":
        return await get(args as Parameters<typeof get>[0]);
      case "decide":
        return await decide(args as Parameters<typeof decide>[0]);
      default:
        throw new Error(`unknown action: ${action}`);
    }
  } finally {
    await db.close();
  }
}

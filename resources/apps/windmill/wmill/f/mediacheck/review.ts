// The review side of the media integrity queue, for the inbox review app's
// Media tab. Separate from f/mediacheck/checker so the powerspecnix token, which
// can run only that script, can't change a review decision.
//
// Actions:
// - list { view }: files waiting on a decision (view "open"), or those already
//   decided ("replace", "good"). Suspect files first, then unclear, then minor.
// - get { id }: one file with its error log.
// - decide { id, decision }: "good" keeps the file and stops flagging it until
//   it changes, "replace" asks for a replacement, "reopen" undoes either one.
//
// Nothing here touches the files. A replacement request only records the
// decision; acting on it is a separate step.
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
  };
}

async function list(args: { view?: string }) {
  const statuses = VIEWS[args.view ?? "open"];
  if (!statuses) throw new Error(`unknown view: ${args.view}`);
  const rows = await db`
    select id, library, path, status, size, duration, decoded, checked_at, line_count, findings
    from media_file
    where status in ${db(statuses)}
    order by array_position(array['suspect', 'unclear', 'minor']::text[], status), library, path`;
  return rows.map(toFile);
}

async function get(args: { id: number }) {
  const [row] = await db`
    select id, library, path, status, size, duration, decoded, checked_at, line_count, findings, log
    from media_file where id = ${args.id}`;
  if (!row) throw new Error(`no media_file ${args.id}`);
  return toFile(row);
}

async function decide(args: { id: number; decision: string }) {
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

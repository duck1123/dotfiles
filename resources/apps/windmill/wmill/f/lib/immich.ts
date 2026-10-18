// Shared Immich helpers for Windmill scripts and apps. Import with an absolute
// path:
//
//   import { connect, openDb } from "/f/lib/immich.ts";
//
// connect() uses f/fleetops/immich_base_url and the photo tag key
// (f/phototags/immich_api_key: tag read/create/update/asset, asset read; no
// delete). openDb() is a read-only connection to Immich's own database
// (f/fleetops/db_immich), for exact counts the API doesn't give: its asset
// search filters by visibility and its tag/page parameters are deprecated
// since v3.2. Never write to that database; changes go through the API.
//
// main() is a diagnostic: server version, tag count and tag assignments.
import * as wmill from "windmill-client";
import { SQL } from "bun";

export type Tag = { id: string; name: string; value: string; parentId?: string | null; color?: string | null };

export type Immich = ReturnType<typeof client>;

export async function connect(): Promise<Immich> {
  return client(
    await wmill.getVariable("f/fleetops/immich_base_url"),
    await wmill.getVariable("f/phototags/immich_api_key"),
  );
}

export function client(base: string, key: string) {
  async function api(method: string, path: string, body?: unknown): Promise<any> {
    const res = await fetch(`${base}/api${path}`, {
      method,
      headers: { "x-api-key": key, ...(body === undefined ? {} : { "Content-Type": "application/json" }) },
      body: body === undefined ? undefined : JSON.stringify(body),
    });
    if (!res.ok) throw new Error(`Immich ${method} ${path}: ${res.status} ${await res.text()}`);
    const text = await res.text();
    return text ? JSON.parse(text) : null;
  }

  const tags = (): Promise<Tag[]> => api("GET", "/tags");
  const version = async (): Promise<string> => {
    const v = await api("GET", "/server/version");
    return `${v.major}.${v.minor}.${v.patch}`;
  };

  return { api, tags, version };
}

/** Read-only connection to Immich's database. Close it when done. */
export async function openDb(): Promise<SQL> {
  const r: any = await wmill.getResource("f/fleetops/db_immich");
  return new SQL({ hostname: r.host, port: r.port, username: r.user, password: r.password, database: r.dbname, tls: false });
}

/** Asset IDs per tag ID, straight from Immich's tag_asset table. */
export async function tagAssets(db: SQL): Promise<Map<string, string[]>> {
  const rows: { tagId: string; assetIds: string[] }[] =
    await db`select "tagId", array_agg("assetId"::text order by "assetId") as "assetIds" from tag_asset group by "tagId"`;
  return new Map(rows.map((r) => [r.tagId, r.assetIds]));
}

export async function main() {
  const immich = await connect();
  const db = await openDb();
  try {
    const [tags, assets, version] = await Promise.all([immich.tags(), tagAssets(db), immich.version()]);
    let assignments = 0;
    for (const ids of assets.values()) assignments += ids.length;
    return { version, tags: tags.length, taggedTags: assets.size, assignments };
  } finally {
    await db.close();
  }
}

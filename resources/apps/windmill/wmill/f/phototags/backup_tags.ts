// Back up every Immich tag with the assets carrying it, as one JSON file on the
// NAS: <dir>/<day>/tags-<day>-<HHMM>.json. Run before the first sync apply
// and before any bulk change to tags (Photo Tagging D5, phase 3).
//
// Tags (id, name, value, parent, colour) come from the API; assignments come
// from Immich's own tag_asset table, read-only, so archived, hidden and locked
// photos are included. The two are cross-checked, the file is read back and
// its counts compared, and the result is returned. Nothing in Immich changes.
//
// Restoring is by hand for now: recreate a tag (POST /tags), then put its
// assets back (PUT /tags/assets).
import { mkdir } from "node:fs/promises";
import { connect, openDb, tagAssets } from "/f/lib/immich.ts";
import { localStamp } from "/f/lib/trilium.ts";

export async function main(dir: string = "/media/Photos/_backups/phototags", dry_run: boolean = false) {
  const immich = await connect();
  const db = await openDb();
  let tags, assets, version;
  try {
    [tags, assets, version] = await Promise.all([immich.tags(), tagAssets(db), immich.version()]);
  } finally {
    await db.close();
  }

  const tagIds = new Set(tags.map((t) => t.id));
  const assignedToUnknownTag = [...assets.keys()].filter((id) => !tagIds.has(id));
  const entries = tags.map((t) => ({
    id: t.id,
    name: t.name,
    value: t.value,
    parentId: t.parentId ?? null,
    color: t.color ?? null,
    assetIds: assets.get(t.id) ?? [],
  }));
  const counts = {
    tags: entries.length,
    tagsWithAssets: entries.filter((e) => e.assetIds.length).length,
    assignments: entries.reduce((n, e) => n + e.assetIds.length, 0),
    assets: new Set(entries.flatMap((e) => e.assetIds)).size,
  };

  const now = new Date();
  const { day, created } = localStamp(now);
  const path = `${dir}/${day}/tags-${created.replace(" ", "-").replace(":", "")}.json`;
  const result = { path, immichVersion: version, counts, assignedToUnknownTag };
  if (dry_run) return { status: "would write", ...result };

  const body = JSON.stringify({ createdAt: now.toISOString(), immichVersion: version, counts, tags: entries }, null, 1);
  await mkdir(`${dir}/${day}`, { recursive: true });
  await Bun.write(path, body);

  const back = JSON.parse(await Bun.file(path).text());
  const readBack = {
    tags: back.tags.length,
    assignments: back.tags.reduce((n: number, e: any) => n + e.assetIds.length, 0),
  };
  if (readBack.tags !== counts.tags || readBack.assignments !== counts.assignments) {
    throw new Error(`backup ${path} doesn't match: wrote ${JSON.stringify(counts)}, read ${JSON.stringify(readBack)}`);
  }
  return { status: "written", bytes: body.length, ...result };
}

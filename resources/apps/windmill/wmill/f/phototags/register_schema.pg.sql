-- database f/fleetops/db_phototags
-- Schema of the legacy tag register (Photo Tagging S1, D10/D11/D14).
-- Idempotent: safe to run again; it never drops or rewrites data.
--
-- legacy_assignment: one row per legacy tag on one file or asset, with its
--   source. Loaded in S2 from the digiKam DB copy, Immich's tag_asset (sidecar
--   keywords) and the Takeout people JSON.
-- legacy_tag_map: one row per distinct legacy tag, mapped in S3 to a Trilium
--   note, kept under _legacy/..., or skipped. Pre-filled with suggestions;
--   only reviewed rows are applied (S4).

create table if not exists legacy_assignment (
  id          bigserial primary key,
  source      text not null check (source in ('digikam', 'immich', 'takeout')),
  legacy_tag  text not null,          -- full path as the source had it, e.g. People/Some Name
  file_path   text,                   -- relative to the Photos share root
  asset_id    uuid,                   -- Immich asset, once matched
  status      text not null default 'pending'
              check (status in ('pending', 'applied', 'skipped', 'unmatched')),
  loaded_at   timestamptz not null default now(),
  applied_at  timestamptz,
  unique nulls not distinct (source, legacy_tag, file_path, asset_id)
);

create index if not exists legacy_assignment_tag on legacy_assignment (legacy_tag);
create index if not exists legacy_assignment_asset on legacy_assignment (asset_id);

create table if not exists legacy_tag_map (
  legacy_tag        text primary key,
  target            text check (target in ('note', 'legacy', 'skip')),
  note_id           text,               -- Trilium noteId when target = 'note'
  legacy_path       text,               -- e.g. _legacy/nsfw/nudity when target = 'legacy'
  suggested_note_id text,               -- pre-filled from title/alias matching
  suggestion        text,               -- how it was suggested: exact, alias, fuzzy
  reviewed          boolean not null default false,
  reviewed_at       timestamptz,
  comment           text,
  check (target is distinct from 'note' or note_id is not null),
  check (target is distinct from 'legacy' or legacy_path like '\_legacy/%')
);

select
  (select count(*) from legacy_assignment) as legacy_assignments,
  (select count(*) from legacy_tag_map) as legacy_tags;

-- database f/fleetops/db_mediacheck
-- Schema of the media integrity queue (Trilium ticket P9NyDWbiY6JK).
-- Idempotent: safe to run again; it never drops or rewrites data.
--
-- media_file: one row per video file on the NAS, keyed by library and its path
--   inside that library (powerspecnix sees /mnt/movies, the Windmill workers
--   /media/Movies). The checker on powerspecnix fills it by scanning (sync),
--   then claims queued rows one at a time, decodes them in full and reports
--   ffmpeg's error output, which f/mediacheck/checker sorts into a verdict.
--
-- Statuses:
--   queued    new or changed since its last check
--   checking  claimed by a checker (claimed_by, claimed_at)
--   ok        no damage found
--   minor     a glitch or two (see classify.ts); waits for review, after suspect
--   suspect   damage found; waits for review
--   unclear   only errors the rules don't know; waits for a rule or review
--   marked_good  reviewed and kept; not flagged again until the file changes
--   missing   not seen by the latest scan of its library

create table if not exists media_file (
  id            bigserial primary key,
  library       text not null check (library in ('movies', 'tv')),
  path          text not null,          -- relative to the library root
  size          bigint not null,
  mtime         timestamptz not null,
  status        text not null default 'queued',
  seen_at       timestamptz not null default now(),   -- latest scan that saw it
  claimed_by    text,
  claimed_at    timestamptz,
  checked_at    timestamptz,
  check_seconds real,                   -- wall time of the decode
  duration      real,                   -- container duration, seconds
  decoded       real,                   -- how far the decode got, seconds
  exit_code     integer,
  line_count    integer,                -- ffmpeg error lines, before any cap
  log           jsonb,                  -- [{t, msg}], capped by the checker
  findings      jsonb,                  -- the classifier's summary
  reviewed_at   timestamptz,
  unique (library, path)
);

alter table media_file drop constraint if exists media_file_status_check;
alter table media_file add constraint media_file_status_check
  check (status in ('queued', 'checking', 'ok', 'minor', 'suspect', 'unclear', 'marked_good', 'missing'));

create index if not exists media_file_queue on media_file (status, mtime desc);

select status, count(*) as files from media_file group by status order by status;

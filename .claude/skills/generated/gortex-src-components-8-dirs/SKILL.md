---
name: gortex-src-components-8-dirs
description: "Work in the src/components +8 dirs area — 265 symbols across 20 files (91% cohesion)"
---

# src/components +8 dirs

265 symbols | 20 files | 91% cohesion

## When to Use

Use this skill when working on files in:
- ``
- `external-call::stdlib:windmill-client`
- `resources/apps/dashboard-ui/src/components/CaptureBox.tsx`
- `resources/apps/dashboard-ui/src/components/InboxReview.tsx`
- `resources/apps/dashboard-ui/src/components/NoteChip.tsx`
- `resources/apps/dashboard-ui/src/components/NotePicker.stories.tsx`
- `resources/apps/dashboard-ui/src/components/NotePicker.test.tsx`
- `resources/apps/dashboard-ui/src/components/NotePicker.tsx`
- `resources/apps/dashboard-ui/src/components/ReviewItem.tsx`
- `resources/apps/dashboard-ui/src/components/ReviewQueue.tsx`
- `resources/apps/dashboard-ui/src/inbox/mockInboxSource.ts`
- `resources/apps/dashboard-ui/src/inbox/types.ts`
- `resources/apps/dashboard-ui/src/notes/mockSource.ts`
- `resources/apps/dashboard-ui/src/notes/types.ts`
- `resources/apps/windmill/wmill/f/fleetops/youtube_ingest.ts`
- `resources/apps/windmill/wmill/f/lib/immich.ts`
- `resources/apps/windmill/wmill/f/lib/trilium.ts`
- `resources/apps/windmill/wmill/f/phototags/backup_tags.ts`
- `resources/apps/windmill/wmill/u/duck/inbox_review.raw_app/App.tsx`
- `resources/apps/windmill/wmill/u/duck/inbox_review.raw_app/backend/trilium.ts`

## Key Files

| File | Symbols |
|------|---------|
| `` | addEventListener, reject, includes, map, some, ... |
| `external-call::stdlib:windmill-client` | windmill-client |
| `resources/apps/dashboard-ui/src/components/CaptureBox.tsx` | CaptureBoxProps |
| `resources/apps/dashboard-ui/src/components/InboxReview.tsx` | save, items, setCapturing, InboxReviewProps, setSelectedId, ... |
| `resources/apps/dashboard-ui/src/components/NoteChip.tsx` | NoteChipProps, NoteChip, path |
| `resources/apps/dashboard-ui/src/components/NotePicker.stories.tsx` | value, setValue, props, args, Stateful, ... |
| `resources/apps/dashboard-ui/src/components/NotePicker.test.tsx` | args, setValue, text, value, source.search, ... |
| `resources/apps/dashboard-ui/src/components/NotePicker.tsx` | known, NotePickerProps, optionId, noun, open, ... |
| `resources/apps/dashboard-ui/src/components/ReviewItem.tsx` | err, notesChanged, ReviewItemProps, setTags, kindOptions, ... |
| `resources/apps/dashboard-ui/src/components/ReviewQueue.tsx` | host, ReviewQueue, day, created, url |
| `resources/apps/dashboard-ui/src/inbox/mockInboxSource.ts` | MockInboxOptions |
| `resources/apps/dashboard-ui/src/inbox/types.ts` | InboxSource, InboxItem, ReviewChanges |
| `resources/apps/dashboard-ui/src/notes/mockSource.ts` | ms, searchOptions, delay, search, noteUrl, ... |
| `resources/apps/dashboard-ui/src/notes/types.ts` | NoteSource, NoteSummary, NoteScope, SearchOptions |
| `resources/apps/windmill/wmill/f/fleetops/youtube_ingest.ts` | Attr |
| `resources/apps/windmill/wmill/f/lib/immich.ts` | version, base, key, Immich, client, ... |
| `resources/apps/windmill/wmill/f/lib/trilium.ts` | visiting, EtapiNote, localStamp, noteIds, paths, ... |
| `resources/apps/windmill/wmill/f/phototags/backup_tags.ts` | main, tagIds, entries, counts, dry_run, ... |
| `resources/apps/windmill/wmill/u/duck/inbox_review.raw_app/App.tsx` | App |
| `resources/apps/windmill/wmill/u/duck/inbox_review.raw_app/backend/trilium.ts` | q, limit, noteIds, listInbox, paras, ... |

## Entry Points

- `resources/apps/dashboard-ui/src/components/NotePicker.tsx::NotePicker`
- `resources/apps/windmill/wmill/f/phototags/backup_tags.ts::main`
- `resources/apps/dashboard-ui/src/components/ReviewItem.tsx::ReviewItem`
- `resources/apps/dashboard-ui/src/components/InboxReview.tsx::InboxReview`

## Connected Communities

- **. +2 dirs** (3 cross-edges)
- **. +1 dirs · linkTitle** (1 cross-edges)
- **src/inbox** (1 cross-edges)

## How to Explore

```
analyze(operation:"communities", id:"community-3")
explore(operation:"context", task:"understand src/components +8 dirs", format:"gcx")
relations(operation:"usages", target:{symbol:"resources/apps/dashboard-ui/src/components/NotePicker.tsx::NotePicker"}, format:"gcx")
```

_`format: "gcx"` returns the [GCX1 compact wire format](../../docs/wire-format.md) — round-trippable, ~27% fewer tokens than JSON. Drop it for JSON output; agents using `@gortex/wire` or the Go `github.com/gortexhq/gcx-go` package decode either._

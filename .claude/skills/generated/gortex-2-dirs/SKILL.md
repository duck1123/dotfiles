---
name: gortex-2-dirs
description: "Work in the . +2 dirs area — 149 symbols across 3 files (93% cohesion)"
---

# . +2 dirs

149 symbols | 3 files | 93% cohesion

## When to Use

Use this skill when working on files in:
- ``
- `resources/apps/windmill/wmill/f/fleetops/youtube_ingest.ts`
- `resources/apps/windmill/wmill/f/lib/trilium.ts`

## Key Files

| File | Symbols |
|------|---------|
| `` | startsWith, forEach, resolve |
| `resources/apps/windmill/wmill/f/fleetops/youtube_ingest.ts` | c, cr, url, u, s, ... |
| `resources/apps/windmill/wmill/f/lib/trilium.ts` | etapi, method, text, body, res, ... |

## Connected Communities

- **src/components +8 dirs** (16 cross-edges)
- **. +1 dirs · linkTitle** (2 cross-edges)
- **. +1 dirs · main** (1 cross-edges)

## How to Explore

```
analyze(operation:"communities", id:"community-223")
explore(operation:"context", task:"understand . +2 dirs", format:"gcx")
```

_`format: "gcx"` returns the [GCX1 compact wire format](../../docs/wire-format.md) — round-trippable, ~27% fewer tokens than JSON. Drop it for JSON output; agents using `@gortex/wire` or the Go `github.com/gortexhq/gcx-go` package decode either._

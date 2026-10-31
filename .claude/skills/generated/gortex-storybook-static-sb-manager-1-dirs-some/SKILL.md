---
name: gortex-storybook-static-sb-manager-1-dirs-some
description: "Work in the storybook-static/sb-manager +1 dirs · some area — 77 symbols across 3 files (69% cohesion)"
---

# storybook-static/sb-manager +1 dirs · some

77 symbols | 3 files | 69% cohesion

## When to Use

Use this skill when working on files in:
- ``
- `resources/apps/dashboard-ui/storybook-static/sb-manager/globals-runtime.js`
- `resources/apps/dashboard-ui/storybook-static/sb-manager/runtime.js`

## Key Files

| File | Symbols |
|------|---------|
| `` | fill, every, some |
| `resources/apps/dashboard-ui/storybook-static/sb-manager/globals-runtime.js` | toHaveClass, isObject, toHaveStyle, assertKeys, condition, ... |
| `resources/apps/dashboard-ui/storybook-static/sb-manager/runtime.js` | compareNumbers2, validateSteps, equal, compareNumbers, equalArrayBuffer2, ... |

## Entry Points

- `resources/apps/dashboard-ui/storybook-static/sb-manager/globals-runtime.js::assertKeys`

## Connected Communities

- **storybook-static/sb-manager +1 dirs · forEach** (19 cross-edges)
- **storybook-static/sb-manager +1 dirs · has** (10 cross-edges)
- **storybook-static/sb-manager +1 dirs · includes** (8 cross-edges)
- **storybook-static/sb-manager +1 dirs · Downshift3** (8 cross-edges)
- **storybook-static/sb-manager +1 dirs · _addIssue** (7 cross-edges)
- **storybook-static/sb-manager +1 dirs · push** (6 cross-edges)
- **. +1 dirs · assert** (6 cross-edges)
- **storybook-static/sb-manager +1 dirs · getAttribute** (4 cross-edges)
- **storybook-static/sb-manager +1 dirs · value** (3 cross-edges)
- **storybook-static/sb-manager +1 dirs · map** (2 cross-edges)
- **storybook-static/sb-manager · match3** (2 cross-edges)
- **storybook-static/sb-manager +1 dirs · concat** (2 cross-edges)
- **storybook-static/sb-manager · createElement** (1 cross-edges)
- **storybook-static/sb-manager · store2.getState** (1 cross-edges)
- **storybook-static/sb-manager · eofRule** (1 cross-edges)
- **storybook-static/sb-manager +1 dirs · slice** (1 cross-edges)
- **storybook-static/sb-manager · getOwnEnumerableProperties** (1 cross-edges)
- **storybook-static/sb-manager +1 dirs · join** (1 cross-edges)
- **storybook-static/sb-manager +1 dirs · get** (1 cross-edges)

## How to Explore

```
analyze(operation:"communities", id:"community-215")
explore(operation:"context", task:"understand storybook-static/sb-manager +1 dirs · some", format:"gcx")
relations(operation:"usages", target:{symbol:"resources/apps/dashboard-ui/storybook-static/sb-manager/globals-runtime.js::assertKeys"}, format:"gcx")
```

_`format: "gcx"` returns the [GCX1 compact wire format](../../docs/wire-format.md) — round-trippable, ~27% fewer tokens than JSON. Drop it for JSON output; agents using `@gortex/wire` or the Go `github.com/gortexhq/gcx-go` package decode either._

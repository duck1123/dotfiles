import { useCallback, useEffect, useMemo, useState } from "react";
import type { ModChanges, ModList, ModSource, ModSummary } from "../mods/types";
import { ModDetail } from "./ModDetail";
import "./ModBrowser.css";

export type ModFilter = "all" | "enabled" | "disabled" | "no-page" | "problems";

const filters: { value: ModFilter; label: string }[] = [
  { value: "all", label: "All mods" },
  { value: "enabled", label: "Enabled" },
  { value: "disabled", label: "Not enabled" },
  { value: "no-page", label: "No download page" },
  { value: "problems", label: "With problems" },
];

export interface ModBrowserProps {
  source: ModSource;
  /** Profile to show first (default: the source's). */
  profile?: string;
  /** Mod to select first. */
  selectedId?: string;
  /** Called when the selection changes, e.g. to keep it in the URL. */
  onSelect?: (id: string) => void;
}

/** Whether `mod` matches every word of `query` (in its id, title or tags). */
export function matchesQuery(mod: ModSummary, query: string): boolean {
  const hay = [mod.id, mod.title ?? "", ...mod.tags].join(" ").toLowerCase();
  return query
    .toLowerCase()
    .split(/\s+/)
    .filter(Boolean)
    .every((w) => hay.includes(w));
}

function matchesFilter(mod: ModSummary, filter: ModFilter): boolean {
  switch (filter) {
    case "enabled":
      return mod.enable;
    case "disabled":
      return !mod.enable;
    case "no-page":
      return mod.downloadPage === null;
    case "problems":
      return mod.problems.length > 0;
    default:
      return true;
  }
}

/** Browse the mod catalog: search and filter on the left, the selected mod on the right. */
export function ModBrowser({ source, profile: initialProfile, selectedId: initialId, onSelect }: ModBrowserProps) {
  const [profile, setProfile] = useState(initialProfile);
  const [list, setList] = useState<ModList>();
  const [status, setStatus] = useState<"loading" | "ready" | "error">("loading");
  const [error, setError] = useState("");
  const [query, setQuery] = useState("");
  const [filter, setFilter] = useState<ModFilter>("all");
  const [tag, setTag] = useState("");
  const [selectedId, setSelectedId] = useState(initialId);

  const load = useCallback(() => {
    const ctl = new AbortController();
    setStatus("loading");
    source
      .list(profile, ctl.signal)
      .then((l) => {
        setList(l);
        setStatus("ready");
      })
      .catch((e: unknown) => {
        if (ctl.signal.aborted) {
          return;
        }
        setError(e instanceof Error ? e.message : String(e));
        setStatus("error");
      });
    return () => ctl.abort();
  }, [source, profile]);

  useEffect(load, [load]);

  // A mod selected from outside (a link, the URL) may be far down the list.
  const [scrolled, setScrolled] = useState(false);
  useEffect(() => {
    if (status !== "ready" || scrolled) {
      return;
    }
    setScrolled(true);
    document.querySelector(".dui-mods__item[aria-current='true']")?.scrollIntoView?.({ block: "nearest" });
  }, [status, scrolled]);

  const mods = list?.mods ?? [];
  const tags = useMemo(() => [...new Set(mods.flatMap((m) => m.tags))].sort(), [mods]);
  const knownIds = useMemo(() => new Set(mods.map((m) => m.id)), [mods]);
  const shown = mods.filter((m) => matchesFilter(m, filter) && (!tag || m.tags.includes(tag)) && matchesQuery(m, query));
  const selected = mods.find((m) => m.id === selectedId);
  // Problems that name no mod would otherwise show nowhere.
  const general = list ? list.problems.filter((p) => !mods.some((m) => m.problems.includes(p))) : [];

  function select(id: string) {
    setSelectedId(id);
    onSelect?.(id);
  }

  function patch(id: string, fields: Partial<ModSummary>) {
    setList((l) => l && { ...l, mods: l.mods.map((m) => (m.id === id ? { ...m, ...fields } : m)) });
  }

  // Reload without the loading state, for what a change may have moved
  // elsewhere (problems, mods enabled through a base profile).
  function refresh() {
    source.list(list?.profile).then(setList, () => {});
  }

  const save =
    source.update &&
    ((id: string) => async (changes: ModChanges) => {
      const result = await source.update!(id, changes);
      patch(id, changes);
      return result;
    });

  const toggle =
    source.setInProfile &&
    list &&
    ((mod: ModSummary) => async (enabled: boolean) => {
      const result = await source.setInProfile!(list.profile, mod.id, enabled);
      const profiles = enabled ? [...new Set([...mod.profiles, list.profile])].sort() : mod.profiles.filter((p) => p !== list.profile);
      patch(mod.id, { profiles, enable: enabled });
      refresh();
      return result;
    });

  return (
    <div className="dui-mods">
      <aside className="dui-mods__side">
        <div className="dui-mods__controls">
          <input
            type="search"
            className="dui-mods__search"
            placeholder="Search mods"
            aria-label="Search mods"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
          />
          <div className="dui-mods__selects">
            <select aria-label="Show" value={filter} onChange={(e) => setFilter(e.target.value as ModFilter)}>
              {filters.map((f) => (
                <option key={f.value} value={f.value}>
                  {f.label}
                </option>
              ))}
            </select>
            <select aria-label="Tag" value={tag} onChange={(e) => setTag(e.target.value)}>
              <option value="">Any tag</option>
              {tags.map((t) => (
                <option key={t} value={t}>
                  {t}
                </option>
              ))}
            </select>
            {list && list.profiles.length > 1 && (
              <select aria-label="Profile" value={list.profile} onChange={(e) => setProfile(e.target.value)}>
                {list.profiles.map((p) => (
                  <option key={p} value={p}>
                    {p}
                  </option>
                ))}
              </select>
            )}
          </div>
        </div>

        {status === "loading" && <p className="dui-mods__message">Loading…</p>}
        {status === "error" && (
          <div className="dui-mods__message dui-mods__message--error" role="alert">
            <span>Couldn't load the mods: {error}</span>
            <button type="button" className="dui-mods__retry" onClick={load}>
              Retry
            </button>
          </div>
        )}
        {status === "ready" && list && (
          <>
            <p className="dui-mods__count">
              {shown.length === mods.length ? `${mods.length} mods` : `${shown.length} of ${mods.length} mods`} ·{" "}
              {mods.filter((m) => m.enable).length} enabled in {list.profile}
            </p>
            {general.length > 0 && (
              <ul className="dui-mods__problems" aria-label="Catalog problems">
                {general.map((p) => (
                  <li key={p}>{p}</li>
                ))}
              </ul>
            )}
            {shown.length === 0 ? (
              <p className="dui-mods__message">No mods match.</p>
            ) : (
              <nav aria-label="Mods">
                <ul className="dui-mods__list">
                  {shown.map((m) => (
                    <li key={m.id}>
                      <button
                        type="button"
                        className="dui-mods__item"
                        aria-current={m.id === selectedId ? "true" : undefined}
                        onClick={() => select(m.id)}
                      >
                        <span className="dui-mods__title">
                          <span className={`dui-mods__dot ${m.enable ? "dui-mods__dot--on" : ""}`} aria-hidden="true" />
                          {m.title ?? m.id}
                        </span>
                        <span className="dui-mods__meta">
                          <span>{m.id}</span>
                          {m.selectedVersion && <span>{m.selectedVersion}</span>}
                          {m.downloadPage === null && <span className="dui-mods__flag">no page</span>}
                          {m.problems.length > 0 && <span className="dui-mods__flag dui-mods__flag--bad">problem</span>}
                        </span>
                      </button>
                    </li>
                  ))}
                </ul>
              </nav>
            )}
          </>
        )}
      </aside>
      <main className="dui-mods__detail">
        {selected && list ? (
          <ModDetail
            // a fresh editor (and messages) for each mod
            key={selected.id}
            mod={selected}
            profile={list.profile}
            knownIds={knownIds}
            onSelect={select}
            onSave={save?.(selected.id)}
            onToggle={toggle ? toggle(selected) : undefined}
          />
        ) : (
          status === "ready" && <p className="dui-mods__message">Pick a mod.</p>
        )}
      </main>
    </div>
  );
}

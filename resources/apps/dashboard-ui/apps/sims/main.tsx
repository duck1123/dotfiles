// The Sims mod web UI that `sims web` serves on localhost. The data comes from
// server.mjs next to this file, which runs `sims` commands (simsnix).
import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { ModBrowser } from "../../src/components/ModBrowser";
import type { ModList, ModSource, RenameResult, WriteResult } from "../../src/mods/types";
import "../../src/theme.css";
import "./app.css";

async function getJson<T>(url: string, signal?: AbortSignal): Promise<T> {
  const res = await fetch(url, { signal, headers: { Accept: "application/json" } });
  const body = await res.json().catch(() => null);
  if (!res.ok) {
    throw new Error(body?.error ?? `${res.status} ${res.statusText}`);
  }
  return body as T;
}

async function postJson<T>(url: string, body: unknown): Promise<T> {
  const res = await fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json", Accept: "application/json" },
    body: JSON.stringify(body),
  });
  const result = await res.json().catch(() => null);
  if (!res.ok) {
    throw new Error(result?.error ?? `${res.status} ${res.statusText}`);
  }
  return result as T;
}

// `sims mod set` clears a field given "" (null means "leave it").
const clearable = (v: string | null | undefined) => (v === null ? "" : v);

// Relative URLs, so the page also works behind a proxy under a sub-path.
const source: ModSource = {
  list: (profile, signal) => getJson<ModList>(`api/mods${profile ? `?profile=${encodeURIComponent(profile)}` : ""}`, signal),
  update: (id, changes) =>
    postJson<WriteResult>("api/mod/set", {
      id,
      downloadPage: clearable(changes.downloadPage),
      title: clearable(changes.title),
      tags: changes.tags,
      folder: changes.folder,
      flatten: changes.flatten,
      version: changes.version,
    }),
  rename: (id, to, apply) => postJson<RenameResult>("api/mod/rename", { id, to, apply }),
  setInProfile: (profile, id, enabled) =>
    postJson<WriteResult>(enabled ? "api/profile/add" : "api/profile/remove", { profile, ids: [id] }),
};

// The selected mod is kept in the URL's #, so a reload or a bookmark comes back to it.
const fromHash = () => decodeURIComponent(location.hash.slice(1)) || undefined;

createRoot(document.getElementById("root")!).render(
  <StrictMode>
    <header className="sims-app__header">
      <h1>Sims mods</h1>
    </header>
    <ModBrowser
      source={source}
      selectedId={fromHash()}
      onSelect={(id) => history.replaceState(null, "", `#${encodeURIComponent(id)}`)}
    />
  </StrictMode>,
);

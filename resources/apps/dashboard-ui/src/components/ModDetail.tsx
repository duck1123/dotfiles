import { useState, type FormEvent } from "react";
import { folderProblem, planPlacement, placementProblems, type Placement } from "../mods/placement";
import type { ModChanges, ModFile, ModSummary, RenameResult, WriteResult } from "../mods/types";
import "./ModDetail.css";

export interface ModDetailProps {
  mod: ModSummary;
  /** The profile `mod.enable` refers to. */
  profile: string;
  /** Ids in the catalog, so requires/conflicts can link to them. */
  knownIds?: ReadonlySet<string>;
  /** Show another mod (from requires/conflicts). */
  onSelect?: (id: string) => void;
  /** Save catalog fields; without it the fields can't be edited. */
  onSave?: (changes: ModChanges) => Promise<WriteResult>;
  /** Add the mod to `profile` or take it out; without it there's no button. */
  onToggle?: (enabled: boolean) => Promise<WriteResult>;
  /** Rename the mod (preview first, then `apply`); without it there's no button. */
  onRename?: (to: string, apply: boolean) => Promise<RenameResult>;
  /** A message to show first (e.g. after a rename brought you here). */
  initialNotice?: string;
}

/** Where a mod installs, as the move form picks it. */
export type PlaceChoice = "default" | "custom" | "root";

export interface PlacementForm {
  place: PlaceChoice;
  customFolder: string;
  flatten: boolean;
}

/** The move form's starting values for `mod`. */
export function placementForm(mod: ModSummary): PlacementForm {
  const place: PlaceChoice = mod.folder === null ? "root" : mod.folder === mod.defaultFolder ? "default" : "custom";
  return { place, customFolder: place === "custom" ? mod.folder! : "", flatten: mod.flatten };
}

const tidyFolder = (f: string) => f.trim().replace(/^\/+|\/+$/g, "");

/** The placement the form describes. */
export function formPlacement(mod: ModSummary, form: PlacementForm): Placement {
  const folder = form.place === "default" ? mod.defaultFolder : form.place === "root" ? null : tidyFolder(form.customFolder);
  return { folder, flatten: form.flatten };
}

/** What the move form changes, as `sims mod set` takes it. */
export function diffPlacement(mod: ModSummary, form: PlacementForm): ModChanges {
  const changes: ModChanges = {};
  const { folder } = formPlacement(mod, form);
  if (folder !== mod.folder) {
    changes.folder = form.place === "default" ? "default" : (folder ?? "");
  }
  if (form.flatten !== mod.flatten) {
    changes.flatten = form.flatten;
  }
  return changes;
}

const idPattern = /^[a-z0-9][a-z0-9-]*\/[a-z0-9][a-z0-9-]*$/;

/** Version labels, newest first (numeric parts compare as numbers). */
export function sortVersions(labels: string[]): string[] {
  return [...labels].sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
}

/** Tags typed as a comma-separated list: trimmed, without blanks or repeats, sorted. */
export function parseTags(text: string): string[] {
  return [...new Set(text.split(",").map((t) => t.trim()).filter(Boolean))].sort();
}

const urlPattern = /^https?:\/\/\S+$/;

/** What the form changes, compared with the mod; an empty field clears it. */
export function diffFields(mod: ModSummary, form: { downloadPage: string; title: string; tags: string }): ModChanges {
  const changes: ModChanges = {};
  const page = form.downloadPage.trim() || null;
  const title = form.title.trim() || null;
  const tags = parseTags(form.tags);
  if (page !== mod.downloadPage) {
    changes.downloadPage = page;
  }
  if (title !== mod.title) {
    changes.title = title;
  }
  if (tags.join(",") !== [...mod.tags].sort().join(",")) {
    changes.tags = tags;
  }
  return changes;
}

function errorText(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}

const sentence = (s: string) => s.charAt(0).toUpperCase() + s.slice(1) + (/[.!?]$/.test(s) ? "" : ".");

function savedText(r: WriteResult): string {
  const saved = r.commit ? `Saved (commit ${r.commit})` : "No change";
  return [saved, ...(r.notes ?? []).map(sentence)].join(" ");
}

const shownFolder = (folder: string | null) => (folder === null ? "Mods/" : `Mods/${folder}/`);

function ModLinks({ ids, knownIds, onSelect }: { ids: string[]; knownIds?: ReadonlySet<string>; onSelect?: (id: string) => void }) {
  return (
    <ul className="dui-mod__links">
      {ids.map((id) => (
        <li key={id}>
          {onSelect && (!knownIds || knownIds.has(id)) ? (
            <button type="button" className="dui-mod__link" onClick={() => onSelect(id)}>
              {id}
            </button>
          ) : (
            <span className="dui-mod__missing" title="Not in the catalog">
              {id}
            </span>
          )}
        </li>
      ))}
    </ul>
  );
}

/**
 * A version's files; past `shown` of them, the rest fold away. Hovering a file
 * shows its hash, and a file with the same hash as an earlier one says so.
 */
function FileList({ files, shown = 5 }: { files: ModFile[]; shown?: number }) {
  const firstNamed = new Map<string, string>();
  for (const f of files) if (!firstNamed.has(f.sha256)) firstNamed.set(f.sha256, f.name);
  const item = (f: ModFile) => {
    const first = firstNamed.get(f.sha256);
    return (
      <li key={f.name} title={`sha256 ${f.sha256}`}>
        {f.name}
        {f.members > 0 && <span className="dui-mod__members"> ({f.members} inside)</span>}
        {first !== f.name && <span className="dui-mod__duplicate"> (same file as {first})</span>}
      </li>
    );
  };
  if (files.length <= shown + 1) {
    return <ul className="dui-mod__files">{files.map(item)}</ul>;
  }
  return (
    <>
      <ul className="dui-mod__files">{files.slice(0, shown).map(item)}</ul>
      <details className="dui-mod__more">
        <summary>{files.length - shown} more files</summary>
        <ul className="dui-mod__files">{files.slice(shown).map(item)}</ul>
      </details>
    </>
  );
}

function EditForm({
  mod,
  onSave,
  onDone,
}: {
  mod: ModSummary;
  onSave: (changes: ModChanges) => Promise<WriteResult>;
  onDone: (message: string) => void;
}) {
  const [form, setForm] = useState({
    downloadPage: mod.downloadPage ?? "",
    title: mod.title ?? "",
    tags: mod.tags.join(", "),
  });
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState("");
  const field = (key: keyof typeof form) => ({
    value: form[key],
    onChange: (e: { target: { value: string } }) => setForm({ ...form, [key]: e.target.value }),
  });

  async function submit(e: FormEvent) {
    e.preventDefault();
    const page = form.downloadPage.trim();
    if (page && !urlPattern.test(page)) {
      setError("The download page must be an http(s) URL.");
      return;
    }
    const changes = diffFields(mod, form);
    if (Object.keys(changes).length === 0) {
      onDone("No change");
      return;
    }
    setSaving(true);
    setError("");
    try {
      onDone(savedText(await onSave(changes)));
    } catch (err) {
      setError(errorText(err));
      setSaving(false);
    }
  }

  return (
    <form className="dui-mod__form" onSubmit={submit} aria-label="Edit mod">
      <label>
        <span>Download page</span>
        <input type="url" placeholder="https://…" autoFocus {...field("downloadPage")} />
      </label>
      <label>
        <span>Title</span>
        <input type="text" placeholder={mod.id} {...field("title")} />
      </label>
      <label>
        <span>Tags</span>
        <input type="text" placeholder="comma, separated" {...field("tags")} />
      </label>
      {error && (
        <p className="dui-mod__error" role="alert">
          {error}
        </p>
      )}
      <div className="dui-mod__actions">
        <button type="submit" className="dui-mod__button dui-mod__button--primary" disabled={saving}>
          {saving ? "Saving…" : "Save"}
        </button>
        <button type="button" className="dui-mod__button" disabled={saving} onClick={() => onDone("")}>
          Cancel
        </button>
      </div>
    </form>
  );
}

/** Pick where the mod installs, with a preview of where each file goes. */
function MoveForm({
  mod,
  onSave,
  onDone,
}: {
  mod: ModSummary;
  onSave: (changes: ModChanges) => Promise<WriteResult>;
  onDone: (message: string) => void;
}) {
  const [form, setForm] = useState(() => placementForm(mod));
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState("");
  const placement = formPlacement(mod, form);
  const badFolder = form.place === "custom" ? folderProblem(form.customFolder) : null;
  const planned = badFolder ? [] : planPlacement(mod.installs, placement);
  const problems = badFolder ? [badFolder] : placementProblems(planned);
  const moving = planned.filter((f) => f.from !== f.to);
  const changes = diffPlacement(mod, form);
  const unchanged = Object.keys(changes).length === 0;
  const radio = (place: PlaceChoice) => ({
    type: "radio" as const,
    name: "dui-mod-place",
    checked: form.place === place,
    onChange: () => setForm({ ...form, place }),
  });

  async function submit(e: FormEvent) {
    e.preventDefault();
    if (problems.length > 0) {
      return;
    }
    if (unchanged) {
      onDone("No change");
      return;
    }
    setSaving(true);
    setError("");
    try {
      onDone(savedText(await onSave(changes)));
    } catch (err) {
      setError(errorText(err));
      setSaving(false);
    }
  }

  return (
    <form className="dui-mod__form" onSubmit={submit} aria-label="Move mod">
      <fieldset className="dui-mod__choices">
        <legend>Install to</legend>
        <label className="dui-mod__choice">
          <input {...radio("default")} />
          <span>
            Its own folder <code>Mods/{mod.defaultFolder}/</code>
          </span>
        </label>
        <label className="dui-mod__choice">
          <input {...radio("custom")} />
          <span>Another folder</span>
        </label>
        {form.place === "custom" && (
          <label className="dui-mod__choice-detail">
            <span>Folder under Mods/</span>
            <input
              type="text"
              autoFocus
              placeholder={mod.defaultFolder}
              value={form.customFolder}
              onChange={(e) => setForm({ ...form, customFolder: e.target.value })}
            />
          </label>
        )}
        <label className="dui-mod__choice">
          <input {...radio("root")} />
          <span>
            Directly in <code>Mods/</code>
          </span>
        </label>
      </fieldset>
      <label className="dui-mod__choice">
        <input type="checkbox" checked={form.flatten} onChange={(e) => setForm({ ...form, flatten: e.target.checked })} />
        <span>Flatten: put every file directly in the folder, without the folders it was imported in</span>
      </label>

      <div className="dui-mod__plan" aria-label="Where the files go">
        {mod.installs.length === 0 ? (
          <p className="dui-mod__none">The selected version installs no files.</p>
        ) : (
          <>
            <p className="dui-mod__plan-summary">
              {moving.length === 0 ? "No file moves." : `${moving.length} of ${planned.length} files move:`}
            </p>
            {moving.length > 0 && (
              <ul className="dui-mod__files">
                {moving.map((f) => (
                  <li key={`${f.source}\t${f.sha256}`} title={`sha256 ${f.sha256}`}>
                    <code className="dui-mod__from">{f.from}</code> → <code>{f.to}</code>
                  </li>
                ))}
              </ul>
            )}
          </>
        )}
      </div>
      {moving.length > 0 && mod.stateFiles.length > 0 && (
        <p className="dui-mod__warning">
          It writes state files ({mod.stateFiles.join(", ")}). Those in its old folder aren't moved: move them by hand after
          switching.
        </p>
      )}
      {problems.length > 0 && (
        <ul className="dui-mod__problems" role="alert" aria-label="Problems with this placement">
          {problems.map((p) => (
            <li key={p}>{p}</li>
          ))}
        </ul>
      )}
      {error && (
        <p className="dui-mod__error" role="alert">
          {error}
        </p>
      )}
      <p className="dui-mod__hint">
        Files move at the next <code>sims switch</code>: it takes them out of the old place (and any folder left empty) and
        installs them in the new one.
      </p>
      <div className="dui-mod__actions">
        <button type="submit" className="dui-mod__button dui-mod__button--primary" disabled={saving || problems.length > 0}>
          {saving ? "Saving…" : "Save"}
        </button>
        <button type="button" className="dui-mod__button" disabled={saving} onClick={() => onDone("")}>
          Cancel
        </button>
      </div>
    </form>
  );
}

/** Rename the mod: preview what changes, then apply it. */
function RenameForm({
  mod,
  onRename,
  onDone,
}: {
  mod: ModSummary;
  onRename: (to: string, apply: boolean) => Promise<RenameResult>;
  onDone: (message: string) => void;
}) {
  const [to, setTo] = useState(mod.id);
  const [preview, setPreview] = useState<RenameResult>();
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState("");
  const target = to.trim();

  async function run(apply: boolean) {
    setBusy(true);
    setError("");
    try {
      const r = await onRename(target, apply);
      if (apply) {
        // this form is gone once the mod is shown under its new id
        onDone(`Renamed to ${r.to}${r.commit ? ` (commit ${r.commit})` : ""}.`);
        return;
      }
      setPreview(r);
    } catch (err) {
      setError(errorText(err));
    }
    setBusy(false);
  }

  function submit(e: FormEvent) {
    e.preventDefault();
    if (!idPattern.test(target)) {
      setError("An id is creator/mod, in lowercase letters, digits and -.");
      return;
    }
    if (target === mod.id) {
      setError("That's its id now.");
      return;
    }
    run(preview?.to === target);
  }

  return (
    <form className="dui-mod__form" onSubmit={submit} aria-label="Rename mod">
      <label>
        <span>New id (creator/mod)</span>
        <input
          type="text"
          autoFocus
          spellCheck={false}
          value={to}
          onChange={(e) => {
            setTo(e.target.value);
            setPreview(undefined);
          }}
        />
      </label>
      {preview && (
        <div className="dui-mod__plan" aria-label="What the rename changes">
          <ul className="dui-mod__files">
            {preview.moved.map((m) => (
              <li key={m}>Moves {m}</li>
            ))}
            {preview.folder ? (
              <li>
                Installs to <code>Mods/{preview.folder.to}/</code> instead of <code>Mods/{preview.folder.from}/</code> (files move
                at the next <code>sims switch</code>)
              </li>
            ) : (
              <li>Installs to the same folder</li>
            )}
            <li>
              Rewrites the id in {preview.rewritten.length} file{preview.rewritten.length === 1 ? "" : "s"}
              {preview.rewritten.length > 0 && <>: {preview.rewritten.map((r) => r.file).join(", ")}</>}
            </li>
          </ul>
          {preview.mentions.length > 0 && (
            <details className="dui-mod__more">
              <summary>{preview.mentions.length} other mentions, not changed</summary>
              <ul className="dui-mod__files">
                {preview.mentions.map((m) => (
                  <li key={`${m.file}:${m.line}`}>
                    <code>
                      {m.file}:{m.line}
                    </code>{" "}
                    {m.text}
                  </li>
                ))}
              </ul>
            </details>
          )}
        </div>
      )}
      {error && (
        <p className="dui-mod__error" role="alert">
          {error}
        </p>
      )}
      <div className="dui-mod__actions">
        <button type="submit" className="dui-mod__button dui-mod__button--primary" disabled={busy}>
          {busy ? "Working…" : preview?.to === target ? "Rename" : "Preview"}
        </button>
        <button type="button" className="dui-mod__button" disabled={busy} onClick={() => onDone("")}>
          Cancel
        </button>
      </div>
    </form>
  );
}

/** Where the mod installs and, folded away, each file's path. */
function Placed({ mod }: { mod: ModSummary }) {
  return (
    <>
      <code>{shownFolder(mod.folder)}</code>
      {mod.flatten && <span className="dui-mod__members"> (flattened)</span>}
      {mod.installs.length > 0 && (
        <details className="dui-mod__more">
          <summary>
            {mod.installs.length} file{mod.installs.length === 1 ? "" : "s"}
          </summary>
          <ul className="dui-mod__files">
            {mod.installs.map((f) => (
              <li key={`${f.target}\t${f.sha256}`} title={`from ${f.source}`}>
                {f.target}
              </li>
            ))}
          </ul>
        </details>
      )}
    </>
  );
}

type Mode = "view" | "edit" | "move" | "rename";

/** Everything the catalog knows about one mod, and (given onSave/onToggle/onRename) its edits. */
export function ModDetail({ mod, profile, knownIds, onSelect, onSave, onToggle, onRename, initialNotice = "" }: ModDetailProps) {
  const labels = sortVersions(Object.keys(mod.versions));
  const [mode, setMode] = useState<Mode>("view");
  const editing = mode === "edit";
  const [toggling, setToggling] = useState(false);
  const [notice, setNotice] = useState(initialNotice);
  const [error, setError] = useState("");
  const listed = mod.profiles.includes(profile);

  function open(next: Mode) {
    setNotice("");
    setError("");
    setMode(next);
  }

  const done = (message: string) => {
    setMode("view");
    setNotice(message);
  };

  async function toggle() {
    if (!onToggle) {
      return;
    }
    setToggling(true);
    setError("");
    setNotice("");
    try {
      setNotice(savedText(await onToggle(!listed)));
    } catch (e) {
      setError(errorText(e));
    } finally {
      setToggling(false);
    }
  }

  return (
    <article className="dui-mod" aria-labelledby="dui-mod-title">
      <header className="dui-mod__header">
        <h2 id="dui-mod-title" className="dui-mod__title">
          {mod.title ?? mod.id}
        </h2>
        <code className="dui-mod__id">{mod.id}</code>
        <span className={`dui-mod__state ${mod.enable ? "dui-mod__state--on" : ""}`}>
          {mod.enable ? `Enabled in ${profile}` : `Not in ${profile}`}
        </span>
        {onToggle && (
          <button type="button" className="dui-mod__button" disabled={toggling} onClick={toggle}>
            {toggling ? "Saving…" : listed ? `Remove from ${profile}` : `Add to ${profile}`}
          </button>
        )}
        {onSave && mod.editable && mode === "view" && (
          <>
            <button type="button" className="dui-mod__button" onClick={() => open("edit")}>
              Edit
            </button>
            <button type="button" className="dui-mod__button" onClick={() => open("move")}>
              Move…
            </button>
          </>
        )}
        {onRename && mod.editable && mode === "view" && (
          <button type="button" className="dui-mod__button" onClick={() => open("rename")}>
            Rename…
          </button>
        )}
      </header>

      {notice && (
        <p className="dui-mod__notice" role="status">
          {notice}
        </p>
      )}
      {error && (
        <p className="dui-mod__error" role="alert">
          {error}
        </p>
      )}

      {mod.problems.length > 0 && (
        <ul className="dui-mod__problems" aria-label="Problems">
          {mod.problems.map((p) => (
            <li key={p}>{p}</li>
          ))}
        </ul>
      )}

      {editing && onSave && <EditForm mod={mod} onSave={onSave} onDone={done} />}
      {mode === "move" && onSave && <MoveForm mod={mod} onSave={onSave} onDone={done} />}
      {mode === "rename" && onRename && <RenameForm mod={mod} onRename={onRename} onDone={done} />}

      <dl className="dui-mod__fields">
        {!editing && (
          <>
            <dt>Download page</dt>
            <dd>
              {mod.downloadPage ? (
                <a href={mod.downloadPage} target="_blank" rel="noreferrer noopener">
                  {mod.downloadPage}
                </a>
              ) : (
                <span className="dui-mod__none">None yet</span>
              )}
            </dd>
            <dt>Tags</dt>
            <dd>
              {mod.tags.length > 0 ? (
                <ul className="dui-mod__tags">
                  {mod.tags.map((t) => (
                    <li key={t} className="dui-mod__tag">
                      {t}
                    </li>
                  ))}
                </ul>
              ) : (
                <span className="dui-mod__none">None</span>
              )}
            </dd>
          </>
        )}
        <dt>Installs to</dt>
        <dd>
          <Placed mod={mod} />
        </dd>
        <dt>Profiles</dt>
        <dd>{mod.profiles.length > 0 ? mod.profiles.join(", ") : <span className="dui-mod__none">None list it</span>}</dd>
        {mod.requires.length > 0 && (
          <>
            <dt>Requires</dt>
            <dd>
              <ModLinks ids={mod.requires} knownIds={knownIds} onSelect={onSelect} />
            </dd>
          </>
        )}
        {mod.conflicts.length > 0 && (
          <>
            <dt>Conflicts with</dt>
            <dd>
              <ModLinks ids={mod.conflicts} knownIds={knownIds} onSelect={onSelect} />
            </dd>
          </>
        )}
        {!mod.editable && (
          <>
            <dt>Defined in</dt>
            <dd>Nix (edit its .nix file by hand)</dd>
          </>
        )}
      </dl>

      <h3 className="dui-mod__subheading">Versions</h3>
      {labels.length === 0 ? (
        <p className="dui-mod__none">No versions recorded.</p>
      ) : (
        <table className="dui-mod__versions">
          <thead>
            <tr>
              <th scope="col">Version</th>
              <th scope="col">Added</th>
              <th scope="col">Game</th>
              <th scope="col">Files</th>
            </tr>
          </thead>
          <tbody>
            {labels.map((label) => {
              const v = mod.versions[label];
              const selected = label === mod.selectedVersion;
              return (
                <tr key={label} aria-current={selected ? "true" : undefined}>
                  <th scope="row">
                    {label}
                    {selected && (
                      <span className="dui-mod__badge">{mod.version === null ? "installs (newest)" : "installs (pinned)"}</span>
                    )}
                  </th>
                  <td>{v.added ?? "—"}</td>
                  <td>{v.gameVersion ?? "—"}</td>
                  <td>
                    <FileList files={v.files} />
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      )}
    </article>
  );
}

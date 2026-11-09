import { useState, type FormEvent } from "react";
import type { ModChanges, ModFile, ModSummary, WriteResult } from "../mods/types";
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
}

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

function savedText(r: WriteResult): string {
  return r.commit ? `Saved (commit ${r.commit})` : "No change";
}

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

/** Everything the catalog knows about one mod, and (given onSave/onToggle) its edits. */
export function ModDetail({ mod, profile, knownIds, onSelect, onSave, onToggle }: ModDetailProps) {
  const labels = sortVersions(Object.keys(mod.versions));
  const [editing, setEditing] = useState(false);
  const [toggling, setToggling] = useState(false);
  const [notice, setNotice] = useState("");
  const [error, setError] = useState("");
  const listed = mod.profiles.includes(profile);

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
        {onSave && mod.editable && !editing && (
          <button
            type="button"
            className="dui-mod__button"
            onClick={() => {
              setNotice("");
              setError("");
              setEditing(true);
            }}
          >
            Edit
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

      {editing && onSave && (
        <EditForm
          mod={mod}
          onSave={onSave}
          onDone={(message) => {
            setEditing(false);
            setNotice(message);
          }}
        />
      )}

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
          <code>{mod.folder === null ? "Mods/" : `Mods/${mod.folder}/`}</code>
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

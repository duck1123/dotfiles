import type { ModFile, ModSummary } from "../mods/types";
import "./ModDetail.css";

export interface ModDetailProps {
  mod: ModSummary;
  /** The profile `mod.enable` refers to. */
  profile: string;
  /** Ids in the catalog, so requires/conflicts can link to them. */
  knownIds?: ReadonlySet<string>;
  /** Show another mod (from requires/conflicts). */
  onSelect?: (id: string) => void;
}

/** Version labels, newest first (numeric parts compare as numbers). */
export function sortVersions(labels: string[]): string[] {
  return [...labels].sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
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

/** A version's files; past `shown` of them, the rest fold away. */
function FileList({ files, shown = 5 }: { files: ModFile[]; shown?: number }) {
  const item = (f: ModFile) => (
    <li key={f.name}>
      {f.name}
      {f.members > 0 && <span className="dui-mod__members"> ({f.members} inside)</span>}
    </li>
  );
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

/** Everything the catalog knows about one mod. */
export function ModDetail({ mod, profile, knownIds, onSelect }: ModDetailProps) {
  const labels = sortVersions(Object.keys(mod.versions));
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
      </header>

      {mod.problems.length > 0 && (
        <ul className="dui-mod__problems" aria-label="Problems">
          {mod.problems.map((p) => (
            <li key={p}>{p}</li>
          ))}
        </ul>
      )}

      <dl className="dui-mod__fields">
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

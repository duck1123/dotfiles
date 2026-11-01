import { useCallback, useEffect, useState } from "react";
import type { MediaDecision, MediaFile, MediaRepair, MediaSource, MediaSpot, MediaView } from "../media/types";
import "./MediaReview.css";

export interface MediaReviewProps {
  source: MediaSource;
}

const VIEWS: { view: MediaView; label: string }[] = [
  { view: "open", label: "To review" },
  { view: "replace", label: "Replacement requested" },
  { view: "replaced", label: "Replaced" },
  { view: "good", label: "Marked good" },
];

const RULES: Record<string, string> = {
  "mkv-ebml": "hole in the file",
  "mkv-element": "damaged container",
  "invalid-data": "damaged container",
  "mp4-truncated": "truncated container",
  "decode-mb": "video decode errors",
  "missing-ref": "missing reference frames",
  nal: "damaged video data",
  "mpeg-video": "damaged video data",
  "decoder-error": "decoder rejected data",
  aac: "damaged audio",
  "ac3-dts": "damaged audio",
  "mpeg-audio": "damaged audio",
};

// ffmpeg reads ahead of what it reports, so a logged position can sit before
// the damage: a hole logged at 19:19 was really at 19:41. Show a range.
const SPOT_LAG_SECONDS = 30;

function clock(seconds: number): string {
  const s = Math.floor(seconds);
  const mm = String(Math.floor((s % 3600) / 60));
  const ss = String(s % 60).padStart(2, "0");
  return s >= 3600 ? `${Math.floor(s / 3600)}:${mm.padStart(2, "0")}:${ss}` : `${mm}:${ss}`;
}

/** Where to look for a spot in a player. */
export function spotWindow(spot: MediaSpot, duration: number | null): string {
  if (spot.t === null) {
    return "Position unknown";
  }
  const end = spot.t + SPOT_LAG_SECONDS;
  return `About ${clock(spot.t)}–${clock(duration === null ? end : Math.min(end, duration))}`;
}

function parts(file: MediaFile) {
  const segments = file.path.split("/");
  return { group: segments[0], title: segments[segments.length - 1].replace(/\.[^.]+$/, "") };
}

/** The file name, without the show or movie that the group already says. */
function shortTitle(file: MediaFile): string {
  const { group, title } = parts(file);
  const bare = group.replace(/ \(\d{4}\)$/, "");
  return title.startsWith(`${bare} - `) ? title.slice(bare.length + 3) : title;
}

function summary(file: MediaFile): string {
  if (file.truncated) {
    return `Stops at ${clock(file.truncated.decoded)} of ${clock(file.truncated.duration)}`;
  }
  const spots = file.spots.length;
  if (spots > 0) {
    return spots === 1 ? "1 damaged spot" : `${spots} damaged spots`;
  }
  return file.fatal ? "Decode crashed" : "Unknown errors";
}

const REPAIR_STATES: Record<MediaRepair["state"], string> = {
  searching: "Moved out of Plex, waiting for a download",
  verifying: "Downloaded, waiting for its full decode",
  replaced: "Replaced: the new file passed a full decode",
  removed: "Original removed",
  no_release: "Left in place: nothing to download",
  attention: "Stuck: needs you",
  untracked: "Left in place: not tracked",
};

function repairStatus(file: MediaFile): string {
  if (file.repair) {
    return REPAIR_STATES[file.repair.state];
  }
  return file.status === "replace_requested" ? "Waiting its turn" : "";
}

function copies(file: MediaFile): string {
  const count = file.repair?.quarantined.length ?? 0;
  return count === 1 ? "1 quarantined file" : `${count} quarantined files`;
}

/** A repair that has moved the file has to run its course. */
function canReopen(file: MediaFile): boolean {
  return !file.repair || file.repair.state === "no_release" || file.repair.state === "untracked";
}

/** What the remove button deletes here, if it has one. Without a tracked copy, an untracked file may be the only one. */
function removal(view: MediaView, file: MediaFile): { decision: MediaDecision; label: string; what: string } | undefined {
  if (view === "replaced") {
    return { decision: "remove_original", label: "Remove original", what: copies(file) };
  }
  if (view === "replace" && file.repair?.state === "untracked" && file.repair.trackedCopy) {
    return { decision: "remove_leftover", label: "Remove leftover", what: "this file" };
  }
  return undefined;
}

function hint(view: MediaView, file: MediaFile): string {
  if (file.repair?.removing) {
    return "Removing it on the next repair run, within 15 minutes.";
  }
  if (view === "open") {
    return "A replacement request moves the file out of Plex when its turn comes. It's kept in quarantine until you remove it.";
  }
  if (view === "replaced") {
    return "The original stays in quarantine until you remove it.";
  }
  if (view === "replace") {
    return canReopen(file) ? "The file is still in place, so this can be undone." : "The original is in quarantine, out of Plex.";
  }
  return "This only records your decision. The file isn't moved or deleted.";
}

function groupFiles(files: MediaFile[]): { group: string; files: MediaFile[] }[] {
  const groups = new Map<string, MediaFile[]>();
  for (const file of files) {
    const { group } = parts(file);
    groups.set(group, [...(groups.get(group) ?? []), file]);
  }
  return [...groups].map(([group, grouped]) => ({ group, files: grouped }));
}

/** Review the files the media integrity checker flagged. Decisions are recorded here; f/mediacheck/repair acts on them. */
export function MediaReview({ source }: MediaReviewProps) {
  const [view, setView] = useState<MediaView>("open");
  const [files, setFiles] = useState<MediaFile[]>([]);
  const [status, setStatus] = useState<"loading" | "ready" | "error">("loading");
  const [error, setError] = useState("");
  const [selectedId, setSelectedId] = useState<number>();
  const [detail, setDetail] = useState<MediaFile>();
  const [saving, setSaving] = useState(false);
  const [saveError, setSaveError] = useState("");
  const [confirmingRemove, setConfirmingRemove] = useState(false);

  const load = useCallback(() => {
    let cancelled = false;
    setStatus("loading");
    source
      .list(view)
      .then((listed) => {
        if (cancelled) {
          return;
        }
        setFiles(listed);
        setSelectedId(listed[0]?.id);
        setStatus("ready");
      })
      .catch((e: unknown) => {
        if (!cancelled) {
          setError(e instanceof Error ? e.message : String(e));
          setStatus("error");
        }
      });
    return () => {
      cancelled = true;
    };
  }, [source, view]);

  useEffect(load, [load]);

  useEffect(() => {
    setSaveError("");
    setConfirmingRemove(false);
    if (selectedId === undefined) {
      return;
    }
    let cancelled = false;
    source
      .get(selectedId)
      .then((file) => {
        if (!cancelled) {
          setDetail(file);
        }
      })
      // The list's copy stays on screen; only the log is missing.
      .catch(() => undefined);
    return () => {
      cancelled = true;
    };
  }, [source, selectedId]);

  const selected = files.find((f) => f.id === selectedId);
  const current = detail?.id === selectedId ? detail : selected;
  const remove = current && !current.repair?.removing ? removal(view, current) : undefined;

  async function decide(decision: MediaDecision) {
    if (!current) {
      return;
    }
    setSaving(true);
    setSaveError("");
    try {
      const updated = await source.decide(current.id, decision);
      if (decision === "remove_original" || decision === "remove_leftover") {
        // It stays in this view, marked, until the repair run has removed it.
        setDetail(updated);
        setFiles(files.map((f) => (f.id === updated.id ? updated : f)));
        setConfirmingRemove(false);
        return;
      }
      // Whatever the decision, the file leaves this view; the next one takes its place.
      const index = files.findIndex((f) => f.id === current.id);
      const remaining = files.filter((f) => f.id !== current.id);
      setFiles(remaining);
      setSelectedId(remaining[Math.min(index, remaining.length - 1)]?.id);
    } catch (e) {
      setSaveError(e instanceof Error ? e.message : String(e));
    } finally {
      setSaving(false);
    }
  }

  return (
    <div className="dui-media">
      <aside className="dui-media__queue">
        <div className="dui-media__views" role="group" aria-label="Show">
          {VIEWS.map((v) => (
            <button
              key={v.view}
              type="button"
              className="dui-media__view"
              aria-pressed={v.view === view}
              onClick={() => setView(v.view)}
            >
              {v.label}
            </button>
          ))}
        </div>
        {status === "loading" && <p className="dui-media__message">Loading…</p>}
        {status === "error" && (
          <div className="dui-media__message dui-media__message--error" role="alert">
            <span>Couldn't load the files: {error}</span>
            <button type="button" className="dui-media__retry" onClick={load}>
              Retry
            </button>
          </div>
        )}
        {status === "ready" && files.length === 0 && <p className="dui-media__message">Nothing here.</p>}
        {status === "ready" && files.length > 0 && (
          <nav aria-label="Flagged files">
            <p className="dui-media__count">{files.length === 1 ? "1 file" : `${files.length} files`}</p>
            {groupFiles(files).map(({ group, files: grouped }) => (
              <section key={group} className="dui-media__group">
                <h3 className="dui-media__group-heading">
                  {group}
                  <span className="dui-media__group-count">{grouped.length}</span>
                </h3>
                <ul className="dui-media__list">
                  {grouped.map((file) => (
                    <li key={file.id}>
                      <button
                        type="button"
                        className="dui-media__item"
                        aria-current={file.id === selectedId ? "true" : undefined}
                        onClick={() => setSelectedId(file.id)}
                      >
                        <span className="dui-media__item-title">{shortTitle(file)}</span>
                        <span className="dui-media__item-meta">
                          {file.verdict && <span className="dui-media__verdict">{file.verdict}</span>}
                          <span>{view === "open" ? summary(file) : repairStatus(file) || summary(file)}</span>
                        </span>
                      </button>
                    </li>
                  ))}
                </ul>
              </section>
            ))}
          </nav>
        )}
      </aside>
      <main className="dui-media__detail">
        {current ? (
          <article key={current.id}>
            <h2 className="dui-media__title">{parts(current).title}</h2>
            <p className="dui-media__path">
              {current.library}/{current.path}
            </p>
            {current.repair && (
              <div className="dui-media__repair">
                <strong>{REPAIR_STATES[current.repair.state]}</strong>
                {current.repair.note && <p>{current.repair.note}</p>}
                {current.repair.attempts > 0 && <p>Replacements that failed their decode: {current.repair.attempts}</p>}
                {current.repair.replacement && (
                  <p>
                    New file: {current.repair.replacement}
                    {current.repair.replacementVerdict === "minor" && " (a glitch or two; it's also in To review)"}
                  </p>
                )}
              </div>
            )}
            <ul className="dui-media__findings">
              {current.truncated && (
                <li>
                  <strong>
                    Stops at {clock(current.truncated.decoded)} of {clock(current.truncated.duration)}
                  </strong>
                </li>
              )}
              {current.fatal && (
                <li>
                  <strong>The decode crashed (exit {current.fatal.exitCode})</strong>
                </li>
              )}
              {current.spots.map((spot, index) => (
                <li key={index}>
                  <strong>{spotWindow(spot, current.duration)}</strong>
                  {": "}
                  {[...new Set(spot.rules.map((r) => RULES[r] ?? r))].join(", ")}
                </li>
              ))}
              {current.unknownSamples.map((sample) => (
                <li key={sample}>Unrecognized error: {sample}</li>
              ))}
            </ul>
            <div className="dui-media__actions">
              {view === "open" && (
                <>
                  <button type="button" className="dui-media__button dui-media__button--primary" disabled={saving} onClick={() => decide("replace")}>
                    Request replacement
                  </button>
                  <button type="button" className="dui-media__button" disabled={saving} onClick={() => decide("good")}>
                    Mark good
                  </button>
                </>
              )}
              {remove && !confirmingRemove && (
                <button type="button" className="dui-media__button" disabled={saving} onClick={() => setConfirmingRemove(true)}>
                  {remove.label}
                </button>
              )}
              {remove && confirmingRemove && (
                <>
                  <button type="button" className="dui-media__button dui-media__button--danger" disabled={saving} onClick={() => decide(remove.decision)}>
                    Permanently delete {remove.what}
                  </button>
                  <button type="button" className="dui-media__button" disabled={saving} onClick={() => setConfirmingRemove(false)}>
                    Keep
                  </button>
                </>
              )}
              {(view === "good" || (view === "replace" && canReopen(current) && !current.repair?.removing)) && (
                <button type="button" className="dui-media__button" disabled={saving} onClick={() => decide("reopen")}>
                  Back to review
                </button>
              )}
            </div>
            <p className="dui-media__hint">{hint(view, current)}</p>
            {saveError && (
              <p className="dui-media__message dui-media__message--error" role="alert">
                Couldn't save: {saveError}
              </p>
            )}
            {current.log && current.log.length > 0 && (
              <details className="dui-media__log">
                <summary>Error log ({current.lineCount ?? current.log.length} lines)</summary>
                <pre>{current.log.map((line) => line.msg).join("\n")}</pre>
              </details>
            )}
          </article>
        ) : (
          status === "ready" && <p className="dui-media__message">Pick a file to review.</p>
        )}
      </main>
    </div>
  );
}

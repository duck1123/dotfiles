import { useEffect, useId, useState, type FormEvent, type KeyboardEvent } from "react";
import { defaultKinds, type InboxItem, type ReviewChanges } from "../inbox/types";
import type { NoteScope, NoteSource, NoteSummary } from "../notes/types";
import { NoteChip } from "./NoteChip";
import { NotePicker } from "./NotePicker";
import "./ReviewItem.css";

export interface ReviewItemProps {
  item: InboxItem;
  /** Where topics are searched and looked up. */
  source: NoteSource;
  topicScope: NoteScope;
  kinds?: string[];
  /** Write the changes back. Rejecting shows the error and keeps the edits. */
  onSave: (changes: ReviewChanges) => Promise<void>;
}

// Render with `key={item.noteId}` so switching items starts a fresh form.

/** The editor for one queue item: title, kind and topics, then save or mark reviewed. */
export function ReviewItem({ item, source, topicScope, kinds = defaultKinds, onSave }: ReviewItemProps) {
  const id = useId();
  const [title, setTitle] = useState(item.title);
  const [kind, setKind] = useState(item.kind ?? "");
  const [topicIds, setTopicIds] = useState(item.topicIds);
  const [saving, setSaving] = useState<"save" | "done" | null>(null);
  const [error, setError] = useState("");
  const [tags, setTags] = useState<NoteSummary[]>([]);

  // Older ~tag relations are shown so they can be copied over as topics.
  useEffect(() => {
    let cancelled = false;
    if (item.tagIds.length > 0) {
      source.getNotes(item.tagIds).then(
        (notes) => !cancelled && setTags(notes),
        () => {},
      );
    }
    return () => {
      cancelled = true;
    };
  }, [source, item.tagIds]);

  const kindOptions = kind && !kinds.includes(kind) ? [...kinds, kind] : kinds;
  const dirty =
    title.trim() !== item.title ||
    kind !== (item.kind ?? "") ||
    topicIds.length !== item.topicIds.length ||
    topicIds.some((t) => !item.topicIds.includes(t));

  async function save(done: boolean) {
    if (saving || title.trim() === "") {
      return;
    }
    setSaving(done ? "done" : "save");
    setError("");
    try {
      await onSave({ title: title.trim(), kind, topicIds, done });
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setSaving(null);
    }
  }

  // Plain Enter (e.g. in the title) must not mark the item reviewed.
  function onSubmit(event: FormEvent) {
    event.preventDefault();
  }

  // Ctrl/Cmd+Enter anywhere in the form marks the item reviewed.
  function onKeyDown(event: KeyboardEvent<HTMLFormElement>) {
    if (event.key === "Enter" && (event.ctrlKey || event.metaKey)) {
      event.preventDefault();
      void save(true);
    }
  }

  return (
    <form className="dui-review" onSubmit={onSubmit} onKeyDown={onKeyDown} aria-label="Review item">
      <div className="dui-review__row">
        <label className="dui-review__label" htmlFor={`${id}-title`}>
          Title
        </label>
        <input
          id={`${id}-title`}
          className="dui-review__input"
          value={title}
          onChange={(event) => setTitle(event.target.value)}
          required
        />
      </div>

      <div className="dui-review__row dui-review__row--inline">
        <label className="dui-review__label" htmlFor={`${id}-kind`}>
          Kind
        </label>
        <select id={`${id}-kind`} className="dui-review__select" value={kind} onChange={(e) => setKind(e.target.value)}>
          {kind === "" && <option value="">(none)</option>}
          {kindOptions.map((k) => (
            <option key={k} value={k}>
              {k}
            </option>
          ))}
        </select>
        <span className="dui-review__links">
          {item.created && <span className="dui-review__muted">{item.created.replace("T", " ")}</span>}
          {item.url && (
            <a href={item.url} target="_blank" rel="noreferrer">
              Open link
            </a>
          )}
          <a href={source.noteUrl(item.noteId)} target="_blank" rel="noreferrer">
            Open in Trilium
          </a>
        </span>
      </div>

      <NotePicker label="Topics" source={source} scope={topicScope} value={topicIds} onChange={setTopicIds} />

      {tags.length > 0 && (
        <div className="dui-review__row">
          <span className="dui-review__label">Older tags (~tag, not changed here)</span>
          <span className="dui-review__chips">
            {tags.map((tag) => (
              <NoteChip key={tag.noteId} note={tag} href={source.noteUrl(tag.noteId)} />
            ))}
            {tags.some((t) => !topicIds.includes(t.noteId)) && (
              <button
                type="button"
                className="dui-review__link-button"
                onClick={() => setTopicIds([...topicIds, ...tags.map((t) => t.noteId).filter((t) => !topicIds.includes(t))])}
              >
                Copy to topics
              </button>
            )}
          </span>
        </div>
      )}

      {item.preview && <p className="dui-review__preview">{item.preview}</p>}

      {error && (
        <p className="dui-review__error" role="alert">
          Couldn't save: {error}
        </p>
      )}

      <div className="dui-review__actions">
        <button type="button" className="dui-review__button" disabled={!dirty || saving !== null} onClick={() => save(false)}>
          {saving === "save" ? "Saving…" : "Save"}
        </button>
        <button
          type="button"
          className="dui-review__button dui-review__button--primary"
          disabled={saving !== null}
          onClick={() => save(true)}
        >
          {saving === "done" ? "Saving…" : "Mark reviewed"}
        </button>
        <span className="dui-review__hint">Ctrl+Enter</span>
      </div>
    </form>
  );
}

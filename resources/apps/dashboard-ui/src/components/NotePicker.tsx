import { useEffect, useId, useMemo, useRef, useState, type KeyboardEvent } from "react";
import type { NoteScope, NoteSource, NoteSummary } from "../notes/types";
import { NoteChip } from "./NoteChip";
import "./NotePicker.css";

// Written against the plain React API (hooks, JSX, no memo/forwardRef/portals)
// so the same source also builds for Trilium widgets, where `react` maps onto
// Trilium's own Preact (`trilium:preact`).

export interface NotePickerProps {
  source: NoteSource;
  scope: NoteScope;
  /** Picked note IDs. */
  value: string[];
  onChange: (noteIds: string[]) => void;
  /** Allow several notes (default) or exactly one. */
  multi?: boolean;
  label?: string;
  placeholder?: string;
  /** Wait this long after typing stops before searching. */
  debounceMs?: number;
  limit?: number;
  /** Open with the result list showing, e.g. for a story. */
  defaultOpen?: boolean;
  /** Start with this text in the field. */
  defaultText?: string;
}

type Status = "idle" | "loading" | "done" | "error";

export function NotePicker({
  source,
  scope,
  value,
  onChange,
  multi = true,
  label,
  placeholder,
  debounceMs = 150,
  limit = 20,
  defaultOpen = false,
  defaultText = "",
}: NotePickerProps) {
  const id = useId();
  const inputId = `${id}-input`;
  const listId = `${id}-list`;
  const optionId = (index: number) => `${id}-option-${index}`;
  const noun = scope.noun ?? "note";

  const [text, setText] = useState(defaultText);
  const [open, setOpen] = useState(defaultOpen);
  const [results, setResults] = useState<NoteSummary[]>([]);
  const [status, setStatus] = useState<Status>("idle");
  const [error, setError] = useState("");
  const [active, setActive] = useState(0);
  const [attempt, setAttempt] = useState(0);
  const [known, setKnown] = useState<Record<string, NoteSummary>>({});

  // Look up picked notes we haven't seen yet, so their chips have titles. Each
  // ID is asked for once, so one that doesn't exist isn't asked for forever.
  const requested = useRef(new Set<string>());
  useEffect(() => {
    const missing = value.filter((noteId) => !known[noteId] && !requested.current.has(noteId));
    if (missing.length === 0) {
      return;
    }
    missing.forEach((noteId) => requested.current.add(noteId));
    let cancelled = false;
    source.getNotes(missing).then(
      (notes) => {
        if (!cancelled) {
          setKnown((prev) => ({ ...prev, ...Object.fromEntries(notes.map((n) => [n.noteId, n])) }));
        }
      },
      () => {
        // Chips fall back to showing the note ID.
      },
    );
    return () => {
      cancelled = true;
    };
  }, [source, value, known]);

  // Search as the text changes, dropping any search still in flight. Depends on
  // the scope's fields rather than the object, so an inline `scope={{...}}`
  // doesn't search again on every render.
  const { query: scopeQuery } = scope;
  useEffect(() => {
    const needle = text.trim();
    if (!open || needle === "") {
      setResults([]);
      setStatus("idle");
      return;
    }
    const controller = new AbortController();
    setStatus("loading");
    const timer = setTimeout(() => {
      source.search(needle, { query: scopeQuery, noun }, { limit, signal: controller.signal }).then(
        (found) => {
          // A source that ignores the signal (a Windmill job can't be
          // cancelled) still resolves; drop results for text that's changed.
          if (controller.signal.aborted) {
            return;
          }
          setResults(found);
          setActive(0);
          setStatus("done");
        },
        (err: unknown) => {
          if (controller.signal.aborted) {
            return;
          }
          setError(err instanceof Error ? err.message : String(err));
          setStatus("error");
        },
      );
    }, debounceMs);
    return () => {
      clearTimeout(timer);
      controller.abort();
    };
  }, [source, scopeQuery, noun, text, open, limit, debounceMs, attempt]);

  const visible = useMemo(() => results.filter((note) => !value.includes(note.noteId)), [results, value]);

  function pick(note: NoteSummary) {
    setKnown((prev) => ({ ...prev, [note.noteId]: note }));
    onChange(multi ? [...value, note.noteId] : [note.noteId]);
    setText("");
    if (!multi) {
      setOpen(false);
    }
  }

  function remove(noteId: string) {
    onChange(value.filter((v) => v !== noteId));
  }

  function onKeyDown(event: KeyboardEvent<HTMLInputElement>) {
    switch (event.key) {
      case "ArrowDown":
        event.preventDefault();
        setOpen(true);
        setActive((i) => Math.min(i + 1, Math.max(visible.length - 1, 0)));
        break;
      case "ArrowUp":
        event.preventDefault();
        setActive((i) => Math.max(i - 1, 0));
        break;
      case "Enter":
        // Ctrl/Cmd+Enter belongs to the surrounding form.
        if (!event.ctrlKey && !event.metaKey && open && visible[active]) {
          event.preventDefault();
          pick(visible[active]);
        }
        break;
      case "Escape":
        setOpen(false);
        break;
      case "Backspace":
        if (text === "" && value.length > 0) {
          remove(value[value.length - 1]);
        }
        break;
    }
  }

  const showPopup = open && (text.trim() !== "" || status !== "idle");
  const activeOption = showPopup && visible[active] ? optionId(active) : undefined;

  return (
    <div className="dui-picker">
      {label && (
        <label className="dui-picker__label" htmlFor={inputId}>
          {label}
        </label>
      )}
      <div className="dui-picker__field">
        {value.map((noteId) => {
          const note = known[noteId] ?? { noteId, title: noteId, path: [] };
          return (
            <NoteChip key={noteId} note={note} href={source.noteUrl(noteId)} onRemove={() => remove(noteId)} />
          );
        })}
        <input
          id={inputId}
          className="dui-picker__input"
          type="text"
          role="combobox"
          autoComplete="off"
          aria-autocomplete="list"
          aria-expanded={showPopup}
          aria-controls={listId}
          aria-activedescendant={activeOption}
          placeholder={value.length === 0 ? (placeholder ?? `Search ${noun}s…`) : undefined}
          value={text}
          onChange={(event) => {
            setText(event.target.value);
            setOpen(true);
          }}
          onFocus={() => setOpen(true)}
          onBlur={() => setOpen(false)}
          onKeyDown={onKeyDown}
        />
      </div>
      {showPopup && (
        <div className="dui-picker__popup">
          <ul id={listId} className="dui-picker__list" role="listbox" aria-label={`${noun} suggestions`}>
            {visible.map((note, index) => (
              <li
                key={note.noteId}
                id={optionId(index)}
                className="dui-picker__option"
                role="option"
                aria-selected={index === active}
                onMouseDown={(event) => event.preventDefault()}
                onMouseEnter={() => setActive(index)}
                onClick={() => pick(note)}
              >
                <span className={`dui-picker__icon ${note.icon ?? "dui-picker__icon--default"}`} aria-hidden="true" />
                <span className="dui-picker__text">
                  <span className="dui-picker__title">{note.title}</span>
                  {note.path.length > 0 && <span className="dui-picker__path">{note.path.join(" › ")}</span>}
                </span>
              </li>
            ))}
          </ul>
          {status === "loading" && visible.length === 0 && (
            <p className="dui-picker__message" role="status">
              Searching…
            </p>
          )}
          {status === "done" && visible.length === 0 && (
            <p className="dui-picker__message" role="status">
              No {noun}s match “{text.trim()}”
            </p>
          )}
          {status === "error" && (
            <div className="dui-picker__message dui-picker__message--error" role="alert">
              <span>Search failed: {error}</span>
              <button
                type="button"
                className="dui-picker__retry"
                onMouseDown={(event) => event.preventDefault()}
                onClick={() => setAttempt((n) => n + 1)}
              >
                Retry
              </button>
            </div>
          )}
        </div>
      )}
    </div>
  );
}

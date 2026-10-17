import type { NoteSummary } from "../notes/types";
import "./NoteChip.css";

export interface NoteChipProps {
  note: NoteSummary;
  /** Where the title links to; omit for a plain label. */
  href?: string;
  /** Shows a remove button that calls this. */
  onRemove?: () => void;
}

/** One picked note: icon, title (linking to the note) and an optional remove button. */
export function NoteChip({ note, href, onRemove }: NoteChipProps) {
  const path = note.path.join(" › ");
  return (
    <span className="dui-chip" title={path ? `${path} › ${note.title}` : note.title}>
      <span className={`dui-chip__icon ${note.icon ?? "dui-chip__icon--default"}`} aria-hidden="true" />
      {href ? (
        <a className="dui-chip__title" href={href} target="_blank" rel="noreferrer">
          {note.title}
        </a>
      ) : (
        <span className="dui-chip__title">{note.title}</span>
      )}
      {onRemove && (
        <button type="button" className="dui-chip__remove" aria-label={`Remove ${note.title}`} onClick={onRemove}>
          ×
        </button>
      )}
    </span>
  );
}

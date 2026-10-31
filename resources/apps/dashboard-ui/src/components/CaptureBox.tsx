import { useState, type FormEvent, type KeyboardEvent } from "react";
import type { InboxItem } from "../inbox/types";
import "./CaptureBox.css";

export interface CaptureBoxProps {
  /** Send the text to the inbox; resolves with the created item. */
  onCapture: (text: string) => Promise<InboxItem>;
  autoFocus?: boolean;
  onFocus?: () => void;
  onBlur?: () => void;
}

/** A text box that adds a thought or link to the inbox, like the phone share. */
export function CaptureBox({ onCapture, autoFocus = false, onFocus, onBlur }: CaptureBoxProps) {
  const [text, setText] = useState("");
  const [sending, setSending] = useState(false);
  const [error, setError] = useState("");
  const [last, setLast] = useState<InboxItem>();

  async function submit(event?: FormEvent) {
    event?.preventDefault();
    if (!text.trim() || sending) {
      return;
    }
    setSending(true);
    setError("");
    try {
      setLast(await onCapture(text));
      setText("");
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setSending(false);
    }
  }

  // Enter sends; Shift+Enter starts a new line.
  function onKeyDown(event: KeyboardEvent<HTMLTextAreaElement>) {
    if (event.key === "Enter" && !event.shiftKey && !event.nativeEvent.isComposing) {
      void submit(event);
    }
  }

  return (
    <form className="dui-capture" onSubmit={submit} aria-label="Capture">
      <textarea
        className="dui-capture__input"
        aria-label="Capture"
        placeholder="A thought or link (task:, watch:, shopping:)"
        rows={2}
        value={text}
        autoFocus={autoFocus}
        onChange={(e) => setText(e.target.value)}
        onKeyDown={onKeyDown}
        onFocus={onFocus}
        onBlur={onBlur}
      />
      <div className="dui-capture__row">
        <button type="submit" className="dui-capture__button" disabled={!text.trim() || sending}>
          {sending ? "Capturing…" : "Capture"}
        </button>
        {error ? (
          <span className="dui-capture__error" role="alert">
            Couldn't capture: {error}
          </span>
        ) : (
          last && (
            <span className="dui-capture__done" role="status">
              Captured {last.kind && <span className="dui-capture__kind">{last.kind}</span>} {last.title}
            </span>
          )
        )}
      </div>
    </form>
  );
}

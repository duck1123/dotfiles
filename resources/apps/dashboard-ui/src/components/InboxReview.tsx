import { useCallback, useEffect, useState } from "react";
import type { InboxItem, InboxSource, ReviewChanges } from "../inbox/types";
import type { NoteScope, NoteSource } from "../notes/types";
import { ReviewItem } from "./ReviewItem";
import { ReviewQueue } from "./ReviewQueue";
import "./InboxReview.css";

export interface InboxReviewProps {
  inbox: InboxSource;
  /** Where topics are searched and looked up. */
  notes: NoteSource;
  topicScope: NoteScope;
  kinds?: string[];
}

/** The whole review page: the queue on one side, the selected item's editor on the other. */
export function InboxReview({ inbox, notes, topicScope, kinds }: InboxReviewProps) {
  const [items, setItems] = useState<InboxItem[]>([]);
  const [status, setStatus] = useState<"loading" | "ready" | "error">("loading");
  const [error, setError] = useState("");
  const [selectedId, setSelectedId] = useState<string>();
  const [detail, setDetail] = useState<InboxItem>();
  const [reviewed, setReviewed] = useState(0);

  const load = useCallback(() => {
    setStatus("loading");
    inbox.list().then(
      (list) => {
        setItems(list);
        setSelectedId((current) => (current && list.some((i) => i.noteId === current) ? current : list[0]?.noteId));
        setStatus("ready");
      },
      (err: unknown) => {
        setError(err instanceof Error ? err.message : String(err));
        setStatus("error");
      },
    );
  }, [inbox]);

  useEffect(load, [load]);

  // Load the selected item's preview. Until it arrives the editor shows the
  // list's copy, so it never waits on the network to appear.
  useEffect(() => {
    setDetail(undefined);
    if (!selectedId) {
      return;
    }
    let cancelled = false;
    inbox.get(selectedId).then(
      (item) => !cancelled && setDetail(item),
      () => {},
    );
    return () => {
      cancelled = true;
    };
  }, [inbox, selectedId]);

  const selected = items.find((i) => i.noteId === selectedId);
  const current = detail?.noteId === selectedId ? detail : selected;

  async function save(changes: ReviewChanges) {
    if (!selected) {
      return;
    }
    const updated = await inbox.save(selected.noteId, changes);
    if (changes.done) {
      const index = items.findIndex((i) => i.noteId === selected.noteId);
      const rest = items.filter((i) => i.noteId !== selected.noteId);
      setItems(rest);
      setSelectedId(rest[Math.min(index, rest.length - 1)]?.noteId);
      setReviewed((n) => n + 1);
    } else {
      setItems(items.map((i) => (i.noteId === updated.noteId ? { ...updated, preview: undefined } : i)));
      setDetail({ ...updated, preview: current?.preview });
    }
  }

  return (
    <div className="dui-inbox">
      <aside className="dui-inbox__queue">
        <ReviewQueue
          items={items}
          selectedId={selectedId}
          onSelect={setSelectedId}
          status={status}
          error={error}
          onRetry={load}
        />
        {reviewed > 0 && <p className="dui-inbox__progress">{reviewed} reviewed this session</p>}
      </aside>
      <main className="dui-inbox__detail">
        {current ? (
          <ReviewItem
            key={current.noteId}
            item={current}
            source={notes}
            topicScope={topicScope}
            kinds={kinds}
            onSave={save}
            // Each item mounts a fresh editor (key), so after "Mark reviewed"
            // or a click in the queue the topic search is focused, ready for typing.
            autoFocusTopics
          />
        ) : (
          status === "ready" && <p className="dui-inbox__empty">Pick an item to review.</p>
        )}
      </main>
    </div>
  );
}

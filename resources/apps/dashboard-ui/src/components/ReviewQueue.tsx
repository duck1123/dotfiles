import type { InboxItem } from "../inbox/types";
import "./ReviewQueue.css";

export interface ReviewQueueProps {
  items: InboxItem[];
  selectedId?: string;
  onSelect: (noteId: string) => void;
  status?: "loading" | "ready" | "error";
  error?: string;
  onRetry?: () => void;
  title?: string;
}

function host(url?: string): string | undefined {
  if (!url) {
    return undefined;
  }
  try {
    return new URL(url).hostname.replace(/^www\./, "");
  } catch {
    return undefined;
  }
}

/** The created date, without the time. */
function day(created?: string): string | undefined {
  return created?.slice(0, 10);
}

/** The list of items waiting for review. */
export function ReviewQueue({
  items,
  selectedId,
  onSelect,
  status = "ready",
  error,
  onRetry,
  title = "Inbox",
}: ReviewQueueProps) {
  return (
    <nav className="dui-queue" aria-label={title}>
      <h2 className="dui-queue__heading">
        {title}
        {status === "ready" && <span className="dui-queue__count">{items.length}</span>}
      </h2>
      {status === "loading" && <p className="dui-queue__message">Loading…</p>}
      {status === "error" && (
        <div className="dui-queue__message dui-queue__message--error" role="alert">
          <span>Couldn't load the queue: {error}</span>
          {onRetry && (
            <button type="button" className="dui-queue__retry" onClick={onRetry}>
              Retry
            </button>
          )}
        </div>
      )}
      {status === "ready" && items.length === 0 && <p className="dui-queue__message">Nothing left to review.</p>}
      {status === "ready" && items.length > 0 && (
        <ul className="dui-queue__list">
          {items.map((item) => {
            const where = host(item.url);
            const topics = item.topicIds.length;
            return (
              <li key={item.noteId}>
                <button
                  type="button"
                  className="dui-queue__item"
                  aria-current={item.noteId === selectedId ? "true" : undefined}
                  onClick={() => onSelect(item.noteId)}
                >
                  <span className="dui-queue__title">{item.title}</span>
                  <span className="dui-queue__meta">
                    {item.kind && <span className="dui-queue__kind">{item.kind}</span>}
                    {day(item.created) && <span>{day(item.created)}</span>}
                    {where && <span>{where}</span>}
                    <span className={topics === 0 ? "dui-queue__untagged" : undefined}>
                      {topics === 0 ? "no topics" : topics === 1 ? "1 topic" : `${topics} topics`}
                    </span>
                  </span>
                </button>
              </li>
            );
          })}
        </ul>
      )}
    </nav>
  );
}

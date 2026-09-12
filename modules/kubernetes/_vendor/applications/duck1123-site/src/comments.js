import { signEvent } from "./auth.js";
import { publishEvent, getZapAmountSats } from "./nostr.js";
import { renderNoteContent } from "./content.js";

function formatTime(ts) {
  return new Date(ts * 1000).toLocaleString();
}

function renderReply(reply, profiles) {
  const el = document.createElement("div");
  el.className = "reply";

  const profile = profiles?.get(reply.pubkey);

  const header = document.createElement("div");
  header.className = "reply-header";

  if (profile?.picture) {
    const avatar = document.createElement("img");
    avatar.className = "reply-avatar";
    avatar.src = profile.picture;
    avatar.alt = "";
    header.append(avatar);
  }

  const name = document.createElement("span");
  name.className = "reply-name";
  name.textContent = profile?.display_name || profile?.name || `${reply.pubkey.slice(0, 8)}…`;
  header.append(name);

  const content = document.createElement("div");
  content.className = "content";
  renderNoteContent(content, reply);

  const meta = document.createElement("div");
  meta.className = "meta";
  meta.textContent = formatTime(reply.created_at);

  el.append(header, content, meta);
  return el;
}

export function renderReplies(container, replies, profiles) {
  container.innerHTML = "";
  if (!replies.length) return;
  const wrap = document.createElement("div");
  wrap.className = "replies";
  for (const reply of replies) wrap.append(renderReply(reply, profiles));
  container.append(wrap);
}

const DEFAULT_REACTION_EMOJI = "❤️";

function summarizeReactions(reactions) {
  const counts = new Map();
  for (const reaction of reactions) {
    const raw = reaction.content?.trim();
    const emoji = !raw || raw === "+" ? DEFAULT_REACTION_EMOJI : raw === "-" ? "👎" : raw;
    counts.set(emoji, (counts.get(emoji) ?? 0) + 1);
  }
  return counts;
}

export function renderEngagement(container, { reactions, zaps }) {
  container.innerHTML = "";
  const counts = summarizeReactions(reactions);
  if (!counts.size && !zaps.length) return;

  const wrap = document.createElement("div");
  wrap.className = "engagement";

  for (const [emoji, count] of counts) {
    const pill = document.createElement("span");
    pill.className = "pill";
    pill.textContent = `${emoji} ${count}`;
    wrap.append(pill);
  }

  if (zaps.length) {
    const totalSats = zaps.reduce((sum, zap) => sum + (getZapAmountSats(zap) ?? 0), 0);
    const pill = document.createElement("span");
    pill.className = "pill zap";
    pill.textContent =
      totalSats > 0 ? `⚡ ${totalSats.toLocaleString()} sats (${zaps.length})` : `⚡ ${zaps.length}`;
    wrap.append(pill);
  }

  container.append(wrap);
}

export function renderReplyForm({ pool, relays, note, ownerPubkey, myPubkey, onPublished }) {
  const form = document.createElement("form");
  form.className = "reply-form";

  const textarea = document.createElement("textarea");
  textarea.placeholder = "Reply…";
  textarea.required = true;

  const button = document.createElement("button");
  button.type = "submit";
  button.textContent = "Post";

  form.append(textarea, button);

  form.addEventListener("submit", async (event) => {
    event.preventDefault();
    const content = textarea.value.trim();
    if (!content) return;

    button.disabled = true;
    button.textContent = "Posting…";

    try {
      const relay = relays[0] ?? "";
      const unsigned = {
        kind: 1,
        pubkey: myPubkey,
        created_at: Math.floor(Date.now() / 1000),
        tags: [
          ["e", note.id, relay, "reply"],
          ["p", ownerPubkey],
        ],
        content,
      };
      const signed = await signEvent(unsigned);
      await publishEvent(pool, signed, relays);
      textarea.value = "";
      onPublished?.(signed);
    } catch (err) {
      alert(`Failed to post reply: ${err.message ?? err}`);
    } finally {
      button.disabled = false;
      button.textContent = "Post";
    }
  });

  return form;
}

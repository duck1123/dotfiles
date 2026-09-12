import { SimplePool } from "nostr-tools";

export function createPool() {
  return new SimplePool();
}

export async function fetchProfiles(pool, pubkeys, relays) {
  const unique = [...new Set(pubkeys)];
  const profiles = new Map();
  if (!unique.length) return profiles;

  const events = await pool.querySync(relays, {
    kinds: [0],
    authors: unique,
  });

  const latestByPubkey = new Map();
  for (const event of events) {
    const existing = latestByPubkey.get(event.pubkey);
    if (!existing || event.created_at > existing.created_at) {
      latestByPubkey.set(event.pubkey, event);
    }
  }

  for (const [pubkey, event] of latestByPubkey) {
    try {
      profiles.set(pubkey, JSON.parse(event.content));
    } catch {
      // skip malformed metadata
    }
  }
  return profiles;
}

export async function fetchProfile(pool, pubkey, relays) {
  const profiles = await fetchProfiles(pool, [pubkey], relays);
  return profiles.get(pubkey) ?? null;
}

export async function fetchNotes(pool, pubkey, relays, limit = 20) {
  const events = await pool.querySync(relays, {
    kinds: [1],
    authors: [pubkey],
    limit,
  });
  return events.sort((a, b) => b.created_at - a.created_at);
}

// Single-level replies only (NIP-10 "reply" marker) — not full threading.
export async function fetchReplies(pool, noteIds, relays) {
  if (!noteIds.length) return [];
  const events = await pool.querySync(relays, {
    kinds: [1],
    "#e": noteIds,
  });
  return events.sort((a, b) => a.created_at - b.created_at);
}

// NIP-25 reactions (likes/emoji).
export async function fetchReactions(pool, noteIds, relays) {
  if (!noteIds.length) return [];
  return pool.querySync(relays, {
    kinds: [7],
    "#e": noteIds,
  });
}

// NIP-57 zap receipts.
export async function fetchZaps(pool, noteIds, relays) {
  if (!noteIds.length) return [];
  return pool.querySync(relays, {
    kinds: [9735],
    "#e": noteIds,
  });
}

const BOLT11_MULTIPLIERS = { m: 1e-3, u: 1e-6, n: 1e-9, p: 1e-12 };

function decodeBolt11AmountSats(bolt11) {
  if (!bolt11) return null;
  const match = /^ln(?:bc|tb|bcrt)(\d+)([munp]?)1/i.exec(bolt11);
  if (!match) return null;
  const [, digits, multiplier] = match;
  const btc = Number(digits) * (multiplier ? BOLT11_MULTIPLIERS[multiplier.toLowerCase()] : 1);
  return Math.round(btc * 1e8);
}

// Amount is authoritative on the embedded zap request (NIP-57); bolt11
// decoding is only a fallback for zap receipts missing that tag.
export function getZapAmountSats(zapReceipt) {
  const descriptionTag = zapReceipt.tags.find((t) => t[0] === "description");
  if (descriptionTag?.[1]) {
    try {
      const zapRequest = JSON.parse(descriptionTag[1]);
      const amountTag = zapRequest.tags?.find((t) => t[0] === "amount");
      const msat = Number(amountTag?.[1]);
      if (Number.isFinite(msat) && msat > 0) return Math.round(msat / 1000);
    } catch {
      // fall through to bolt11 decode
    }
  }
  const bolt11Tag = zapReceipt.tags.find((t) => t[0] === "bolt11");
  return decodeBolt11AmountSats(bolt11Tag?.[1]);
}

export function publishEvent(pool, event, relays) {
  return Promise.allSettled(pool.publish(relays, event));
}

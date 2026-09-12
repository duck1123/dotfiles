// Best-effort per-viewer cache (localStorage) so a reload shows the
// last-seen content immediately instead of blank "Loading…" placeholders.
// Fresh data is always still fetched from relays in the background and
// replaces whatever was cached — this only affects the first paint.
const PREFIX = "duck1123:v1";

function read(key, fallback) {
  try {
    const raw = localStorage.getItem(key);
    return raw === null ? fallback : JSON.parse(raw);
  } catch {
    return fallback;
  }
}

function write(key, value) {
  try {
    localStorage.setItem(key, JSON.stringify(value));
  } catch {
    // storage full/disabled/private-mode — cache is best-effort only
  }
}

function remove(key) {
  try {
    localStorage.removeItem(key);
  } catch {
    // ignore
  }
}

export function getCachedProfile() {
  return read(`${PREFIX}:profile`, null);
}
export function setCachedProfile(profile) {
  write(`${PREFIX}:profile`, profile);
}

export function getCachedNotes() {
  return read(`${PREFIX}:notes`, null);
}
export function setCachedNotes(notes) {
  write(`${PREFIX}:notes`, notes);
}

export function getCachedReplies(noteId) {
  return read(`${PREFIX}:replies:${noteId}`, null);
}
export function setCachedReplies(noteId, replies) {
  write(`${PREFIX}:replies:${noteId}`, replies);
}

export function getCachedEngagement(noteId) {
  return read(`${PREFIX}:engagement:${noteId}`, null);
}
export function setCachedEngagement(noteId, engagement) {
  write(`${PREFIX}:engagement:${noteId}`, engagement);
}

// Reply-author profiles accumulate across notes rather than growing per-note,
// so they're kept in one shared blob instead of one key per pubkey.
export function getCachedProfiles() {
  return new Map(Object.entries(read(`${PREFIX}:profiles`, {})));
}
export function mergeCachedProfiles(profilesMap) {
  const merged = read(`${PREFIX}:profiles`, {});
  for (const [pubkey, profile] of profilesMap) {
    merged[pubkey] = profile;
  }
  write(`${PREFIX}:profiles`, merged);
}

// Drops replies/engagement cached for notes that no longer appear in the
// latest fetch, so localStorage doesn't grow forever as old notes (deleted,
// or aged out of fetchNotes' limit) leave orphaned entries behind.
export function pruneNoteCache(currentNoteIds) {
  const keep = new Set(currentNoteIds);
  try {
    for (let i = localStorage.length - 1; i >= 0; i--) {
      const key = localStorage.key(i);
      const match = key?.match(new RegExp(`^${PREFIX}:(?:replies|engagement):(.+)$`));
      if (match && !keep.has(match[1])) {
        remove(key);
      }
    }
  } catch {
    // ignore
  }
}

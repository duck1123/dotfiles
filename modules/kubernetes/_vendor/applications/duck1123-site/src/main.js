import "./style.css";
import { CONFIG } from "./config.js";
import {
  createPool,
  fetchProfile,
  fetchProfiles,
  fetchNotes,
  fetchReplies,
  fetchReactions,
  fetchZaps,
} from "./nostr.js";
import { hasExtension, login } from "./auth.js";
import { renderReplies, renderReplyForm, renderEngagement } from "./comments.js";
import { renderNoteContent } from "./content.js";
import {
  getCachedProfile,
  setCachedProfile,
  getCachedNotes,
  setCachedNotes,
  getCachedReplies,
  setCachedReplies,
  getCachedEngagement,
  setCachedEngagement,
  getCachedProfiles,
  mergeCachedProfiles,
  pruneNoteCache,
} from "./cache.js";

const pool = createPool();
let myPubkey = null;

const profileEl = document.getElementById("profile");
const notesEl = document.getElementById("notes");
const loginBtn = document.getElementById("login-btn");
const authStatus = document.getElementById("auth-status");

function renderProfile(profile) {
  profileEl.innerHTML = "";
  if (!profile) {
    profileEl.innerHTML = '<p class="error">Could not load profile from configured relays.</p>';
    return;
  }

  if (profile.banner) {
    const banner = document.createElement("img");
    banner.className = "banner";
    banner.src = profile.banner;
    banner.alt = "";
    profileEl.append(banner);
  }

  const row = document.createElement("div");
  row.className = "profile-row";

  if (profile.picture) {
    const img = document.createElement("img");
    img.className = "avatar";
    img.src = profile.picture;
    img.alt = profile.name ?? "avatar";
    row.append(img);
  }

  const info = document.createElement("div");

  const h1 = document.createElement("h1");
  h1.textContent = profile.display_name || profile.name || `${CONFIG.pubkey.slice(0, 8)}…`;
  info.append(h1);

  if (profile.about) {
    const about = document.createElement("p");
    about.className = "about";
    about.textContent = profile.about;
    info.append(about);
  }

  if (profile.nip05) {
    const nip05 = document.createElement("p");
    nip05.className = "nip05";
    nip05.textContent = `✓ ${profile.nip05}`;
    info.append(nip05);
  }

  if (profile.website) {
    const website = document.createElement("a");
    website.className = "website";
    website.href = profile.website;
    website.target = "_blank";
    website.rel = "noopener noreferrer";
    website.textContent = profile.website.replace(/^https?:\/\//, "");
    info.append(website);
  }

  const lightningAddress = profile.lud16 || profile.lud06;
  if (lightningAddress) {
    const lightning = document.createElement("p");
    lightning.className = "lightning";
    lightning.textContent = `⚡ ${lightningAddress}`;
    info.append(lightning);
  }

  row.append(info);
  profileEl.append(row);
}

function renderNote(note) {
  const el = document.createElement("article");
  el.className = "note";

  const content = document.createElement("div");
  content.className = "content";
  renderNoteContent(content, note);

  const meta = document.createElement("div");
  meta.className = "meta";
  meta.textContent = new Date(note.created_at * 1000).toLocaleString();

  const engagementContainer = document.createElement("div");
  const repliesContainer = document.createElement("div");

  el.append(content, meta, engagementContainer, repliesContainer);

  async function loadReplies() {
    const cachedReplies = getCachedReplies(note.id);
    if (cachedReplies) {
      renderReplies(repliesContainer, cachedReplies, getCachedProfiles());
    }

    const replies = await fetchReplies(pool, [note.id], CONFIG.relays);
    const profiles = await fetchProfiles(
      pool,
      replies.map((reply) => reply.pubkey),
      CONFIG.relays,
    );
    mergeCachedProfiles(profiles);
    setCachedReplies(note.id, replies);
    renderReplies(repliesContainer, replies, profiles);
  }

  async function loadEngagement() {
    const cachedEngagement = getCachedEngagement(note.id);
    if (cachedEngagement) renderEngagement(engagementContainer, cachedEngagement);

    const [reactions, zaps] = await Promise.all([
      fetchReactions(pool, [note.id], CONFIG.relays),
      fetchZaps(pool, [note.id], CONFIG.relays),
    ]);
    const engagement = { reactions, zaps };
    setCachedEngagement(note.id, engagement);
    renderEngagement(engagementContainer, engagement);
  }

  if (myPubkey) {
    const form = renderReplyForm({
      pool,
      relays: CONFIG.relays,
      note,
      ownerPubkey: CONFIG.pubkey,
      myPubkey,
      onPublished: loadReplies,
    });
    el.append(form);
  }

  loadReplies();
  loadEngagement();

  return el;
}

async function renderNotes() {
  const cachedNotes = getCachedNotes();
  if (cachedNotes?.length) {
    notesEl.innerHTML = "";
    for (const note of cachedNotes) notesEl.append(renderNote(note));
  } else {
    notesEl.innerHTML = '<p class="loading">Loading notes…</p>';
  }

  const notes = await fetchNotes(pool, CONFIG.pubkey, CONFIG.relays);
  if (!notes.length) {
    if (!cachedNotes?.length) {
      notesEl.innerHTML = '<p class="error">No notes found on the configured relays.</p>';
    }
    return;
  }

  setCachedNotes(notes);
  pruneNoteCache(notes.map((note) => note.id));
  notesEl.innerHTML = "";
  for (const note of notes) {
    notesEl.append(renderNote(note));
  }
}

function updateAuthUI() {
  if (myPubkey) {
    loginBtn.style.display = "none";
    authStatus.textContent = `Logged in as ${myPubkey.slice(0, 8)}…`;
  } else if (!hasExtension()) {
    loginBtn.disabled = true;
    loginBtn.textContent = "No Nostr extension found";
  } else {
    loginBtn.style.display = "inline-block";
    authStatus.textContent = "";
  }
}

loginBtn.addEventListener("click", async () => {
  try {
    myPubkey = await login();
    updateAuthUI();
    await renderNotes();
  } catch (err) {
    alert(err.message ?? String(err));
  }
});

function loadProfile() {
  const cachedProfile = getCachedProfile();
  if (cachedProfile) renderProfile(cachedProfile);

  fetchProfile(pool, CONFIG.pubkey, CONFIG.relays).then((profile) => {
    if (profile) {
      setCachedProfile(profile);
      renderProfile(profile);
    } else if (!cachedProfile) {
      renderProfile(null);
    }
  });
}

updateAuthUI();
loadProfile();
renderNotes();

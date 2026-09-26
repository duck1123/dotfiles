import { parse } from "nostr-tools/nip27";

const MAX_URL_LABEL = 40;

function truncateMiddle(str, max) {
  if (str.length <= max) return str;
  const half = Math.floor((max - 1) / 2);
  return `${str.slice(0, half)}…${str.slice(-half)}`;
}

function renderToken(token) {
  switch (token.type) {
    case "hashtag": {
      const span = document.createElement("span");
      span.className = "hashtag";
      span.textContent = `#${token.value}`;
      return span;
    }
    case "reference": {
      const span = document.createElement("span");
      span.className = "mention";
      const { pubkey, id } = token.pointer;
      span.textContent = pubkey ? `@${pubkey.slice(0, 8)}…` : id ? `note:${id.slice(0, 8)}…` : "@mention";
      return span;
    }
    case "url": {
      const a = document.createElement("a");
      a.href = token.url;
      a.target = "_blank";
      a.rel = "noopener noreferrer";
      a.className = "note-link";
      a.textContent = truncateMiddle(token.url.replace(/^https?:\/\//, ""), MAX_URL_LABEL);
      return a;
    }
    case "image": {
      const a = document.createElement("a");
      a.className = "note-image-link";
      a.href = token.url;
      a.target = "_blank";
      a.rel = "noopener noreferrer";
      const img = document.createElement("img");
      img.className = "note-image";
      img.src = token.url;
      img.alt = "";
      img.loading = "lazy";
      // Nostr image CDNs die/rotate often — a broken link shouldn't leave a blank box.
      img.onerror = () => a.remove();
      a.append(img);
      return a;
    }
    case "video": {
      const video = document.createElement("video");
      video.className = "note-media";
      video.src = token.url;
      video.controls = true;
      video.preload = "metadata";
      return video;
    }
    case "audio": {
      const audio = document.createElement("audio");
      audio.className = "note-media";
      audio.src = token.url;
      audio.controls = true;
      audio.preload = "metadata";
      return audio;
    }
    case "emoji": {
      const img = document.createElement("img");
      img.className = "note-emoji";
      img.src = token.url;
      img.alt = `:${token.shortcode}:`;
      img.title = `:${token.shortcode}:`;
      return img;
    }
    case "relay":
      return document.createTextNode(token.url);
    case "text":
    default:
      return document.createTextNode(token.text ?? "");
  }
}

// Renders an event's content per NIP-27 (nostr-tools' parser splits it into
// text/url/image/video/audio/hashtag/mention/custom-emoji tokens), so plain
// image/video/audio links and NIP-30 custom emoji show inline instead of as
// raw URLs.
export function renderNoteContent(container, event) {
  container.innerHTML = "";
  for (const token of parse(event)) {
    container.append(renderToken(token));
  }
}

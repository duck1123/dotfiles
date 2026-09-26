// NIP-07 login — signing happens entirely inside the visitor's browser
// extension (e.g. Alby, nos2x); this page never sees a private key.

export function hasExtension() {
  return typeof window !== "undefined" && !!window.nostr;
}

export async function login() {
  if (!hasExtension()) {
    throw new Error("No NIP-07 extension found (try Alby or nos2x)");
  }
  return window.nostr.getPublicKey();
}

export async function signEvent(event) {
  if (!hasExtension()) {
    throw new Error("No NIP-07 extension found");
  }
  return window.nostr.signEvent(event);
}

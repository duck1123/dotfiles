// Local server for the Sims mod web UI, started by `sims web` (simsnix,
// nushell/modules/sims_module.nu). It serves the built page and a small JSON
// API that runs `sims` commands in Nushell. Plain Node, no dependencies.
//
// Environment (set by `sims web`):
//   SIMS_MODULE    sims_module.nu, which each command `use`s
//   SIMS_NU        the nu binary (default: nu on PATH)
//   SIMS_WEB_ROOT  the built page (dist-sims/)
//   PORT           port on 127.0.0.1 (default 7341)
//
// It listens on 127.0.0.1 only and answers only requests addressed to it by
// that name (Host header), so another site can't reach it through DNS
// rebinding. The mod list is private: it never leaves this machine.

import { spawn } from "node:child_process";
import { readFile, stat } from "node:fs/promises";
import { createServer } from "node:http";
import { extname, resolve, sep } from "node:path";

const port = Number(process.env.PORT ?? 7341);
const root = resolve(process.env.SIMS_WEB_ROOT ?? "dist-sims");
const nu = process.env.SIMS_NU ?? "nu";
const simsModule = process.env.SIMS_MODULE;
if (!simsModule) {
  console.error("SIMS_MODULE isn't set: start this with `sims web`");
  process.exit(1);
}

const allowedHosts = new Set([`127.0.0.1:${port}`, `localhost:${port}`]);

// Each route runs a fixed Nushell snippet. Request values reach it only as the
// record $args (JSON in the environment), never as script text.
const routes = {
  "GET /api/mods": {
    args: (url) => ({ profile: url.searchParams.get("profile") || null }),
    script: "sims mod list --profile $args.profile?",
  },
};

class SimsError extends Error {}

/** The message of a Nushell error, from its plain-style stderr. */
export function nuErrorMessage(stderr) {
  const lines = stderr.split("\n");
  const at = lines.findIndex((l) => l.startsWith("Error: "));
  if (at < 0) {
    return stderr.trim() || "the sims command failed";
  }
  const first = lines[at].slice("Error: ".length).trim();
  // nu's own errors put a code first and the explanation below it
  if (/^nu::[\w:]+$/.test(first)) {
    const detail = lines.slice(at + 1).find((l) => l.trim() && !/Diagnostic severity|^Begin snippet/.test(l.trim()));
    return detail ? `${detail.trim()} (${first})` : first;
  }
  return first;
}

function runSims(script, args) {
  const program = [
    `$env.config.error_style = "plain"`,
    `use ${JSON.stringify(simsModule)} *`,
    `let args = $env.SIMS_ARGS | from json`,
    `${script} | to json -r`,
  ].join("\n");
  return new Promise((resolvePromise, reject) => {
    const child = spawn(nu, ["--no-config-file", "-c", program], {
      env: { ...process.env, SIMS_ARGS: JSON.stringify(args), NO_COLOR: "1" },
      stdio: ["ignore", "pipe", "pipe"],
    });
    let out = "";
    let err = "";
    child.stdout.setEncoding("utf8").on("data", (d) => (out += d));
    child.stderr.setEncoding("utf8").on("data", (d) => (err += d));
    child.on("error", reject);
    child.on("close", (code) => {
      if (code !== 0) {
        console.error(err.trim());
        reject(new SimsError(nuErrorMessage(err)));
        return;
      }
      if (err.trim()) {
        console.error(err.trim());
      }
      try {
        resolvePromise(out.trim() ? JSON.parse(out) : null);
      } catch (e) {
        reject(new Error(`unreadable output from sims: ${e.message}`));
      }
    });
  });
}

const types = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".svg": "image/svg+xml",
  ".png": "image/png",
  ".ico": "image/x-icon",
  ".json": "application/json",
  ".woff2": "font/woff2",
};

const securityHeaders = {
  "X-Content-Type-Options": "nosniff",
  "Referrer-Policy": "no-referrer",
  "Content-Security-Policy":
    "default-src 'self'; img-src 'self' data:; style-src 'self' 'unsafe-inline'; frame-ancestors 'none'; base-uri 'none'",
};

function send(res, status, body, headers = {}) {
  res.writeHead(status, { ...securityHeaders, ...headers });
  res.end(body);
}

function sendJson(res, status, value) {
  send(res, status, JSON.stringify(value), { "Content-Type": "application/json", "Cache-Control": "no-store" });
}

async function serveStatic(res, pathname) {
  let decoded;
  try {
    decoded = decodeURIComponent(pathname);
  } catch {
    return send(res, 400, "Bad request");
  }
  let file = resolve(root, "." + decoded);
  if (file !== root && !file.startsWith(root + sep)) {
    return send(res, 403, "Forbidden");
  }
  const info = await stat(file).catch(() => null);
  if (!info || info.isDirectory()) {
    // the app has one page; anything else that isn't a file gets it too
    file = resolve(root, "index.html");
  }
  try {
    const body = await readFile(file);
    const hashed = file.includes(`${sep}assets${sep}`);
    send(res, 200, body, {
      "Content-Type": types[extname(file)] ?? "application/octet-stream",
      "Cache-Control": hashed ? "public, max-age=31536000, immutable" : "no-cache",
    });
  } catch {
    send(res, 404, "Not found");
  }
}

const server = createServer((req, res) => {
  handle(req, res).catch((e) => {
    console.error(e);
    if (!res.headersSent) {
      sendJson(res, 500, { error: "internal error" });
    }
  });
});

async function handle(req, res) {
  if (!allowedHosts.has(req.headers.host ?? "")) {
    return send(res, 403, "Forbidden: use http://127.0.0.1:" + port);
  }
  const url = new URL(req.url ?? "/", `http://${req.headers.host}`);
  if (url.pathname.startsWith("/api/")) {
    const route = routes[`${req.method} ${url.pathname}`];
    if (!route) {
      return sendJson(res, 404, { error: `no route ${req.method} ${url.pathname}` });
    }
    try {
      return sendJson(res, 200, await runSims(route.script, route.args(url)));
    } catch (e) {
      return sendJson(res, e instanceof SimsError ? 400 : 500, { error: e.message });
    }
  }
  if (req.method !== "GET" && req.method !== "HEAD") {
    return send(res, 405, "Method not allowed");
  }
  return serveStatic(res, url.pathname);
}

server.listen(port, "127.0.0.1", () => {
  console.error(`sims web: serving ${root} on http://127.0.0.1:${port}/`);
});

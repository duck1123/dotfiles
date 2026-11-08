import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// The Sims mod web UI (apps/sims), built into dist-sims/ for `sims web`.
// `npx vite --config vite.sims.config.ts` serves it with hot reload, sending
// api/ to a `sims web --root dist-sims` already running on the default port.
export default defineConfig({
  root: "apps/sims",
  base: "./",
  plugins: [react()],
  build: {
    outDir: "../../dist-sims",
    emptyOutDir: true,
  },
  server: {
    port: 7342,
    proxy: { "/api": "http://127.0.0.1:7341" },
  },
});

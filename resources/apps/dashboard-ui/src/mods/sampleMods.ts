import type { ModList, ModSummary } from "./types";

// Made-up mods for stories and tests. Never put real mod names here: this repo
// is public and the mod list is private (Simsnix rule).

function mod(id: string, fields: Partial<ModSummary> = {}): ModSummary {
  return {
    id,
    title: null,
    downloadPage: null,
    tags: [],
    enable: true,
    folder: id.replace("/", "_"),
    version: null,
    selectedVersion: "1.0",
    requires: [],
    conflicts: [],
    versions: { "1.0": { added: "2026-05-17", gameVersion: null, files: [{ name: `${id.split("/")[1]}.package`, members: 0 }] } },
    editable: true,
    profiles: ["current"],
    problems: [],
    ...fields,
  };
}

export const sampleMods: ModSummary[] = [
  mod("acme/core", {
    title: "Acme Core Library",
    downloadPage: "https://example.com/acme/core",
    tags: ["library", "script"],
    selectedVersion: "2.4.1",
    versions: {
      "2.3.0": { added: "2025-10-09", gameVersion: "1.110", files: [{ name: "AcmeCore_2.3.0.zip", members: 3 }] },
      "2.4.1": { added: "2026-05-17", gameVersion: "1.116", files: [{ name: "AcmeCore_2.4.1.zip", members: 4 }] },
    },
  }),
  mod("acme/widgets", {
    title: "Acme Widgets",
    downloadPage: "https://example.com/acme/widgets",
    tags: ["build-buy"],
    requires: ["acme/core"],
  }),
  mod("acme/widgets-extra", {
    title: "Acme Widgets: Extra Colours",
    tags: ["build-buy", "cc"],
    requires: ["acme/widgets"],
    enable: false,
    profiles: [],
  }),
  mod("zed/loose", {
    title: null,
    folder: null,
    version: "2025-12-01",
    selectedVersion: "2025-12-01",
    versions: {
      "2025-12-01": { added: "2025-12-01", gameVersion: null, files: [{ name: "zed_loose.package", members: 0 }] },
      "2026-02-14": { added: "2026-02-14", gameVersion: null, files: [{ name: "zed_loose_v2.package", members: 0 }] },
    },
  }),
  mod("nimbus/weather", {
    title: "Nimbus Seasonal Weather Overhaul With A Rather Long Title That Wraps",
    downloadPage: "https://downloads.example.org/files/nimbus-weather-overhaul-seasonal",
    tags: ["gameplay", "script"],
    conflicts: ["zed/loose"],
    problems: ["nimbus/weather conflicts with zed/loose, which is also enabled"],
  }),
  mod("unknown/mystery-pack", {
    title: "mystery_pack_v3",
    tags: [],
    enable: false,
    profiles: [],
  }),
  mod("pixel/hair-set", {
    title: "Pixel Hair Set",
    tags: ["cc"],
    editable: false,
  }),
];

export const sampleModList: ModList = {
  profile: "current",
  profiles: ["current", "testing"],
  problems: [
    "nimbus/weather conflicts with zed/loose, which is also enabled",
    "profile testing: unknown base profile vanilla",
  ],
  mods: sampleMods,
};

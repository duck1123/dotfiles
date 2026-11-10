import { placeTarget } from "./placement";
import type { ModList, ModSummary } from "./types";

// Made-up mods for stories and tests. Never put real mod names here: this repo
// is public and the mod list is private (Simsnix rule).

/** A made-up 64-digit hex hash, stable per seed. */
function fakeHash(seed: string): string {
  let h = 0;
  for (const c of seed) h = (h * 31 + c.charCodeAt(0)) >>> 0;
  return h.toString(16).padStart(8, "0").repeat(8);
}

/** A sample mod; its installs default to the selected version's plain files. */
function mod(id: string, fields: Partial<ModSummary> = {}): ModSummary {
  const m: ModSummary = {
    id,
    title: null,
    downloadPage: null,
    tags: [],
    enable: true,
    folder: id.replace("/", "_"),
    defaultFolder: id.replace("/", "_"),
    flatten: false,
    stateFiles: [],
    installs: [],
    version: null,
    selectedVersion: "1.0",
    requires: [],
    conflicts: [],
    versions: { "1.0": { added: "2026-05-17", gameVersion: null, files: [{ name: `${id.split("/")[1]}.package`, sha256: fakeHash(id), members: 0 }] } },
    editable: true,
    profiles: ["current"],
    problems: [],
    ...fields,
  };
  if (!fields.installs) {
    const files = m.selectedVersion ? (m.versions[m.selectedVersion]?.files ?? []) : [];
    m.installs = files
      .filter((f) => f.members === 0)
      .map((f) => ({ source: f.name, sha256: f.sha256, target: placeTarget(f.name, m) }));
  }
  return m;
}

export const sampleMods: ModSummary[] = [
  mod("acme/core", {
    title: "Acme Core Library",
    downloadPage: "https://example.com/acme/core",
    tags: ["library", "script"],
    selectedVersion: "2.4.1",
    stateFiles: ["*.log", "settings.cfg"],
    versions: {
      "2.3.0": { added: "2025-10-09", gameVersion: "1.110", files: [{ name: "AcmeCore_2.3.0.zip", sha256: fakeHash("core-2.3.0"), members: 3 }] },
      "2.4.1": { added: "2026-05-17", gameVersion: "1.116", files: [{ name: "AcmeCore_2.4.1.zip", sha256: fakeHash("core-2.4.1"), members: 4 }] },
    },
    installs: [
      { source: "AcmeCore.ts4script", sha256: fakeHash("core-script"), target: "acme_core/AcmeCore.ts4script" },
      { source: "AcmeCore.package", sha256: fakeHash("core-package"), target: "acme_core/AcmeCore.package" },
      { source: "Extras/AcmeCore_Extras.package", sha256: fakeHash("core-extras"), target: "acme_core/Extras/AcmeCore_Extras.package" },
    ],
  }),
  // one of several mods imported from a creator's shared folder, installed where it was found
  mod("acme/tweak-moodlets", {
    title: "Acme Moodlet Tweak",
    folder: null,
    versions: {
      "2026-03-07": {
        added: "2026-03-07",
        gameVersion: null,
        files: [{ name: "Acme/Acme_TweakMoodlets.package", sha256: fakeHash("tweak-moodlets"), members: 0 }],
      },
    },
    version: "2026-03-07",
    selectedVersion: "2026-03-07",
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
      "2025-12-01": { added: "2025-12-01", gameVersion: null, files: [{ name: "zed_loose.package", sha256: fakeHash("loose-1"), members: 0 }] },
      "2026-02-14": {
        added: "2026-02-14",
        gameVersion: null,
        files: [
          { name: "zed_loose_v2.package", sha256: fakeHash("loose-2"), members: 0 },
          // the same file imported from two places
          { name: "Old/zed_loose_v2.package", sha256: fakeHash("loose-2"), members: 0 },
        ],
      },
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

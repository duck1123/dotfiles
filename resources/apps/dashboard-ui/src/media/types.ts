// A video file the integrity checker flagged, as the review component needs it.
// Fetching and saving is the host's job.

export interface MediaSpot {
  /** Decode position when the errors were logged, in seconds; see spotWindow. */
  t: number | null;
  lines: number;
  rules: string[];
}

export interface MediaFile {
  id: number;
  library: string;
  /** Relative to the library root. */
  path: string;
  status: string;
  size: number;
  /** Container duration, seconds. */
  duration: number | null;
  /** How far the decode got, seconds. */
  decoded: number | null;
  checkedAt?: string;
  /** ffmpeg's error lines, before any cap. */
  lineCount?: number;
  /** What the checker concluded: suspect, minor or unclear. */
  verdict?: string;
  damage: number;
  unknown: number;
  spots: MediaSpot[];
  unknownSamples: string[];
  truncated?: { duration: number; decoded: number };
  fatal?: { exitCode: number; last: string };
  /** The error lines, once loaded. */
  log?: { t: number | null; msg: string }[];
}

/** Which files to show: waiting on a decision, or already decided. */
export type MediaView = "open" | "replace" | "good";

export type MediaDecision = "good" | "replace" | "reopen";

/** Where a media review gets its files and records decisions. */
export interface MediaSource {
  list(view: MediaView): Promise<MediaFile[]>;
  /** One file with its log loaded. */
  get(id: number): Promise<MediaFile>;
  /** Record a decision; returns the updated file. */
  decide(id: number, decision: MediaDecision): Promise<MediaFile>;
}

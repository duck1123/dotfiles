// Sorts the error output of a full ffmpeg decode into a verdict. Imported by
// f/mediacheck/checker; main() runs it on a log by hand, to try a rule change.
//
// Every line is matched against RULES in order. A "damage" line means the
// file itself is broken there (a hole in the container, a frame that wouldn't
// decode). A "benign" line is noise ffmpeg prints for files that play fine.
// A line no rule knows counts as unknown.
//
// Damage in the first or last EDGE_SECONDS doesn't count: MPEG-2 and TS
// recordings (the files tdarr flagged that played fine) often start before a
// keyframe or end mid-frame. ffmpeg exits 0 even for a broken file, so the
// exit code only adds a "fatal" finding; a decode that stopped short of the
// container's duration adds a "truncated" one.
//
// Verdict: with damage, minor if it's at most MINOR_SPOTS small spots with no
// container damage, truncation or crash, else suspect. Without damage,
// unclear if any line is unknown, else ok.

export type LogLine = { t: number | null; msg: string };

export type Findings = {
  verdict: "ok" | "minor" | "suspect" | "unclear";
  damage: number;
  benign: number;
  unknown: number;
  edge: number;
  rules: Record<string, number>;
  spots: { t: number | null; lines: number; rules: string[] }[];
  unknownSamples: string[];
  truncated?: { duration: number; decoded: number };
  fatal?: { exitCode: number; last: string };
};

type Rule = { id: string; kind: "damage" | "benign"; re: RegExp };

export const RULES: Rule[] = [
  // Before invalid-data: a decoder rejecting one packet isn't container damage.
  { id: "decoder-error", kind: "damage", re: /Error submitting packet to decoder|Error while decoding stream|Decoding error/ },
  // Container damage
  { id: "mkv-ebml", kind: "damage", re: /invalid as first byte of an EBML number/ },
  { id: "mkv-element", kind: "damage", re: /exceeds containing master element|Invalid EBML|Unknown-sized element .* inside parent with finite size/ },
  { id: "invalid-data", kind: "damage", re: /Invalid data found when processing input|Packet corrupt|corrupt decoded frame|corrupt input packet/ },
  { id: "mp4-truncated", kind: "damage", re: /stream \d+, offset 0x[0-9a-f]+: partial file|moov atom not found/ },
  // Video bitstream damage
  { id: "decode-mb", kind: "damage", re: /error while decoding MB|concealing \d+ DC, \d+ AC, \d+ MV errors/ },
  { id: "missing-ref", kind: "damage", re: /Missing reference picture|reference picture missing|Could not find ref with POC/ },
  { id: "nal", kind: "damage", re: /Invalid NAL unit size|missing picture in access unit|decode_slice_header error|no frame!|Error parsing NAL unit|PPS id out of range|non-existing PPS|SPS \d+ does not exist/ },
  { id: "mpeg-video", kind: "damage", re: /ac-tex damaged|invalid mb type|Warning MVs not available|skipped MB in I-frame|slice mismatch|end mismatch|qscale == 0|marker does not match|Error at MB: \d+|error while decoding block/ },
  // Audio bitstream damage
  { id: "aac", kind: "damage", re: /channel element \d+\.\d+ is not allocated|Number of bands \(\d+\) exceeds limit|Prediction is not allowed in AAC-LC|Input buffer exhausted before END element|Reserved bit set|invalid band type/ },
  { id: "ac3-dts", kind: "damage", re: /\[(ac3|eac3|dca|truehd|mlp)\b.*(frame sync error|error decoding|Invalid|incomplete frame|out of range|out-of-range)/ },
  { id: "mpeg-audio", kind: "damage", re: /\[(mp1|mp2|mp3)\w*\b.*(Header missing|invalid new backstep|overread|big_values too big)/ },

  // Noise
  { id: "timestamps", kind: "benign", re: /non monotonically increasing dts|Timestamps are unset|Invalid timestamp|out of order|Packet with invalid duration|Could not update timestamps|pts has no value|DTS discontinuity/ },
  { id: "h264-refs", kind: "benign", re: /number of reference frames .* exceeds max|mmco: unref short failure|co located POCs unavailable|illegal short term buffer state|Increasing reorder buffer|first frame is no keyframe/ },
  { id: "probe", kind: "benign", re: /Could not find codec parameters|not enough frames to estimate rate|max_analyze_duration .* reached|Estimating duration from bitrate|decoding for stream \d+ failed/ },
  { id: "metadata", kind: "benign", re: /Referenced QT chapter track not found|Unknown cover type|Discarding ID3 tags|deprecated pixel format|Unsupported encoding type|Ignoring attempt to set invalid timebase/ },
  // Old DivX/MS-MPEG4 encoders leave these all through files that play fine.
  { id: "divx-overflow", kind: "benign", re: /dc overflow|ignoring overflow at|I-frame too long, ignoring ext header/ },
  { id: "empty", kind: "benign", re: /^\[[^\]]+\]\s*,?$/ },
];

// Damage that makes a file suspect however little of it there is.
const SEVERE = ["mkv-ebml", "mkv-element", "invalid-data", "mp4-truncated"];
// Otherwise this little damage makes it only minor: a glitch or two.
const MINOR_SPOTS = 3;
const MINOR_LINES = 10;

export const EDGE_SECONDS = 3;
const SPOT_GAP_SECONDS = 10;
const UNKNOWN_SAMPLES = 10;

// "[h264 @ 0x5d1d2d33ae40] error ..." -> "[h264] error ...", so repeats group.
export function normalize(msg: string): string {
  return msg.replace(/ @ 0x[0-9a-f]+\]/, "]").trim();
}

export function classify(
  log: LogLine[],
  { duration = null, decoded = null, exitCode = 0 }: { duration?: number | null; decoded?: number | null; exitCode?: number } = {},
): Findings {
  const f: Findings = { verdict: "ok", damage: 0, benign: 0, unknown: 0, edge: 0, rules: {}, spots: [], unknownSamples: [] };
  const end = decoded ?? duration;
  let previous: { rule: Rule | null; t: number | null; msg: string } | null = null;

  const count = (rule: Rule | null, t: number | null, msg: string, times: number) => {
    const atEdge = t !== null && (t < EDGE_SECONDS || (end !== null && t > end - EDGE_SECONDS));
    if (!rule) {
      f.unknown += times;
      if (f.unknownSamples.length < UNKNOWN_SAMPLES && !f.unknownSamples.includes(msg)) f.unknownSamples.push(msg);
      return;
    }
    f.rules[rule.id] = (f.rules[rule.id] ?? 0) + times;
    if (rule.kind === "benign") f.benign += times;
    else if (atEdge) f.edge += times;
    else {
      f.damage += times;
      const spot = f.spots.at(-1);
      if (spot && t !== null && spot.t !== null && t - spot.t <= SPOT_GAP_SECONDS) {
        spot.lines += times;
        if (!spot.rules.includes(rule.id)) spot.rules.push(rule.id);
      } else {
        f.spots.push({ t, lines: times, rules: [rule.id] });
      }
    }
  };

  for (const line of log) {
    const msg = normalize(line.msg);
    if (!msg) continue;
    const repeated = msg.match(/Last message repeated (\d+) times?/);
    if (repeated) {
      if (previous) count(previous.rule, previous.t, previous.msg, Number(repeated[1]));
      continue;
    }
    const rule = RULES.find((r) => r.re.test(msg)) ?? null;
    count(rule, line.t, msg, 1);
    previous = { rule, t: line.t, msg };
  }

  if (duration && decoded !== null && decoded < duration - Math.max(10, duration * 0.01)) {
    f.truncated = { duration, decoded };
    f.damage += 1;
  }
  if (exitCode !== 0) {
    f.fatal = { exitCode, last: normalize(log.at(-1)?.msg ?? "") };
    f.damage += 1;
  }

  const minor =
    f.damage <= MINOR_LINES &&
    f.spots.length <= MINOR_SPOTS &&
    !f.truncated &&
    !f.fatal &&
    !SEVERE.some((id) => f.spots.some((s) => s.rules.includes(id)));
  f.verdict = f.damage === 0 ? (f.unknown > 0 ? "unclear" : "ok") : minor ? "minor" : "suspect";
  return f;
}

export async function main(log: LogLine[], duration: number | null = null, decoded: number | null = null, exit_code: number = 0) {
  return classify(log, { duration, decoded, exitCode: exit_code });
}

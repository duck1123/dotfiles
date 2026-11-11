# mediacheck: decodes every video file on the NAS in full, at idle priority,
# and reports ffmpeg's error output to the media integrity queue in Windmill
# (f/mediacheck/checker), which sorts it into ok / suspect / unclear.
#
# The queue lives in the cluster; this side only scans, decodes and reports.
# Stopping the service mid-file is fine: the next start hands the claim back.
#
# Settings (environment):
#   MEDIACHECK_URL         run_wait_result URL of f/mediacheck/checker
#   MEDIACHECK_TOKEN_FILE  Windmill token that can run only that script
#   MEDIACHECK_LIBRARIES   "movies=/mnt/movies,tv=/mnt/tv"
#   MEDIACHECK_SCAN_HOURS  hours between library scans (default 6)

const VIDEO_EXTS = [mkv mp4 m4v avi mpg mpeg ts m2ts mov wmv webm flv vob]
const MAX_LINES = 2000
const SYNC_BATCH = 1000
const IDLE_MINUTES = 10

def state-dir [] {
  let dir = ($env.XDG_STATE_HOME? | default ($env.HOME | path join .local state)) | path join mediacheck
  mkdir $dir
  $dir
}

def libraries [] {
  $env.MEDIACHECK_LIBRARIES? | default "movies=/mnt/movies,tv=/mnt/tv"
  | split row ","
  | each {|entry| $entry | parse "{name}={root}" | first }
}

def host [] { sys host | get hostname }

# Run one checker action in Windmill and return its result.
def api [action: string, args: record] {
  let token = open --raw $env.MEDIACHECK_TOKEN_FILE | str trim
  http post --content-type application/json --max-time 5min --headers [Authorization $"Bearer ($token)"] $env.MEDIACHECK_URL {action: $action, args: $args}
}

# Video files under a library root, with size and mtime (epoch seconds).
# Hidden folders (the quarantine among them) and samples are left out.
def scan-files [root: path] {
  ^find $root -type f -not -path '*/.*' -printf '%s\t%T@\t%P\n'
  | lines
  | parse "{size}\t{mtime}\t{path}"
  | where {|f| ($f.path | path parse | get extension | str lowercase) in $VIDEO_EXTS }
  | where path !~ '(?i)(^|[/._ -])sample([/._ -]|$)'
  | each {|f| {path: $f.path, size: ($f.size | into int), mtime: ($f.mtime | into float | math floor)} }
}

# Decode a file in full and collect what ffmpeg says about it.
def decode [file: path] {
  let probe = ^ffprobe -v error -show_entries format=duration -of csv=p=0 $file | complete
  let duration = try { $probe.stdout | str trim | into float } catch { null }
  let started = date now

  # -progress on stderr interleaves out_time_us with the error lines, so each
  # line gets the position it was printed at. ffmpeg's exit code comes last,
  # through sh, since nushell drops either it or the output otherwise.
  # 0:V leaves out cover art: an image that won't decode stalls the muxer, which
  # then buffers every decoded frame of the film until memory runs out.
  let state = ^sh -c 'ffmpeg "$@"; echo "mediacheck-exit=$?" >&2' sh -hide_banner -nostdin -nostats -v error -progress pipe:2 -stats_period 5 -i $file -map '0:V?' -map '0:a?' -f null - e>| lines
  | reduce --fold {t: null, log: [], count: 0, exit: null} {|line, s|
    if ($line | str starts-with "mediacheck-exit=") {
      $s | upsert exit ($line | str replace "mediacheck-exit=" "" | into int)
    } else if ($line | str starts-with "out_time_us=") {
      let us = try { $line | str replace "out_time_us=" "" | into int } catch { null }
      if $us == null { $s } else { $s | upsert t ($us / 1_000_000) }
    } else if ($line =~ '^[a-z0-9_]+=\s*\S*$') {
      $s
    } else if $s.count < $MAX_LINES {
      $s | upsert log ($s.log | append {t: $s.t, msg: $line}) | upsert count ($s.count + 1)
    } else {
      $s | upsert count ($s.count + 1)
    }
  }

  {
    checkSeconds: (((date now) - $started) / 1sec)
    duration: $duration
    decoded: $state.t
    exitCode: ($state.exit | default (-1))
    lineCount: $state.count
    log: $state.log
  }
}

def scan-due [] {
  let stamp = state-dir | path join last-scan
  if not ($stamp | path exists) { return true }
  let hours = $env.MEDIACHECK_SCAN_HOURS? | default "6" | into int
  ((date now) - (ls $stamp | first | get modified)) > ($hours * 1hr)
}

# Scan every library and record the files in the queue.
def "main scan" [] {
  for lib in (libraries) {
    if not ($lib.root | path exists) {
      print -e $"skipping ($lib.name): ($lib.root) isn't there"
      continue
    }
    let scan_started = date now | format date "%+"
    let files = scan-files $lib.root
    let totals = $files | chunks $SYNC_BATCH | each {|batch|
      api sync {library: $lib.name, scanStarted: $scan_started, files: $batch}
    }
    let done = api sync_done {library: $lib.name, scanStarted: $scan_started}
    print $"($lib.name): ($files | length) files, ($totals | get new | math sum) new, ($totals | get queued | math sum) queued, ($done.missing | length) missing"
  }
  touch (state-dir | path join last-scan)
}

# Decode one file and print the result, without reporting it.
def "main check" [file: path] {
  decode $file
}

# Check the next queued file. Returns false when the queue is empty.
def "main once" [] {
  let job = api claim {host: (host)}
  if $job == null { return false }
  let lib = libraries | where name == $job.library | first
  let file = $lib.root | path join $job.path
  print $"checking ($job.library)/($job.path)"
  let result = decode $file
  # ffmpeg wasn't allowed to open the file, so this says nothing about the file.
  # (A file deleted since the scan is still reported; the next scan marks it missing.)
  # In November 2026 the NFS mount refused reads in bursts for a day, and every
  # file checked meanwhile was reported as a crash. Hand the claim back and
  # wait instead; the file is claimed again once reads work.
  if $result.decoded == null and ($result.log | any {|l| $l.msg =~ 'Error opening input.*Permission denied' }) {
    let reason = $result.log | last | get msg
    api release {host: (host)}
    print -e $"  couldn't open it \(($reason)\); released, retrying in ($IDLE_MINUTES) minutes"
    sleep ($IDLE_MINUTES * 1min)
    return true
  }
  let now = ls -l $file | first
  let reported = api report ({id: $job.id, size: ($now.size | into int), mtime: (($now.modified | into int) // 1_000_000_000)} | merge $result)
  print $"  ($reported.status) in ($result.checkSeconds | math round)s: ($result.lineCount) error lines"
  true
}

# Work through the queue, scanning every MEDIACHECK_SCAN_HOURS.
def "main run" [] {
  print $"released (api release {host: (host)} | get released) unfinished claims"
  loop {
    if (scan-due) { main scan }
    if not (main once) { sleep ($IDLE_MINUTES * 1min) }
  }
}

# Queue counts per library and status.
def "main stats" [] {
  api stats {}
}

# Stop checking (systemctl --user stop), e.g. before a game.
def "main pause" [] { ^systemctl --user stop mediacheck.service }
# Start checking again.
def "main resume" [] { ^systemctl --user start mediacheck.service }
# The service's state and latest log lines.
def "main status" [] { ^systemctl --user status mediacheck.service }

def main [] {
  help main
}

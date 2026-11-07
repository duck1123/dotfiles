# Sims 4 save management across several installs ("instances") of the game.
#
# Config: ~/.config/simsnix/config.json, written by the dotfiles `sims4` feature
# (override the path with $env.SIMSNIX_CONFIG):
#   { backupDir, defaultInstance, instances: { <name>: { documents, gameDir } } }
# `documents` is the instance's `Documents/Electronic Arts/The Sims 4` folder,
# `gameDir` its install folder (the one holding `Game/` and `Data/`).
#
# A backup is a folder under backupDir holding:
#   meta.json   instance, slot(s), game versions, what was copied
#   saves/      the slot's Slot_<hex>.save + .ver0-4, mod data in saves/ subfolders
#               whose path names that slot, and mod data that names no slot (global)
#   Tray/       the whole library (households, lots, rooms)
#   settings/   Options.ini, UserSetting.ini, Events.ini
#   Mods/       snapshot; files unchanged since the previous backup are hardlinks to it
#
# Mod data in saves/ subfolders is matched to a slot by name: `slot_<8+ hex digits>`
# is the slot id as in Slot_<hex>.save, `slot_<decimal>` is the same id in decimal.
# Restore and wipe are dry runs unless given --apply.

const settings_files = [Options.ini UserSetting.ini Events.ini]

def sims-config []: nothing -> record {
  let file = $env.SIMSNIX_CONFIG? | default ($env.XDG_CONFIG_HOME? | default ($env.HOME | path join .config) | path join simsnix config.json)
  if not ($file | path exists) {
    error make {msg: $"($file) not found: it is written by the dotfiles `sims4` feature"}
  }
  open $file
}

def backup-root []: nothing -> path {
  sims-config | get backupDir | path expand
}

def "nu-complete sims instances" []: nothing -> list<string> {
  sims-config | get instances | columns
}

def "nu-complete sims backups" []: nothing -> list<string> {
  sims save list | get id
}

# Slots of the instance named by -i/--instance on the command line (else the default)
def "nu-complete sims slots" [context: string]: nothing -> list<string> {
  let words = $context | split row ' '
  let at = $words | enumerate | where item in [-i --instance] | get -o 0.index
  let name = if $at == null { null } else { $words | get -o ($at + 1) }
  sims save slots --instance $name | get slot
}

# Resolve an instance name (default from config) to its record
def instance [name?: string]: nothing -> record {
  let cfg = sims-config
  let name = $name | default $cfg.defaultInstance?
  if $name == null { error make {msg: "no default instance configured: pass --instance"} }
  let inst = $cfg.instances | get -o $name
  if $inst == null {
    error make {msg: $"unknown instance: ($name) \(known: ($cfg.instances | columns | str join ', ')\)"}
  }
  let documents = $inst.documents | path expand
  let game_dir = $inst.gameDir | path expand
  {
    name: $name
    documents: $documents
    saves: ($documents | path join saves)
    gameDir: $game_dir
    gameVersion: (game-version $game_dir)
    lastRunVersion: (last-run-version $documents)
  }
}

# Version of the installed game, from Game/Bin/Default.ini
def game-version [game_dir: path]: nothing -> any {
  let ini = $game_dir | path join Game Bin Default.ini
  if not ($ini | path exists) { return null }
  let found = ^grep -a -i -o -E 'gameversion *= *[0-9.]+' $ini | complete
  if $found.exit_code != 0 { return null }
  $found.stdout | lines | first | str replace -r '.*= *' ''
}

# Version the game last ran as in this user folder (GameVersion.txt has binary junk before it)
def last-run-version [documents: path]: nothing -> any {
  let file = $documents | path join GameVersion.txt
  if not ($file | path exists) { return null }
  let found = ^grep -a -o -E '[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+' $file | complete
  if $found.exit_code != 0 { return null }
  $found.stdout | lines | first
}

# Compare dotted versions: -1, 0 or 1
def version-cmp [a: string, b: string]: nothing -> int {
  let pa = $a | split row . | each { into int }
  let pb = $b | split row . | each { into int }
  for i in 0..<([($pa | length) ($pb | length)] | math max) {
    let x = $pa | get -o $i | default 0
    let y = $pb | get -o $i | default 0
    if $x < $y { return (-1) }
    if $x > $y { return 1 }
  }
  0
}

# A slot id as 8 hex digits, as in Slot_<hex>.save
def hex8 [n: int]: nothing -> string {
  $n | format number | get upperhex | str replace '0x' '' | fill -a right -c '0' -w 8
}

# Normalise a slot id given by the user ("8", "00000008", "1111") to 8 hex digits
def slot-hex [slot: string]: nothing -> string {
  let s = $slot | str trim | str replace -r '(?i)^slot_' '' | str uppercase
  if not ($s =~ '^[0-9A-F]{1,16}$') {
    error make {msg: $"not a slot id: ($slot) \(use the id from `sims save slots`\)"}
  }
  $s | fill -a right -c '0' -w 8
}

# The slot a path under saves/ belongs to (an int), or null if it names none
def path-slot [rel: string]: nothing -> any {
  let hex = $rel | parse --regex '(?i)slot_(?<id>[0-9a-f]{8,16})(?:[^0-9a-z]|$)'
  if ($hex | is-not-empty) { return ($hex.0.id | into int --radix 16) }
  let dec = $rel | parse --regex '(?i)slot_(?<id>\d+)(?:\D|$)'
  if ($dec | is-not-empty) { return ($dec.0.id | into int) }
  null
}

# Every file under saves/, relative, classified by slot
def saves-files [saves: path]: nothing -> table {
  if not ($saves | path exists) { return [] }
  cd $saves
  glob --no-dir '**/*' | each {|f|
    let rel = $f | path relative-to $saves
    let top = ($rel | path split | length) == 1
    let slot_file = $rel | parse --regex '^Slot_(?<id>[0-9A-Fa-f]{8,16})\.save(?:\.ver\d)?$'
    if $top and ($slot_file | is-not-empty) {
      {path: $rel, kind: save, slot: ($slot_file.0.id | into int --radix 16)}
    } else {
      let s = path-slot $rel
      if $s == null { {path: $rel, kind: global, slot: null} } else { {path: $rel, kind: modslot, slot: $s} }
    }
  }
}

# Remove files (rows from saves-files) and the folders that leaves empty
def remove-saves [saves: path, files: table] {
  for f in $files { rm ($saves | path join $f.path) }
  for d in (glob --no-file $"($saves)/**/*" | sort-by {|d| $d | str length } --reverse) {
    if (ls -a $d | is-empty) { rm $d }
  }
}

def assert-game-closed [] {
  let running = ps | where name =~ '(?i)^ts4_'
  if ($running | is-not-empty) {
    error make {msg: $"The Sims 4 is running \(($running.name | uniq | str join ', ')\); close it first"}
  }
}

def rsync-files [src: path, dst: path, files: list<string>] {
  if ($files | is-empty) { return }
  mkdir $dst
  let list = mktemp -t simsnix-files.XXXXXX
  $files | str join "\n" | save -f $list
  # --ignore-times: a restored file must replace the target's even if size and mtime match
  ^rsync -a --ignore-times $"--files-from=($list)" $"($src)/" $"($dst)/"
  rm $list
}

# List the configured instances and their game versions
export def "sims instances" []: nothing -> table {
  let cfg = sims-config
  $cfg.instances | columns | each {|n|
    let i = instance $n
    {
      name: $n
      default: ($n == $cfg.defaultInstance)
      gameVersion: $i.gameVersion
      lastRunVersion: $i.lastRunVersion
      slots: (saves-files $i.saves | where kind == save | get slot | uniq | length)
      documents: $i.documents
    }
  }
}

# List an instance's save slots, newest first
export def "sims save slots" [
  --instance (-i): string@"nu-complete sims instances" # instance (default from config)
]: nothing -> table {
  let i = instance $instance
  let files = saves-files $i.saves
  $files | where kind == save | where path =~ '\.save$' | each {|f|
    let st = ls -l ($i.saves | path join $f.path) | first
    let slot_bytes = $files | where slot == $f.slot | each {|g| ls ($i.saves | path join $g.path) | get size.0 } | math sum
    {
      slot: ($f.path | parse --regex '^Slot_(?<id>[0-9A-Fa-f]+)\.save$' | get id.0)
      modified: $st.modified
      size: $slot_bytes
      modFiles: ($files | where kind == modslot and slot == $f.slot | length)
    }
  } | sort-by modified --reverse
}

# Back up one save slot (default: the newest) with its mod data, Tray, settings and a Mods snapshot
export def "sims save backup" [
  slot?: string                                         # slot id from `sims save slots` (default: newest)
  --instance (-i): string@"nu-complete sims instances" # instance (default from config)
  --all (-a)                                            # every slot instead of one
  --label (-l): string                                  # short note, added to the backup's name
  --no-mods                                             # skip the Mods snapshot
]: nothing -> record {
  assert-game-closed
  let i = instance $instance
  let files = saves-files $i.saves
  let slots = if $all {
    $files | where kind == save | get slot | uniq
  } else {
    let hex = if $slot == null {
      let newest = sims save slots --instance $i.name
      if ($newest | is-empty) { error make {msg: $"no saves in ($i.saves)"} }
      $newest.0.slot
    } else { slot-hex $slot }
    let id = $hex | into int --radix 16
    if ($files | where kind == save and slot == $id | is-empty) {
      error make {msg: $"no slot ($hex) in instance ($i.name)"}
    }
    [$id]
  }
  let picked = if $all { $files } else { $files | where {|f| $f.kind == global or ($f.slot in $slots) } }

  let root = backup-root
  let stamp = date now | format date '%Y-%m-%dT%H%M%S'
  let what = if $all { 'all' } else { $"slot(hex8 $slots.0)" }
  let suffix = if $label == null { '' } else { '-' + ($label | str replace -a -r '[^A-Za-z0-9._-]+' '_') }
  let id = $"($stamp)-($i.name)-($what)($suffix)"
  let dest = $root | path join $id
  let previous = sims save list | where {|b| $b.mods } | get -o 0.id
  mkdir $dest

  rsync-files $i.saves ($dest | path join saves) ($picked | get path)
  let tray = $i.documents | path join Tray
  if ($tray | path exists) { ^rsync -a $"($tray)/" $"($dest | path join Tray)/" }
  let settings = $settings_files | where {|f| $i.documents | path join $f | path exists }
  rsync-files $i.documents ($dest | path join settings) $settings

  let mods = $i.documents | path join Mods
  let with_mods = (not $no_mods) and ($mods | path exists)
  if $with_mods {
    let link = if $previous == null { [] } else { [$"--link-dest=($root | path join $previous Mods)"] }
    ^rsync -a ...$link $"($mods)/" $"($dest | path join Mods)/"
  }

  let meta = {
    id: $id
    created: (date now | format date '%+')
    instance: $i.name
    slots: ($slots | each {|s| hex8 $s })
    all: $all
    label: $label
    gameVersion: $i.gameVersion
    lastRunVersion: $i.lastRunVersion
    saveFiles: ($picked | where kind == save | length)
    modSlotFiles: ($picked | where kind == modslot | length)
    globalFiles: ($picked | where kind == global | length)
    settings: $settings
    mods: $with_mods
  }
  $meta | to json | save ($dest | path join meta.json)
  $meta | insert size (du $dest | get physical.0)
}

# List backups, newest first
export def "sims save list" [
  --instance (-i): string@"nu-complete sims instances" # only backups taken from this instance
]: nothing -> table {
  let root = backup-root
  if not ($root | path exists) { return [] }
  let all = ls $root | where type == dir | each {|d|
    let m = $d.name | path join meta.json
    if ($m | path exists) { open $m } else { null }
  } | compact | sort-by id --reverse
  let all = if $instance == null { $all } else { $all | where instance == $instance }
  $all | select id instance slots gameVersion label mods
}

# Show one backup's metadata and size
export def "sims save show" [
  id: string@"nu-complete sims backups" # backup id from `sims save list`
]: nothing -> record {
  let dir = backup-root | path join $id
  if not ($dir | path join meta.json | path exists) { error make {msg: $"no backup ($id)"} }
  open ($dir | path join meta.json) | insert size (du $dir | get physical.0) | insert path $dir
}

# Restore a backup into an instance (the one it came from by default). Dry run unless --apply.
#
# Puts back the backup's slot(s) and their mod data, and merges its Tray in. Before
# writing, the slots it replaces in the target are backed up (label `pre-restore`).
# Refuses a save from a newer game version than the target's unless --force.
export def "sims save restore" [
  id: string@"nu-complete sims backups"           # backup id from `sims save list`
  --to (-t): string@"nu-complete sims instances" # target instance (default: the one it came from)
  --replace (-r)                                  # wipe the target's saves first (see `sims save wipe`)
  --with-global (-g)                              # also restore mod data that names no slot (mod settings)
  --with-mods (-m)                                # also replace the target's Mods/ with the backup's snapshot
  --with-settings (-s)                            # also restore Options.ini etc.
  --force (-f)                                    # restore even into an older game version
  --apply                                         # actually do it
]: nothing -> record {
  let dir = backup-root | path join $id
  let meta = sims save show $id
  let t = instance ($to | default $meta.instance)

  if ($meta.gameVersion != null) and ($t.gameVersion != null) and ((version-cmp $meta.gameVersion $t.gameVersion) > 0) and (not $force) {
    error make {msg: $"backup is from game ($meta.gameVersion), ($t.name) is ($t.gameVersion): an older game can't load newer saves \(--force to try anyway\)"}
  }
  if $with_mods and (not $meta.mods) { error make {msg: $"backup ($id) has no Mods snapshot"} }

  let incoming = saves-files ($dir | path join saves)
  let restore = $incoming | where {|f| $f.kind != global or $with_global or $replace }
  let slots = $incoming | where kind == save | get slot | uniq
  let existing = saves-files $t.saves
  # Files the restored slots replace in the target (their old mod data included)
  let replaced = $existing | where {|f| $f.slot != null and ($f.slot in $slots) }

  let mods_changes = if $meta.mods and ($t.documents | path join Mods | path exists) {
    ^rsync -an --delete --itemize-changes $"($dir | path join Mods)/" $"($t.documents | path join Mods)/" | lines | where $it != '' | length
  } else { null }

  let plan = {
    backup: $id
    to: $t.name
    slots: $meta.slots
    gameVersions: $"($meta.gameVersion) -> ($t.gameVersion)"
    wipeFirst: $replace
    removeFiles: (if $replace { $existing | where {|f| $f.kind != global } | length } else { $replaced | length })
    copyFiles: ($restore | length)
    globalModData: ($with_global or $replace)
    tray: 'merge'
    mods: (if $with_mods { 'replace' } else { 'keep' })
    modsDiffer: $mods_changes
    settings: (if $with_settings { $meta.settings } else { [] })
    applied: false
  }
  if not $apply {
    if (not $with_mods) and ($mods_changes | default 0) > 0 {
      print $"warning: ($mods_changes) files differ between the backup's Mods snapshot and ($t.name)'s Mods/ \(--with-mods to restore them\)"
    }
    print 'dry run: add --apply to restore'
    return $plan
  }

  assert-game-closed
  if $replace {
    sims save wipe --instance $t.name --label pre-restore --apply | ignore
  } else if ($replaced | is-not-empty) {
    for s in ($existing | where kind == save and ($it.slot in $slots) | get slot | uniq) {
      sims save backup (hex8 $s) --instance $t.name --label pre-restore --no-mods=(not $with_mods) | ignore
    }
    for f in $replaced { rm ($t.saves | path join $f.path) }
  }

  rsync-files ($dir | path join saves) $t.saves ($restore | get path)
  if ($dir | path join Tray | path exists) { ^rsync -a --ignore-times $"($dir | path join Tray)/" $"($t.documents | path join Tray)/" }
  if $with_settings { rsync-files ($dir | path join settings) $t.documents $meta.settings }
  if $with_mods { ^rsync -a --delete $"($dir | path join Mods)/" $"($t.documents | path join Mods)/" }
  # Thumbnails cached for the old saves and mods
  let thumbs = $t.documents | path join localthumbcache.package
  if ($thumbs | path exists) { rm $thumbs }

  $plan | update applied true
}

# Back up every slot, then empty an instance's saves so the game starts fresh. Dry run unless --apply.
#
# Keeps mod data that names no slot (mod settings) unless --everything; Tray and Mods are kept.
export def "sims save wipe" [
  --instance (-i): string@"nu-complete sims instances" # instance (default from config)
  --everything (-e)                                     # also remove mod data that names no slot
  --label (-l): string = 'pre-wipe'                     # label for the safety backup
  --apply                                               # actually do it
]: nothing -> record {
  let i = instance $instance
  let files = saves-files $i.saves
  let remove = $files | where {|f| $everything or $f.kind != global }
  let plan = {
    instance: $i.name
    slots: ($files | where kind == save | get slot | uniq | length)
    removeFiles: ($remove | length)
    keepFiles: (($files | length) - ($remove | length))
    backup: null
    applied: false
  }
  if not $apply {
    print 'dry run: add --apply to back up and wipe'
    return $plan
  }
  assert-game-closed
  if ($files | is-empty) { return ($plan | update applied true) }
  let b = sims save backup --instance $i.name --all --label $label
  remove-saves $i.saves $remove
  let thumbs = $i.documents | path join localthumbcache.package
  if ($thumbs | path exists) { rm $thumbs }
  $plan | update backup $b.id | update applied true
}

# Back up a save slot, then delete it and its mod data. Dry run unless --apply.
export def "sims save delete" [
  slot: string@"nu-complete sims slots"                 # slot id from `sims save slots`
  --instance (-i): string@"nu-complete sims instances" # instance (default from config)
  --apply                                               # actually do it
]: nothing -> record {
  let i = instance $instance
  let hex = slot-hex $slot
  let id = $hex | into int --radix 16
  let remove = saves-files $i.saves | where slot == $id
  if ($remove | where kind == save | is-empty) {
    error make {msg: $"no slot ($hex) in instance ($i.name)"}
  }
  let plan = {
    instance: $i.name
    slot: $hex
    saveFiles: ($remove | where kind == save | length)
    modFiles: ($remove | where kind == modslot | length)
    backup: null
    applied: false
  }
  if not $apply {
    print 'dry run: add --apply to back up and delete'
    return $plan
  }
  assert-game-closed
  let b = sims save backup $hex --instance $i.name --label pre-delete
  remove-saves $i.saves $remove
  $plan | update backup $b.id | update applied true
}

# Delete backups by id. Dry run unless --apply.
export def "sims save delete-backup" [
  ...ids: string@"nu-complete sims backups" # backup ids from `sims save list`
  --apply                                    # actually do it
]: nothing -> table {
  if ($ids | is-empty) { error make {msg: "give at least one backup id"} }
  let root = backup-root
  for id in $ids {
    if ($id =~ '[/\\]' or $id in ['.' '..'] or not ($root | path join $id meta.json | path exists)) {
      error make {msg: $"no backup ($id)"}
    }
  }
  let found = $ids | each {|id|
    let dir = $root | path join $id
    # Includes Mods files hardlinked into other backups, so deleting may free less
    {id: $id, size: (du $dir | get physical.0), deleted: false}
  }
  if not $apply {
    print 'dry run: add --apply to delete'
    return $found
  }
  for b in $found { rm -r ($root | path join $b.id) }
  $found | update deleted true
}

# Mods (simsnix). The catalog and profiles are evaluated by the framework in
# dotfiles' modules/sims4/_simsnix with `nix eval --impure`; the catalog is read
# in place from dataDir, so nothing from it is copied to /nix/store.

# Evaluate a profile into the manifest `sims switch` will install
export def "sims manifest" [
  profile?: string # profile to evaluate (default: "default")
]: nothing -> record {
  let cfg = sims-config
  let data = $cfg.dataDir | path expand
  if not ($data | path exists) {
    error make {msg: $"($data) not found: clone the private repo there, or set the sims4 feature's dataDir"}
  }
  let sources_file = $data | path join sources.json
  if ($sources_file | path exists) and (open $sources_file | is-not-empty) {
    error make {msg: $"($sources_file) lists sources, which aren't supported yet"}
  }
  let args = {
    framework: $cfg.framework
    nixpkgsLib: $cfg.nixpkgsLib
    sources: [$data]
    profile: ($profile | default default)
  }
  let expr = '
    let a = builtins.fromJSON (builtins.getEnv "SIMSNIX_ARGS"); in
    import (/. + a.framework + "/eval.nix") {
      lib = import (/. + a.nixpkgsLib);
      sources = map (s: /. + s) a.sources;
      inherit (a) profile;
    }'
  let r = with-env {SIMSNIX_ARGS: ($args | to json -r)} { ^nix eval --impure --json --expr $expr | complete }
  if $r.exit_code != 0 {
    # the framework's own errors read better without nix's trace
    let own = $r.stderr | lines | skip until {|l| $l =~ 'error: simsnix: ' } | str trim | str replace -r '^error: ' ''
    error make {msg: (if ($own | is-empty) { $r.stderr } else { $own | str join "\n" })}
  }
  $r.stdout | from json
}

# Importing a folder of mods. Every file is copied into the blob store (the folder
# itself is only read: blobs are chmod 444, so they must not share an inode with it).
# What the files are goes in the catalog, mods/<creator>/<mod>.json, and in
# import/<name>.json, a record of every file in the folder.
#
# A mod is a top-level folder, or the loose files sharing a name prefix. Mods the
# catalog already has get a new version if the files differ. import/grouping.json
# (hand-edited, optional) overrides the guesses:
#   groups:    { "<folder or loose file, relative to Mods/>": { mod: "creator/mod", version?: "label" } }
#              a nested folder here becomes a mod of its own
#   rules:     [{ match: regex, mod: "creator/mod", version?: "label" }]  the first match wins;
#              all a mod's matches are one mod, installed at the paths they have now
#   state:     [regex]  paths the mods write
#   files:     [regex]  paths that are part of a mod, whatever their extension
#   unmanaged: [regex]  paths that belong to no mod

const state_exts = [log dat ini cfg html htm json simshare]
const archive_exts = [7z rar zip]
const game_files = [resource.cfg]
const version_res = [
  '(?i)(?:^|[ _-])v(?:er(?:sion)?)?[ ._-]*(?<v>\d+(?:[._]\d+)*[a-z]?)(?=$|[ _-])'
  '(?:^|[ _-])(?<v>\d{4}[_.-]\d{1,2}[_.-]\d{1,2})(?=$|[ _-])'
  '(?i)(?:^|[ _-])(?<v>(?:jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*[ _-]?\d{4})(?=$|[ _-])'
  '(?:^|[ _-])(?<v>\d+(?:\.\d+)+[a-z]?)(?=$|[ _-])'
]

def slug []: string -> string {
  str lowercase | str replace -a -r '[^a-z0-9]+' '-' | str trim -c '-'
}

# A folder or file name as its name without the version, and that version (or null)
def split-version [name: string]: nothing -> record {
  for re in $version_res {
    let found = $name | parse --regex $re
    if ($found | is-not-empty) {
      return {stem: ($name | str replace -r $re ''), version: ($found.0.v | str replace -a -r '[_ ]' '.')}
    }
  }
  {stem: $name, version: null}
}

def without-ext [name: string]: nothing -> string {
  let ext = $name | path parse | get extension
  if $ext in [package ts4script ...$archive_exts] { $name | path parse | get stem } else { $name }
}

def first-token [name: string]: nothing -> string {
  $name | str replace -a -r '[^A-Za-z0-9]+' ' ' | str trim | split row ' ' | first
}

# Every file under a folder: path (relative), size, date (modified), sha256
def hash-tree [dir: path]: nothing -> table {
  let listing = ^find $dir -type f -printf "%s\t%TY-%Tm-%Td\t%P\\0" | complete
  if $listing.exit_code != 0 { error make {msg: $"can't read ($dir): ($listing.stderr)"} }
  $listing.stdout | split row (char nul) | where $it != '' | par-each {|e|
    let parts = $e | split row (char tab)
    let rel = $parts | skip 2 | str join (char tab)
    let sum = ^sha256sum -z -- $"($dir)/($rel)" | complete
    if $sum.exit_code != 0 { error make {msg: $"can't hash ($rel): ($sum.stderr)"} }
    {path: $rel, size: ($parts.0 | into int), date: $parts.1, sha256: ($sum.stdout | str substring 0..<64)}
  } | sort-by path
}

def total-size []: table -> filesize {
  reduce -f 0b {|f, acc| $acc + ($f.size | into int | $in * 1b) }
}

def blob-root []: nothing -> path {
  sims-config | get blobDir | path expand | path join sha256
}

# Copy a file into the blob store, checking it still has the hash we recorded
def store-blob [root: path, src: path, sha256: string] {
  let dst = $root | path join $sha256
  if ($dst | path exists) { return }
  let tmp = $"($dst).tmp"
  ^cp --preserve=timestamps -- $src $tmp
  let got = ^sha256sum -z -- $tmp | complete | get stdout | str substring 0..<64
  if $got != $sha256 {
    rm $tmp
    error make {msg: $"($src) changed during the import"}
  }
  ^chmod 444 -- $tmp
  ^mv -- $tmp $dst
}

def file-kind [rel: string, grouping: record]: nothing -> string {
  let matches = {|res| $res | any {|re| $rel =~ $re } }
  let ext = $rel | path parse | get extension | str lowercase
  if (do $matches $grouping.unmanaged) { 'unmanaged'
  } else if (do $matches $grouping.state) { 'state'
  } else if (do $matches $grouping.files) { 'file'
  } else if ($rel | path basename | str lowercase) in $game_files { 'unmanaged'
  } else if $ext in $state_exts { 'state'
  } else { 'file' }
}

# The import/<name>.json records (not grouping.json or grouping.suggested.json)
def import-reports [data: path]: nothing -> list<path> {
  let dir = $data | path join import
  if not ($dir | path exists) { return [] }
  ls $dir | where {|f|
    let base = $f.name | path basename
    ($base | str ends-with .json) and not ($base | str starts-with grouping)
  } | get name
}

def read-grouping [data: path]: nothing -> record {
  let file = $data | path join import grouping.json
  let found = if ($file | path exists) { open $file } else { {} }
  {groups: {}, rules: [], state: [], files: [], unmanaged: []} | merge $found
}

# The catalog as written: [{ id, path, mod }]
def read-catalog [data: path]: nothing -> table {
  let dir = $data | path join mods
  if not ($dir | path exists) { return [] }
  ls $dir | where type == dir | each {|c|
    ls $c.name | where {|f| $f.name | str ends-with .json } | each {|f|
      let creator = $c.name | path basename
      let mod = $f.name | path parse | get stem
      {id: $"($creator)/($mod)", path: $f.name, mod: (open $f.name)}
    }
  } | flatten
}

# Download pages from the spreadsheets (mods*.xlsx, title and Site columns), by slug of
# the top-level folder or loose file with its version removed
def read-pages [data: path]: nothing -> record {
  let sheets = ls $data | where {|f| ($f.name | path basename) =~ '^mods.*\.xlsx$' } | sort-by name --reverse
  let rows = $sheets | each {|f|
    open $f.name | values | first | where Site != null | select title Site
  } | flatten
  $rows | each {|r|
    let top = $r.title | path split | first
    {key: (split-version (without-ext $top) | get stem | slug), page: $r.Site}
  } | uniq-by key | reduce -f {} {|r, acc| $acc | merge {($r.key): $r.page} }
}

# Which group each file is in: the longest matching key of `groups`, else the first
# matching rule, else its top-level folder, else its loose-file prefix (when several
# loose files share one)
def assign-groups [files: table, grouping: record]: nothing -> table {
  let keys = $grouping.groups | columns | sort-by {|k| $k | str length } --reverse
  let tokens = $files | where {|f| not ($f.path | str contains '/') } | each {|f| first-token $f.path | str lowercase }
  let shared = $tokens | uniq --repeated
  $files | each {|f|
    let key = $keys | where {|k| $f.path == $k or ($f.path | str starts-with $"($k)/") } | get -o 0
    let rule = if $key == null { $grouping.rules | where {|r| $f.path =~ $r.match } | get -o 0 }
    let is_folder = ($f.path | str contains '/') and $rule == null
    let group = if $key != null { $key
    } else if $rule != null { $rule.mod
    } else if $is_folder { $f.path | path split | first
    } else if ((first-token $f.path | str lowercase) in $shared) { first-token $f.path
    } else { $f.path }
    let folder = if $key != null {
      if $f.path == $key { null } else { $key }
    } else if $is_folder { $group } else { null }
    let kind = file-kind $f.path $grouping
    let name = if $folder == null { $f.path } else { $f.path | str substring (($folder | str length -g) + 1).. -g }
    $f | insert group $group | insert folder $folder | insert name $name | insert kind (
      if $folder == null and $kind == state { 'unmanaged' } else { $kind }
    )
  }
}

# The known mod a group is another copy of: the same name, the same name without its
# version, or mostly the same .package/.ts4script names as its newest files (at least
# half of each side's, so part of a big catch-all folder doesn't count as all of it)
def match-known [group: string, files: table, known: table]: nothing -> any {
  let exact = $known | where title == $group | get -o 0.id
  if $exact != null { return $exact }
  let stem = split-version (without-ext $group) | get stem | slug
  let by_name = $known | where {|k| $k.stem == $stem } | get -o 0.id
  if $by_name != null { return $by_name }
  let names = $files | where {|f| ($f.name | path parse | get extension | str lowercase) in [package ts4script] } | get name | each { path basename }
  if ($names | is-empty) { return null }
  let scored = $known | each {|k|
    {id: $k.id, n: ($names | where $it in $k.names | length), size: ($k.names | length)}
  } | where n > 0
  if ($scored | is-empty) { return null }
  let best = $scored | sort-by n --reverse | first
  if $best.n * 2 >= ($names | length) and $best.n * 2 >= $best.size { $best.id } else { null }
}

def guess-id [group: string, tokens: list<string>]: nothing -> string {
  let bare = split-version (without-ext $group) | get stem
  let token = first-token $bare
  let rest = $bare | str replace $token '' | slug
  let shared = ($tokens | where $it == ($token | str lowercase) | length) > 1
  if $shared {
    $"($token | slug)/($rest | default -e ($token | slug))"
  } else {
    $"unknown/($bare | slug | default -e mod)"
  }
}

def state-glob [name: string]: nothing -> string {
  let p = $name | path parse
  if ($p.extension | str lowercase) == log {
    if $p.parent == '' { '*.log' } else { $"($p.parent)/*.log" }
  } else { $name }
}

# The plan for one group: which mod, and what changes in its JSON
def plan-group [g: record, label: string, is_live: bool, name: string]: nothing -> record {
  let files = $g.items | where kind == file | sort-by name | select name sha256
  let state = $g.items | where kind == state
  let mod = $g.existing | default {}
  let versions = $mod.versions? | default {}
  let same = $versions | transpose label v | where {|r|
    ($r.v.files | each {|f| {name: $f.name, sha256: $f.sha256} } | sort-by name) == $files
  } | get -o 0.label
  let wanted = $g.version | default $label
  let new_label = if $same != null { $same
  } else if ($wanted in ($versions | columns)) { $"($wanted)+($name)"
  } else { $wanted }
  let archives = $files | where {|f| ($f.name | path parse | get extension | str lowercase) in $archive_exts }
  let scripts = $files | where {|f| ($f.name | str lowercase | str ends-with .ts4script) }
  let installs = ($files | length) > ($archives | length)
  let new_versions = if ($same != null) or ($files | is-empty) { $versions } else {
    $versions | merge {($new_label): {added: (date now | format date '%Y-%m-%d'), files: $files}}
  }
  let merged = $mod | merge {
    title: ($mod.title? | default $g.group)
    downloadPage: ($mod.downloadPage? | default $g.page)
    tags: ($mod.tags? | default [] | append (if ($scripts | is-empty) { [] } else { [script] }) | uniq | sort)
    folder: (if $mod == {} { $g.folder } else { $mod.folder })
    include: ['.*']
    exclude: (if ($archives | is-empty) { $mod.exclude? | default [] } else { ['.*\.(7z|rar|zip)'] })
    stateFiles: ($mod.stateFiles? | default [] | append ($state | get name | each {|n| state-glob $n }) | uniq | sort)
    versions: $new_versions
  }
  let final = if $is_live and $installs { $merged | merge {version: $new_label} } else { $merged }
  {
    id: $g.id
    json: $final
    inProfile: ($is_live and $installs)
    change: (if $mod == {} { 'new mod' } else if $same == null and ($files | is-not-empty) { 'new version' } else { 'same' })
    version: $new_label
    group: $g.group
    folder: $g.folder
    files: ($files | length)
    state: ($state | length)
    page: $final.downloadPage
  }
}

# Import a folder of mods into the blob store and the catalog. Dry run unless --apply.
#
# --profile marks this as the set in use: its mods are pinned to these versions and
# listed in that profile. Without it the folder is history: older versions of known
# mods, and unknown mods in no profile.
#
# --fresh starts the catalog over, after grouping.json changes: it ignores the mods
# already in it and, with --apply, deletes them and every import/<name>.json first.
# Re-run the other imports (without --fresh) after it. The blob store is kept.
export def "sims import" [
  dir: path                # folder to import; only read
  --name (-n): string      # name of this import, for import/<name>.json
  --profile (-p): string   # write this profile from the folder
  --fresh                  # start from an empty catalog
  --apply                  # write the blobs and the catalog
]: nothing -> record {
  if $name == null { error make {msg: "give the import a --name"} }
  let dir = $dir | path expand
  let data = sims-config | get dataDir | path expand
  let grouping = read-grouping $data
  let is_live = $profile != null
  let scanned = hash-tree $dir
  let today = date now | format date '%Y-%m-%d'
  let files = assign-groups $scanned $grouping
  let old_catalog = read-catalog $data
  let catalog = if $fresh { [] } else { $old_catalog }
  let known = $catalog | each {|c|
    let label = $c.mod.version? | default ($c.mod.versions | columns | sort | last)
    {
      id: $c.id
      title: ($c.mod.title? | default $c.id)
      stem: (split-version (without-ext ($c.mod.title? | default $c.id)) | get stem | slug)
      names: ($c.mod.versions | get -o $label | get -o files | default [] | get name | each { path basename })
    }
  }
  let pages = read-pages $data
  let tokens = $files | get group | uniq | each {|g| first-token (split-version (without-ext $g) | get stem) | str lowercase }

  let groups = $files | group-by group | transpose group items
  mut taken = $catalog | get -o id | default []
  mut plans = []
  for g in $groups {
    if ($g.items | where kind == file | is-empty) { continue }
    let override = $grouping.groups | transpose k v | where k == $g.group | get -o 0.v | default ($grouping.rules | where mod == $g.group | get -o 0)
    let matched = if $override != null { $override.mod } else {
      match-known $g.group ($g.items | where kind == file) $known
    }
    let date = $g.items | get date | sort | last
    let split = split-version (without-ext $g.group)
    let id = if $matched != null { $matched } else {
      let guess = guess-id $g.group $tokens
      if $guess in $taken { $"($guess)-($taken | where $it == $guess | length | $in + 2)" } else { $guess }
    }
    $taken = $taken | append $id
    let earlier = $plans | where id == $id
    let existing = if ($earlier | is-empty) { $catalog | where id == $id | get -o 0.mod } else { $earlier | last | get json }
    let plan = plan-group {
      group: $g.group
      items: $g.items
      id: $id
      existing: $existing
      folder: $g.items.0.folder
      version: ($override.version? | default $split.version)
      page: ($pages | transpose k v | where k == ($split.stem | slug) | get -o 0.v)
    } $date $is_live $name
    $plans = $plans | append $plan
  }
  let plans = $plans

  let owner = $files | each {|f|
    let group = $f.group
    let mod = if $f.kind == unmanaged { null } else { $plans | where group == $group | get -o 0.id }
    # a folder with nothing but state files has no mod to own them
    $f | update kind (if $mod == null { 'unmanaged' } else { $f.kind }) | insert mod $mod
  }
  let root = blob-root
  let new_blobs = $scanned | uniq-by sha256 | where {|f| not ($root | path join $f.sha256 | path exists) }
  let summary = {
    name: $name
    dir: $dir
    files: ($scanned | length)
    size: ($scanned | total-size)
    distinctHashes: ($scanned | get sha256 | uniq | length)
    newBlobs: ($new_blobs | length)
    newBlobSize: ($new_blobs | total-size)
    modFiles: ($owner | where kind == file | length)
    stateFiles: ($owner | where kind == state | length)
    unmanagedFiles: ($owner | where kind == unmanaged | get path)
    changes: ($plans | group-by change | transpose change mods | each {|c| {change: $c.change, mods: ($c.mods | length)} })
    mods: ($plans | reject json inProfile | sort-by id)
    sharedHashes: ($owner | where kind == file | group-by sha256 | values | where {|v| ($v | get mod | uniq | length) > 1 } | each {|v| $v | get path })
    applied: false
  }
  let old_reports = import-reports $data
  let summary = if $fresh {
    $summary | insert removes {mods: ($old_catalog | length), reports: ($old_reports | each { path basename })}
  } else { $summary }
  if not $apply {
    print 'dry run: add --apply to write the blobs and the catalog'
    return $summary
  }

  if $fresh {
    for c in $old_catalog { rm $c.path }
    for d in (ls ($data | path join mods) | where type == dir | get name) {
      if (ls -a $d | is-empty) { rm $d }
    }
    for r in $old_reports { rm $r }
  }
  mkdir $root
  $new_blobs | par-each {|f| store-blob $root $"($dir)/($f.path)" $f.sha256 }
  # several groups can be versions of one mod: the last plan has them all
  for p in ($plans | reverse | uniq-by id) {
    let file = $data | path join mods $"($p.id).json"
    mkdir ($file | path dirname)
    $p.json | to json -i 2 | save -f $file
  }
  mkdir ($data | path join import)
  {
    name: $name
    dir: $dir
    date: $today
    files: ($owner | select path size sha256 kind mod)
  } | to json -i 2 | save -f ($data | path join import $"($name).json")
  if $is_live {
    mkdir ($data | path join profiles)
    {mods: ($plans | where inProfile | get id | uniq | sort)} | to json -i 2 | save -f ($data | path join profiles $"($profile).json")
  }
  $summary | update applied true
}

# Compare an instance's Mods/ with a profile, and the catalog with the blob store
export def "sims status" [
  --instance (-i): string@"nu-complete sims instances" # instance (default from config)
  --profile (-p): string                                # profile (default: the one last switched to, else current)
]: nothing -> record {
  let i = instance $instance
  let mods_dir = $i.documents | path join Mods
  let state_file = $mods_dir | path join .simsnix-state.json
  let active = if ($state_file | path exists) { open $state_file | get -o profile }
  let name = $profile | default $active | default current
  let manifest = sims manifest $name
  let live = hash-tree $mods_dir | where path != .simsnix-state.json
  let wanted = $manifest.files | select mod target sha256
  let joined = $wanted | join --left ($live | select path sha256 | rename target live) target
  let extra = $live | where path not-in $wanted.target
  let state_res = $manifest.mods | each {|m|
    $m.stateFiles | each {|g|
      let rel = if $m.folder == null { $g } else { $"($m.folder)/($g)" }
      '^' + ($rel | str escape-regex | str replace -a '\*' '[^/]*') + '$'
    }
  } | flatten
  let state = $extra | where {|f| $state_res | any {|re| $f.path =~ $re } }
  let unmanaged = $extra | where path not-in $state.path

  let data = sims-config | get dataDir | path expand
  let catalog_hashes = read-catalog $data | each {|c|
    $c.mod.versions | values | each {|v| $v.files | get sha256 } | flatten
  } | flatten | uniq
  let report_hashes = import-reports $data | each {|r| open $r | get files | get sha256 } | flatten | uniq
  # downloads stored by `sims sources index --apply` (not the archives inside them)
  let source_hashes = read-sources $data | where in == null | get -o sha256 | default []
  let root = blob-root
  let blob_list = if ($root | path exists) { ^find $root -type f -printf "%s %f\n" | lines | parse '{size} {sha256}' } else { [] }
  let known = $catalog_hashes | append $report_hashes | append $source_hashes | uniq
  {
    instance: $i.name
    profile: $name
    switched: ($active != null)
    ok: ($joined | where {|f| $f.live == $f.sha256 } | length)
    changed: ($joined | where {|f| $f.live != null and $f.live != $f.sha256 } | get target)
    missing: ($joined | where live == null | get target)
    stateFiles: ($state | length)
    unmanaged: ($unmanaged | get path)
    blobs: ($blob_list | length)
    blobSize: ($blob_list | total-size)
    blobsMissing: ($known | where $it not-in ($blob_list | get -o sha256 | default []))
    blobsUnreferenced: ($blob_list | where sha256 not-in $known | length)
  }
}

# Download sources. The archives and loose mod files that mods were installed from,
# indexed by what's inside them so installed files can be traced back to their download.
# The index is <dataDir>/archives.json:
#   [{ sha256, name, size, paths, in, error?, members: [{ path, size, sha256 }] }]
# `paths` is where the file has been seen. An archive inside another has `in` (the
# outer one's sha256) and is a source of its own.

const mod_exts = [package ts4script]

def read-sources [data: path]: nothing -> table {
  let file = $data | path join archives.json
  if ($file | path exists) { open $file } else { [] }
}

def ext-of [name: string]: nothing -> string {
  $name | path parse | get extension | str lowercase
}

def sha256-of [file: path]: nothing -> string {
  let r = ^sha256sum -z -- $file | complete
  if $r.exit_code != 0 { error make {msg: $"can't hash ($file): ($r.stderr)"} }
  $r.stdout | str substring 0..<64
}

# Index one archive: unpack it into a scratch folder, hash what's in it, then do the
# same for the archives inside it
def index-archive [file: path, sha256: string, name: string, parent: any, tmp: path, depth: int]: nothing -> list<record> {
  let out = mktemp -d -p $tmp
  let r = ^7zz x -y -bso0 -bsp0 -p- $"-o($out)" -- $file | complete
  ^chmod -R u+rwX $out
  let members = if $r.exit_code == 0 { hash-tree $out } else { [] }
  let entry = {
    sha256: $sha256
    name: $name
    size: (ls $file | get size.0 | into int)
    paths: []
    in: $parent
    members: ($members | select -o path size sha256)
  }
  let entry = if $r.exit_code == 0 { $entry } else { $entry | insert error ($r.stderr | str trim) }
  let nested = if $depth >= 3 { [] } else {
    $members | where {|m| (ext-of $m.path) in $archive_exts } | each {|m|
      index-archive ($out | path join $m.path) $m.sha256 ($m.path | path basename) $sha256 $tmp ($depth + 1)
    } | flatten
  }
  rm -rf $out
  [$entry ...$nested]
}

# Index the downloads in a folder: every archive (7z, rar, zip) and loose .package or
# .ts4script file, at any depth. Dry run unless --apply, which unpacks the new ones in
# a scratch folder beside the blob store, adds them to archives.json and copies each
# download into the blob store. Downloads already indexed (by hash) aren't unpacked again.
export def "sims sources index" [
  dir: path                      # folder of downloads; only read
  --skip (-s): list<string> = [] # regexes of paths (relative to dir) to leave out
  --apply                        # unpack, index and store the new downloads
]: nothing -> record {
  let dir = $dir | path expand
  let data = sims-config | get dataDir | path expand
  let index = read-sources $data
  let listing = ^find $dir -type f -printf "%s\t%P\\0" | complete
  if $listing.exit_code != 0 { error make {msg: $"can't read ($dir): ($listing.stderr)"} }
  let found = $listing.stdout | split row (char nul) | where $it != '' | each {|e|
    let parts = $e | split row (char tab)
    {path: ($parts | skip 1 | str join (char tab)), size: ($parts.0 | into int)}
  } | where {|f| (ext-of $f.path) in [...$archive_exts ...$mod_exts] and not ($skip | any {|re| $f.path =~ $re }) }
  let hashed = $found | par-each {|f| $f | insert sha256 (sha256-of ($dir | path join $f.path)) } | sort-by path
  let known = $index | get -o sha256 | default []
  let new = $hashed | where sha256 not-in $known | uniq-by sha256
  let summary = {
    dir: $dir
    downloads: ($hashed | length)
    size: ($hashed | total-size)
    archives: ($hashed | where {|f| (ext-of $f.path) in $archive_exts } | length)
    modFiles: ($hashed | where {|f| (ext-of $f.path) in $mod_exts } | length)
    duplicates: (($hashed | length) - ($hashed | uniq-by sha256 | length))
    new: ($new | length)
    newSize: ($new | total-size)
    applied: false
  }
  if not $apply {
    print 'dry run: add --apply to unpack, index and store the new downloads'
    return $summary
  }

  let root = blob-root
  let tmp = sims-config | get blobDir | path expand | path join tmp
  mkdir $root $tmp
  let added = $new | par-each --threads 4 {|f|
    let file = $dir | path join $f.path
    store-blob $root $file $f.sha256
    let name = $f.path | path basename
    if (ext-of $f.path) in $mod_exts {
      [{sha256: $f.sha256, name: $name, size: $f.size, paths: [], in: null, members: [{path: $name, size: $f.size, sha256: $f.sha256}]}]
    } else {
      index-archive $file $f.sha256 $name null $tmp 0
    }
  } | flatten
  rm -rf $tmp
  # where each download is now, added to where it was seen before
  let home = $env.HOME
  let seen = $hashed | group-by sha256 | transpose sha256 rows | reduce -f {} {|g, acc|
    $acc | insert $g.sha256 ($g.rows | each {|r| $dir | path join $r.path | str replace -r $"^($home | str escape-regex)/" '~/' })
  }
  # a download that is also inside another archive is kept as a download
  let merged = $index | append $added | sort-by {|s| $s.in != null } | uniq-by sha256 | each {|s|
    $s | update paths ($s.paths | append ($seen | get -o $s.sha256 | default []) | uniq | sort)
  } | sort-by name
  $merged | to json -i 2 | save -f ($data | path join archives.json)
  $summary | merge {
    indexed: ($added | length)
    errors: ($added | where {|s| $s.error? != null } | each {|s| {name: $s.name, error: $s.error} })
    applied: true
  }
}

# An id for a download that becomes a mod of its own: the creator of the mods its files
# were in (when they agree), else a guess from its name, then its name without the
# creator, the version or a WW_ prefix
def source-id [name: string, was: list<string>, stems: list<string>]: nothing -> string {
  let creators = $was | each {|m| $m | split row '/' | first } | uniq | where $it != unknown
  if ($creators | length) != 1 { return (guess-id $name $stems) }
  let creator = $creators.0
  let stem = split-version (without-ext $name) | get stem
  let rest = $stem | str replace -a -r '[^A-Za-z0-9]+' ' ' | str trim | split row ' ' | where {|t|
    ($t | str lowercase) not-in [$creator ww]
  } | str join '-' | slug
  $"($creator)/($rest | default -e mod)"
}

# Trace an import's mod files back to the downloads they came from (index them with
# `sims sources index` first). Each .package and .ts4script file is matched by hash.
# When several downloads have it, the one most fully installed wins, then the one with
# the fewest mod files, so a mod's own archive beats an older version or a bundle.
#
# Each download that matched is one row, with `change`:
#   same     it is exactly one mod of the import, all of whose files it has
#   partial  one mod, but only some of its files (often an older version)
#   split    one of several downloads that one mod's files came from
#   merge    it has the files of several mods
# A split or merged download gets a new id: the creator of the mods it came from (when
# they agree) and the download's name. --write saves import/grouping.suggested.json:
# grouping.json plus a rule for each split or merge (ahead of the hand-written rules)
# matching its files, and whole top folders that came from it alone. Hand-written
# `groups` entries that would hide those rules become rules after them, so they still
# take the files the downloads didn't.
export def "sims sources match" [
  --import (-i): string = 'frozen-live' # the import/<name>.json to match
  --write                               # write import/grouping.suggested.json
]: nothing -> record {
  let data = sims-config | get dataDir | path expand
  let sources = read-sources $data
  if ($sources | is-empty) { error make {msg: "no archives.json: run `sims sources index` first"} }
  let report_file = $data | path join import $"($import).json"
  if not ($report_file | path exists) { error make {msg: $"no import ($import) in ($data)/import"} }
  let report = open $report_file
  let live = $report.files | where kind == file | where {|f| (ext-of $f.path) in $mod_exts } | select path sha256 mod
  let live_hashes = $live | get sha256 | uniq
  let names = $sources | reduce -f {} {|s, acc| $acc | upsert $s.sha256 $s.name }
  let members = $sources | each {|s|
    $s.members | where {|m| (ext-of $m.path) in $mod_exts } | get sha256 | uniq | each {|h| {sha256: $h, source: $s.sha256} }
  } | flatten
  let scored = $sources | each {|s|
    let mine = $members | where source == $s.sha256
    let matched = $mine | where sha256 in $live_hashes | length
    {source: $s.sha256, name: $s.name, in: $s.in, n: ($mine | length), matched: $matched}
  } | where matched > 0 | insert coverage {|s| $s.matched / $s.n }
  let best = $live | join $members sha256 | join $scored source | group-by path | values | each {|rows|
    $rows | sort-by -c {|a, b|
      if $a.coverage != $b.coverage { $a.coverage > $b.coverage
      } else if $a.n != $b.n { $a.n < $b.n
      } else { $a.name > $b.name }
    } | first
  }
  let unmatched = $live | where path not-in ($best | get -o path | default [])

  # which downloads each mod of the import came from, and its unmatched files
  let mod_spread = $live | get mod | uniq | each {|m|
    {mod: $m, sources: ($best | where mod == $m | get source | uniq), unmatched: ($unmatched | where mod == $m | length)}
  }
  let stems = $best | get name | uniq | each {|n| first-token (split-version (without-ext $n) | get stem) | str lowercase }
  # a mod that is split up or merged, with no files left over, frees its id for a piece
  let merged_mods = $best | group-by source | values | where {|rows| ($rows | get mod | uniq | length) > 1 } | each { get mod } | flatten
  let freed = $mod_spread | where {|m| $m.unmatched == 0 and (($m.sources | length) > 1 or $m.mod in $merged_mods) } | get mod
  mut taken = read-catalog $data | get -o id | default [] | where $it not-in $freed
  mut suggestions = []
  for rows in ($best | group-by source | values | sort-by {|r| $r.0.name }) {
    let r0 = $rows.0
    let mods = $rows | get mod | uniq
    let spread = $mod_spread | where mod in $mods
    let change = if ($mods | length) > 1 { 'merge'
    } else if ($spread.0.sources | length) > 1 { 'split'
    } else if $spread.0.unmatched > 0 { 'partial'
    } else { 'same' }
    let id = if $change in [same partial] { $mods.0 } else {
      let guess = source-id $r0.name $mods $stems
      if $guess in $taken {
        2.. | each {|n| $"($guess)-($n)" } | where $it not-in $taken | first
      } else { $guess }
    }
    $taken = $taken | append $id
    $suggestions = $suggestions | append {
      change: $change
      mod: $id
      source: $r0.name
      in: ($names | get -o ($r0.in | default ''))
      version: (split-version (without-ext $r0.name) | get version)
      files: ($rows | length)
      of: $r0.n
      was: $mods
      paths: ($rows | get path | sort)
    }
  }
  let suggestions = $suggestions

  let summary = {
    import: $import
    modFiles: ($live | length)
    matched: ($best | length)
    unmatched: ($unmatched | length)
    changes: ($suggestions | group-by change | transpose change rows | each {|c| {change: $c.change, downloads: ($c.rows | length)} })
    mods: ($suggestions | reject paths | sort-by mod)
    unmatchedByMod: ($unmatched | group-by mod | transpose mod rows | each {|g| {mod: $g.mod, files: ($g.rows | get path)} } | sort-by mod)
    written: null
  }
  if not $write { return $summary }

  let grouping = read-grouping $data
  let all_paths = $report.files | where kind != unmanaged | get path
  # one rule per download (a mod in both `groups` and `rules` would be two groups):
  # whole top folders that came from it alone, then its other files
  let rules = $suggestions | where change in [split merge] | each {|s|
    let folders = $s.paths | where {|p| $p | str contains '/' } | each {|p| $p | path split | first } | uniq | where {|f|
      $live | where {|l| $l.path | str starts-with $"($f)/" } | get path | all {|p| $p in $s.paths }
    }
    let loose = $s.paths | where {|p| not ($folders | any {|f| $p | str starts-with $"($f)/" }) }
    let parts = ($folders | each {|f| ($f | str escape-regex) + '/.*' }) ++ ($loose | each { str escape-regex })
    {match: ('^(?:' + ($parts | str join '|') + ')$'), mod: $s.mod, version: $s.version}
  }
  let ruled = $rules | each {|r| $all_paths | where $it =~ $r.match } | flatten
  let hidden = {|k| $ruled | any {|p| $p == $k or ($p | str starts-with $"($k)/") } }
  let kept = $grouping.groups | transpose k v | where {|g| not (do $hidden $g.k) } | reduce -f {} {|g, acc| $acc | insert $g.k $g.v }
  # hand-written groups the new rules would hide become rules after them, so they
  # still take the files the downloads didn't
  let demoted = $grouping.groups | transpose k v | where {|g| do $hidden $g.k }
  let demoted_rules = $demoted | each {|g| $g.v | insert match ('^' + ($g.k | str escape-regex) + '(?:/|$)') }
  let file = $data | path join import grouping.suggested.json
  $grouping | merge {
    groups: $kept
    rules: ([...$rules ...$demoted_rules ...$grouping.rules] | each {|r|
      if $r.version? == null { $r | select match mod } else { $r | select match mod version }
    })
  } | to json -i 2 | save -f $file
  $summary | update written $file | insert groupsToRules ($demoted | get k)
}

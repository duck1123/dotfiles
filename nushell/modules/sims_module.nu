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
# This file, for `sims web`'s server to `use`
const sims_module = path self

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

# Version labels compared as Nix's builtins.compareVersions does (the framework picks
# the highest label with it): -1, 0 or 1
def label-cmp [a: string, b: string]: nothing -> int {
  let parts = {|s| $s | parse -r '(\d+|[^\d.-]+)' | get capture0 }
  let pa = do $parts $a
  let pb = do $parts $b
  let num = {|c| if $c =~ '^\d+$' { $c | into int } }
  let lt = {|x, y|
    let nx = do $num $x
    let ny = do $num $y
    if $nx != null and $ny != null { $nx < $ny
    } else if $x == '' and $ny != null { true
    } else if $x == pre and $y != pre { true
    } else if $y == pre { false
    } else if $ny != null { true
    } else if $nx != null { false
    } else { $x < $y }
  }
  for i in 0..<([($pa | length) ($pb | length)] | math max) {
    let x = $pa | get -o $i | default ''
    let y = $pb | get -o $i | default ''
    if (do $lt $x $y) { return (-1) }
    if (do $lt $y $x) { return 1 }
  }
  0
}

# Labels, highest first
def labels-desc []: list<string> -> list<string> {
  sort-by -c {|a, b| (label-cmp $a $b) > 0 }
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
      print -e $"warning: ($mods_changes) files differ between the backup's Mods snapshot and ($t.name)'s Mods/ \(--with-mods to restore them\)"
    }
    print -e 'dry run: add --apply to restore'
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
    print -e 'dry run: add --apply to back up and wipe'
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
    print -e 'dry run: add --apply to back up and delete'
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
    print -e 'dry run: add --apply to delete'
    return $found
  }
  for b in $found { rm -r ($root | path join $b.id) }
  $found | update deleted true
}

# Mods (simsnix). The catalog and profiles are evaluated by the framework in
# dotfiles' modules/sims4/_simsnix with `nix eval --impure`; the catalog is read
# in place from dataDir, so nothing from it is copied to /nix/store.

# Evaluate the catalog with the framework. `body` is a Nix expression over `lib` and
# `r`, eval.nix's result for `profile` (with `raw = true` when --raw)
def simsnix-eval [profile: any, body: string, --raw]: nothing -> any {
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
    profile: $profile
    raw: $raw
  }
  let expr = '
    let
      a = builtins.fromJSON (builtins.getEnv "SIMSNIX_ARGS");
      lib = import (/. + a.nixpkgsLib);
      r = import (/. + a.framework + "/eval.nix") {
        inherit lib;
        sources = map (s: /. + s) a.sources;
        inherit (a) profile raw;
      };
    in (' + $body + ')'
  let r = with-env {SIMSNIX_ARGS: ($args | to json -r)} { ^nix eval --impure --json --expr $expr | complete }
  if $r.exit_code != 0 {
    # the framework's own errors read better without nix's trace
    let own = $r.stderr | lines | skip until {|l| $l =~ 'error: simsnix: ' } | str trim | str replace -r '^error: ' ''
    error make {msg: (if ($own | is-empty) { $r.stderr } else { $own | str join "\n" })}
  }
  $r.stdout | from json
}

# Evaluate a profile into the manifest `sims switch` will install
export def "sims manifest" [
  profile?: string # profile to evaluate (default: "default")
]: nothing -> record {
  simsnix-eval ($profile | default default) r
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
    # a loose state file belongs to no mod, unless a rule or a `groups` key gives it one
    $f | insert group $group | insert folder $folder | insert name $name | insert kind (
      if $folder == null and $rule == null and $key == null and $kind == state { 'unmanaged' } else { $kind }
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
    print -e 'dry run: add --apply to write the blobs and the catalog'
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

# Check the profiles against the catalog and the blob store: the framework's checks
# (unknown profiles, mods and versions, requires and conflicts, clashing targets and
# folders), then that every file of each enabled mod's version is in the blob store.
# --hash also re-hashes those blobs. `catalog` holds the problems that don't depend on
# the profile evaluated (unknown mod ids are among them, labelled with their profile).
export def "sims validate" [
  profile?: string # only this profile (default: all of them)
  --hash           # re-hash the blobs, not only check they're there
]: nothing -> record {
  let body = '{
    inherit (r) failures;
    profiles = lib.attrNames r.config.profiles;
    mods = lib.length r.config.activeMods;
    files = lib.concatMap (
      m:
      let
        v = if m.selectedVersion == null then null else m.versions.${m.selectedVersion} or null;
      in
      if !m.enable || v == null then
        [ ]
      else
        map (f: {
          mod = m.id;
          version = m.selectedVersion;
          inherit (f) name sha256;
        }) v.files
    ) (lib.concatMap lib.attrValues (lib.attrValues r.config.mods));
  }'
  let all = simsnix-eval null $body --raw
  let names = if $profile == null { $all.profiles } else if $profile in $all.profiles { [$profile] } else {
    error make {msg: $"unknown profile ($profile) \(known: ($all.profiles | str join ', '))"}
  }
  let root = blob-root
  let results = $names | each {|p|
    let r = simsnix-eval $p $body --raw
    let missing = $r.files | where {|f| not ($root | path join $f.sha256 | path exists) }
    let bad = if $hash {
      $r.files | where sha256 not-in $missing.sha256 | uniq-by sha256 | par-each {|f|
        if (sha256-of ($root | path join $f.sha256)) != $f.sha256 { $f }
      }
    } else { [] }
    {
      profile: $p
      mods: $r.mods
      files: ($r.files | length)
      problems: ($r.failures | where $it not-in $all.failures)
      missingBlobs: $missing
      badBlobs: $bad
    }
  }
  let ok = ($all.failures | is-empty) and ($results | all {|r|
    ($r.problems | is-empty) and ($r.missingBlobs | is-empty) and ($r.badBlobs | is-empty)
  })
  {ok: $ok, catalog: $all.failures, profiles: $results}
}

# Switching an instance's Mods/ to a profile. simsnix owns only the files it installed
# or adopted, as listed in Mods/.simsnix-state.json:
#   { profile, switched, files: [{ target, sha256, mod }], dirs: [folder, ...] }
# `dirs` are the folders a switch created; one is removed once it's empty. Every
# other file in Mods/ is loose: never changed, moved or deleted.

const switch_state = '.simsnix-state.json'

def read-switch-state [mods_dir: path]: nothing -> record {
  let file = $mods_dir | path join $switch_state
  let found = if ($file | path exists) { open $file } else { {} }
  {profile: null, switched: null, files: [], dirs: []} | merge $found
}

# By process name (Wine names the game's process after its .exe), not command line,
# so a shell or editor that mentions it doesn't count
def game-running []: nothing -> bool {
  ^ps -eo comm= | lines | any {|c| ($c | str trim) =~ '^TS4_(DX9_)?x64\.exe$' }
}

# Put a manifest file at dest: a hardlink to its blob (a copy on another filesystem),
# or unpacked from its archive's blob and checked against its hash
def install-file [root: path, tmp: path, f: record, dest: path] {
  if $f.archive? == null {
    let src = $root | path join $f.sha256
    if (^ln -- $src $dest | complete | get exit_code) != 0 {
      let c = ^cp -- $src $dest | complete
      if $c.exit_code != 0 { error make {msg: $"can't install ($f.target): ($c.stderr)"} }
    }
  } else {
    let out = mktemp -d -p $tmp
    # -spd: the member's name is not a wildcard
    let r = ^7zz e -y -bso0 -bsp0 -p- -spd $"-o($out)" -- ($root | path join $f.archive) $f.member | complete
    let file = $out | path join ($f.member | path basename)
    if $r.exit_code != 0 or not ($file | path exists) {
      rm -rf $out
      error make {msg: $"can't unpack ($f.member) for ($f.target): ($r.stderr)"}
    }
    if (sha256-of $file) != $f.sha256 {
      rm -rf $out
      error make {msg: $"($f.member) in ($f.archive) doesn't match its recorded hash"}
    }
    ^chmod 444 -- $file
    ^mv -- $file $dest
    rm -rf $out
  }
}

# Folders above `rel` (relative to Mods/) that don't exist yet, outermost first
def missing-dirs [mods_dir: path, rel: string]: nothing -> list<string> {
  let parts = $rel | path split | drop 1
  0..<($parts | length) | each {|n| $parts | first ($n + 1) | path join } | where {|d|
    not ($mods_dir | path join $d | path exists)
  }
}

# Install a profile into an instance's Mods/. Dry run unless --apply.
#
# Every file of the profile is checked against what's there. A file with the right
# content is kept (adopted, if simsnix didn't install it). A missing one is installed.
# A file simsnix owns that the profile no longer has is removed. Nothing simsnix
# doesn't own is ever touched: a different file at a profile's path is a conflict, and
# --apply refuses until it's resolved by hand. A file simsnix installed that has changed
# since is a conflict too, unless --force; one the profile dropped is then left in place
# and disowned.
#
# --apply refuses while the game runs, and first backs up the newest save (`sims save
# backup`) unless --no-backup.
export def "sims switch" [
  profile: string                                       # profile to install
  --instance (-i): string@"nu-complete sims instances" # instance (default from config)
  --apply                                               # change Mods/
  --force                                               # replace or remove simsnix's files even if changed
  --no-backup                                           # don't back up the newest save first
]: nothing -> record {
  let i = instance $instance
  let mods_dir = $i.documents | path join Mods
  if not ($mods_dir | path exists) { error make {msg: $"($mods_dir) not found"} }
  let manifest = sims manifest $profile
  let state = read-switch-state $mods_dir
  let live = hash-tree $mods_dir | where path != $switch_state
  let key = {|p| $p | str lowercase }
  let live_by = $live | group-by {|f| do $key $f.path }
  let owned_by = $state.files | group-by {|f| do $key $f.target }
  let wanted_keys = $manifest.files | each {|f| do $key $f.target }

  let wanted_ops = $manifest.files | each {|w|
    let k = do $key $w.target
    let l = $live_by | get -o $k | get -o 0
    let o = $owned_by | get -o $k | get -o 0
    let op = if $l == null { 'install'
    } else if $l.sha256 == $w.sha256 { if $o != null { 'keep' } else { 'adopt' }
    } else if $o != null and ($o.sha256 == $l.sha256 or $force) { 'replace'
    } else { 'conflict' }
    let reason = if $op != conflict { null } else if $o != null { 'changed since simsnix installed it' } else { "not simsnix's file" }
    $w | insert op $op | insert path ($l.path? | default $w.target) | insert reason $reason
  }
  let dropped_ops = $state.files | where {|o| (do $key $o.target) not-in $wanted_keys } | each {|o|
    let l = $live_by | get -o (do $key $o.target) | get -o 0
    let op = if $l == null { 'gone' } else if $l.sha256 == $o.sha256 or $force { 'remove' } else { 'disown' }
    $o | insert op $op | insert path ($l.path? | default $o.target)
  }
  let ops = [...$wanted_ops ...$dropped_ops]
  let conflicts = $ops | where op == conflict
  let owned_keys = $state.files | each {|o| do $key $o.target }
  let loose = $live | where {|f| (do $key $f.path) not-in $wanted_keys and (do $key $f.path) not-in $owned_keys } | get path | each {|p| $mods_dir | path join $p }
  let count = {|name| $ops | where op == $name | length }
  let summary = {
    instance: $i.name
    profile: $profile
    from: $state.profile
    keep: (do $count keep)
    adopt: (do $count adopt)
    install: (do $count install)
    replace: (do $count replace)
    remove: (do $count remove)
    disown: (do $count disown)
    gone: (do $count gone)
    changes: ($ops | where op in [install replace remove disown] | select op path mod)
    conflicts: ($conflicts | select path mod reason)
    loose: $loose
    applied: false
  }
  if not $apply {
    print -e 'dry run: add --apply to change Mods/'
    return $summary
  }
  if ($conflicts | is-not-empty) {
    error make {msg: $"($conflicts | length) conflict\(s\): resolve them by hand first \(see `sims switch ($profile)` without --apply\)"}
  }
  if (game-running) { error make {msg: 'the game is running: close it first'} }
  let changing = ($ops | where op in [install replace remove] | is-not-empty)
  if $changing and not $no_backup {
    try {
      sims save backup --instance $i.name --label $"before-switch-($profile)" | ignore
    } catch {|e|
      error make {msg: $"nothing changed: the save backup failed \(($e.msg)\); pass --no-backup to switch without one"}
    }
  }

  let root = blob-root
  let tmp = sims-config | get blobDir | path expand | path join tmp
  mkdir $tmp
  # removals first, so a path can move from one mod to another
  let order = {|op| match $op { remove => 0, disown => 0, gone => 0, replace => 1, install => 2, _ => 3 } }
  let start = {files: ($state.files | where {|o| (do $key $o.target) in $wanted_keys }), dirs: $state.dirs, error: null}
  let result = $ops | sort-by {|op| do $order $op.op } | reduce -f $start {|op, acc|
    if $acc.error != null { $acc } else {
      try {
        let dest = $mods_dir | path join $op.path
        let k = do $key $op.target
        let without = $acc.files | where {|f| (do $key $f.target) != $k }
        let entry = {target: $op.path, sha256: $op.sha256, mod: $op.mod}
        match $op.op {
          'remove' => { rm -f $dest; $acc }
          'disown' | 'gone' => { $acc }
          'keep' | 'adopt' => { $acc | update files ($without | append $entry) }
          _ => {
            let new_dirs = missing-dirs $mods_dir $op.path
            if $new_dirs != [] { mkdir ($dest | path dirname) }
            if $op.op == replace { rm -f $dest }
            install-file $root $tmp $op $dest
            {files: ($without | append $entry), dirs: ($acc.dirs | append $new_dirs | uniq), error: null}
          }
        }
      } catch {|e| $acc | update error $"($op.path): ($e.msg)" }
    }
  }
  rm -rf $tmp
  # the folders simsnix made that are now empty, deepest first
  let dirs = $result.dirs | sort-by {|d| $d | path split | length } --reverse | reduce -f [] {|d, kept|
    let full = $mods_dir | path join $d
    if not ($full | path exists) { $kept
    } else if (ls -a $full | is-empty) { rm $full; $kept
    } else { $kept | append $d }
  } | sort
  {
    profile: (if $result.error == null { $profile } else { $state.profile })
    switched: (date now | format date '%Y-%m-%dT%H:%M:%S')
    files: ($result.files | sort-by target)
    dirs: $dirs
  } | to json -i 2 | save -f ($mods_dir | path join $switch_state)
  if $result.error != null {
    error make {msg: $"switch stopped at ($result.error); .simsnix-state.json records what was done"}
  }
  let thumbs = $i.documents | path join localthumbcache.package
  if $changing and ($thumbs | path exists) {
    print -e $"mods changed: the game rebuilds ($thumbs) if you delete it"
  }
  $summary | update applied true
}

def "nu-complete sims mods" []: nothing -> list<string> {
  let dir = sims-config | get dataDir | path expand | path join mods
  if not ($dir | path exists) { return [] }
  glob $"($dir)/*/*.{json,nix}" | each {|f|
    $f | path relative-to $dir | str replace -r '\.(json|nix)$' ''
  } | uniq | sort
}

# Rewrites of every reference to mod ids in dataDir, for `swap` ([{from, to}]): the
# import/<name>.json records, profiles/*.json, grouping.json and
# grouping.suggested.json, the requires and conflicts of catalog mods not in `skip`,
# and the quoted id in .nix files. Other mentions (an attribute path, notes) are
# only listed. Each edit is { file, refs, json } (`text` for the .nix files); a
# grouping file is listed even when it doesn't name the ids (refs 0).
def id-ref-edits [data: path, swap: list<record>, skip: list<string>]: nothing -> record {
  let olds = $swap | get from
  let to = {|id| $swap | where from == $id | get -o 0.to | default $id }
  let rel = {|f| $f | path relative-to $data }

  let records = import-reports $data | each {|f| {file: $f, json: (open $f)} }
  let reports = $records | each {|r|
    let refs = $r.json.files | where {|x| $x.mod in $olds } | length
    if $refs > 0 {
      let files = $r.json.files | each {|x| if $x.mod in $olds { $x | update mod (do $to $x.mod) } else { $x } }
      {file: $r.file, refs: $refs, json: ($r.json | update files $files)}
    }
  }
  let profiles_dir = $data | path join profiles
  let profiles = if ($profiles_dir | path exists) { glob $"($profiles_dir)/*.json" } else { [] } | each {|f|
    let p = open $f
    let refs = $p.mods? | default [] | where $it in $olds | length
    if $refs > 0 {
      {file: $f, refs: $refs, json: ($p | update mods ($p.mods | each {|m| do $to $m } | uniq | sort))}
    }
  }
  let deps = read-catalog $data | where id not-in $skip | each {|c|
    let fields = [requires conflicts] | where {|k| $c.mod | get -o $k | default [] | any {|r| $r in $olds } }
    if ($fields | is-not-empty) {
      let json = $fields | reduce -f $c.mod {|k, m| $m | update $k ($m | get $k | each {|r| do $to $r } | uniq) }
      {file: $c.path, refs: ($fields | length), json: $json}
    }
  }

  # grouping.json and grouping.suggested.json: `groups` entries and rules
  let groupings = [grouping.json grouping.suggested.json] | each {|n| $data | path join import $n } | where { path exists } | each {|f|
    let g = open $f
    let groups = $g.groups? | default {}
    let rules = $g.rules? | default []
    let in_groups = $groups | values | where {|v| $v.mod? in $olds } | length
    let in_rules = $rules | where {|r| $r.mod? in $olds } | length
    let renamed = if $in_groups == 0 { $g } else {
      $g | update groups ($groups | transpose k v | reduce -f {} {|e, acc|
        $acc | insert $e.k (if $e.v.mod? in $olds { $e.v | update mod (do $to $e.v.mod) } else { $e.v })
      })
    }
    let renamed = if $in_rules == 0 { $renamed } else {
      $renamed | update rules ($rules | each {|r| if $r.mod? in $olds { $r | update mod (do $to $r.mod) } else { $r } })
    }
    {file: $f, refs: ($in_groups + $in_rules), json: $renamed}
  }

  # the quoted id in .nix files; other mentions of it are only listed
  let text_files = glob $"($data)/**/*.{nix,md}" | where {|f| '/.git/' not-in $f }
  let texts = $text_files | where { str ends-with .nix } | each {|f|
    let t = open --raw $f | decode utf-8
    let refs = $swap | each {|s| ($t | split row $'"($s.from)"' | length) - 1 } | math sum
    if $refs > 0 {
      let text = $swap | reduce -f $t {|s, acc| $acc | str replace -a $'"($s.from)"' $'"($s.to)"' }
      {file: $f, refs: $refs, text: $text}
    }
  }
  let needles = $olds | each {|o| [$o ($o | str replace '/' '.')] } | flatten
  let mentions = $text_files | each {|f|
    let t = $texts | where file == $f | get -o 0.text | default (open --raw $f | decode utf-8)
    $t | lines | enumerate | where {|l| $needles | any {|n| $l.item | str contains $n } } | each {|l|
      {file: (do $rel $f), line: ($l.index + 1), text: ($l.item | str trim)}
    }
  } | flatten

  {
    records: $records
    reports: $reports
    profiles: $profiles
    deps: $deps
    groupings: $groupings
    texts: $texts
    mentions: $mentions
  }
}

# A `--folder` value as the catalog stores it: `default` drops the field (the mod
# gets <creator>_<mod>), '' is null (straight into Mods/), else the name itself
def folder-value [folder: string]: nothing -> record {
  let f = $folder | str trim
  if $f == default { return {drop: true, value: null} }
  if $f == '' { return {drop: false, value: null} }
  if ($f | str starts-with '/') or ($f | path split | any {|c| $c in ['.' '..'] }) {
    error make {msg: $"($folder): a folder is a path relative to Mods/"}
  }
  {drop: false, value: ($f | str trim -c '/')}
}

# For a flattened mod: one entry per file in each version. Entries with the same
# hash and base name install to the same place, so only the shortest path is kept.
# Returns { mod, collapsed } (how many entries were dropped).
def collapse-flat [mod: record]: nothing -> record {
  if not ($mod.flatten? | default false) { return {mod: $mod, collapsed: 0} }
  let before = $mod.versions | values | each { get files | length } | math sum
  let versions = $mod.versions | items {|label, v|
    let files = $v.files | enumerate
      | insert key {|e| $"($e.item.name | path basename | str lowercase)\t($e.item.sha256)" }
      | insert len {|e| $e.item.name | str length }
      | sort-by len | uniq-by key | sort-by index | get item
    {label: $label, v: ($v | update files $files)}
  } | reduce -f {} {|e, acc| $acc | insert $e.label $e.v }
  let after = $versions | values | each { get files | length } | math sum
  {mod: ($mod | update versions $versions), collapsed: ($before - $after)}
}

def apply-folder [mod: record, folder: record]: nothing -> record {
  if $folder.drop { $mod | reject -o folder } else { $mod | upsert folder $folder.value }
}

# Merge mods into one: their versions, by label, become `into`'s, and every profile,
# import record, grouping rule and requires/conflicts that names them names `into`
# instead. Then their catalog files are deleted. Dry run unless --apply.
#
# A version label the mods share gets the files of all of them (one copy of a file
# two mods have). The mods must agree on stripComponents, include and exclude, since
# those apply to every file of the merged mod. A pinned `version` becomes the highest
# pin among them. Other fields are `into`'s, filled in from the others where unset;
# tags, stateFiles, requires and conflicts are combined.
#
# --folder sets where the merged mod installs: a folder under Mods/, `default` for
# <creator>_<mod>, or '' for Mods/ itself. --flatten installs every file directly in
# that folder, whatever path it was imported at.
export def "sims mod merge" [
  into: string@"nu-complete sims mods"     # the mod to keep, creator/mod
  ...mods: string@"nu-complete sims mods"  # the mods to fold into it
  --folder: string                         # folder under Mods/, `default` or ''
  --flatten                                # install files directly in the folder
  --apply                                  # write the files
  --commit                                 # with --apply, commit the files it changed
]: nothing -> record {
  let data = sims-config | get dataDir | path expand
  let mods = $mods | uniq | where $it != $into
  if ($mods | is-empty) { error make {msg: "name at least one mod to merge into it"} }
  let file_of = {|id| $data | path join mods $"($id).json" }
  for id in [$into ...$mods] {
    if not (do $file_of $id | path exists) {
      error make {msg: (if ($data | path join mods $"($id).nix" | path exists) {
        $"($id) is defined in Nix: merge it by hand"
      } else { $"($id) isn't in the catalog" })}
    }
  }
  let all = [$into ...$mods] | each {|id| {id: $id, mod: (open (do $file_of $id))} }

  # fields that decide which files install must match
  let differ = [stripComponents include exclude] | each {|k|
    let vals = $all | each {|m| {id: $m.id, v: ($m.mod | get -o $k)} }
    let norm = $vals | each {|x| if $k == stripComponents { $x.v | default 0 } else { $x.v | default [] } }
    if ($norm | uniq | length) > 1 { $"($k): ($vals | each {|x| $'($x.id)=($x.v | to json -r)' } | str join ', ')" }
  }
  if ($differ | is-not-empty) {
    error make {msg: $"the mods install files differently; make these match first:\n($differ | str join "\n")"}
  }

  # versions by label: every mod's files, one per sha256; one name with two hashes clashes
  let labels = $all | each {|m| $m.mod.versions | columns } | flatten | uniq
  let versions = $labels | reduce -f {} {|label, acc|
    let parts = $all | where {|m| $label in ($m.mod.versions | columns) } | each {|m| $m.mod.versions | get $label }
    let files = $parts | each {|v| $v.files } | flatten | uniq-by sha256
    let clash = $files | group-by name | values | where { length | $in > 1 } | each { get 0.name }
    if ($clash | is-not-empty) {
      error make {msg: $"version ($label): different files named ($clash | str join ', ')"}
    }
    let added = $parts | get -o added | compact | sort | get -o 0
    let game = $parts | get -o gameVersion | compact | get -o 0
    let v = {added: $added, files: $files}
    $acc | insert $label (if $game == null { $v } else { $v | insert gameVersion $game })
  }
  let pins = $all | each {|m| $m.mod.version? } | compact | uniq
  let pin = if ($pins | is-empty) { null } else {
    $pins | labels-desc | first
  }

  let base = $all.0.mod
  let others = $all | skip 1 | get mod
  let combine = {|k| $all | each {|m| $m.mod | get -o $k | default [] } | flatten | uniq | sort }
  let merged_ids = [$into ...$mods]
  let fill = {|k| [$base ...$others] | each {|m| $m | get -o $k } | compact | get -o 0 }
  let merged = $base
    | upsert title (do $fill title)
    | upsert downloadPage (do $fill downloadPage)
    | upsert tags (do $combine tags)
    | upsert stateFiles (do $combine stateFiles)
    | upsert requires (do $combine requires | where $it not-in $merged_ids)
    | upsert conflicts (do $combine conflicts | where $it not-in $merged_ids)
    | upsert versions $versions
    | upsert version $pin
  let merged = if $folder == null { $merged } else { apply-folder $merged (folder-value $folder) }
  let merged = if $flatten { $merged | upsert flatten true } else { $merged }
  let merged = collapse-flat $merged | get mod
  let versions = $merged.versions

  # the newest version of each mod should still be in the merged mod's selected one
  let selected = $pin | default ($labels | labels-desc | first)
  # (or a file of the same name, which replaces it)
  let selected_files = $versions | get $selected | get files
  let selected_names = $selected_files | get name | each { path basename | str lowercase }
  let dropped = $all | each {|m|
    let newest = $m.mod.version? | default ($m.mod.versions | columns | labels-desc | get -o 0)
    if $newest != null {
      $m.mod.versions | get $newest | get files | where {|f|
        $f.sha256 not-in $selected_files.sha256 and ($f.name | path basename | str lowercase) not-in $selected_names
      } | each {|f|
        {mod: $m.id, version: $newest, file: $f.name}
      }
    }
  } | flatten

  let refs = id-ref-edits $data ($mods | each {|m| {from: $m, to: $into} }) $merged_ids
  let edits = [...$refs.reports ...$refs.profiles ...$refs.deps ...($refs.groupings | where refs > 0)]
  let rel = {|f| $f | path relative-to $data }
  let summary = {
    into: $into
    merged: $mods
    folder: ($merged.folder? | default (if 'folder' in ($merged | columns) { null } else { $into | str replace '/' '_' }))
    flatten: ($merged.flatten? | default false)
    version: $selected
    versions: ($versions | transpose label v | each {|r|
      {label: $r.label, files: ($r.v.files | get name)}
    } | sort-by -c {|a, b| (label-cmp $a.label $b.label) > 0 })
    notInSelected: $dropped
    rewritten: ([...$edits ...$refs.texts] | each {|e| {file: (do $rel $e.file), refs: $e.refs} })
    removed: ($mods | each {|m| do $rel (do $file_of $m) })
    mentions: $refs.mentions
    applied: false
  }
  if not $apply {
    print -e 'dry run: add --apply to write the files'
    return $summary
  }

  for e in $edits { $e.json | to json -i 2 | save -f $e.file }
  for e in $refs.texts { $e.text | save -f $e.file }
  $merged | to json -i 2 | save -f (do $file_of $into)
  for m in $mods {
    rm (do $file_of $m)
    let dir = do $file_of $m | path dirname
    if (ls -a $dir | is-empty) { rm $dir }
  }
  let commit = if $commit {
    let touched = [...$edits ...$refs.texts] | get file | append (do $file_of $into) | append ($mods | each {|m| do $file_of $m })
    commit-data $data $touched $"sims: merge ($mods | str join ', ') into ($into)"
  }
  $summary | update applied true | insert commit $commit
}

# Rename a mod, e.g. unknown/foo to someone/foo. Dry run unless --apply.
#
# Moves mods/<creator>/<mod>.json (and .nix) and rewrites every reference to the id:
# the import/<name>.json records, profiles/*.json, grouping.json and
# grouping.suggested.json, other mods' requires and conflicts, and the quoted id in the
# data folder's .nix files. Other mentions (an attribute path, notes) are listed, not
# changed.
#
# Unless grouping.json already names the mod, its top folders get `groups` entries, so
# a --fresh import keeps the new id instead of guessing again. A folder that other mods
# have files in isn't pinned, and neither is a mod of loose files.
export def "sims mod rename" [
  old: string@"nu-complete sims mods" # the mod's id, creator/mod
  new: string                         # its new id
  --apply                             # move and rewrite the files
  --commit                            # with --apply, commit the files it changed
]: nothing -> record {
  let data = sims-config | get dataDir | path expand
  if $new !~ '^[a-z0-9][a-z0-9-]*/[a-z0-9][a-z0-9-]*$' {
    error make {msg: $"($new): an id is creator/mod, in lowercase letters, digits and -"}
  }
  if $new == $old { error make {msg: "the new id is the old one"} }
  let path_of = {|id, ext| $data | path join mods $"($id).($ext)" }
  let moves = [json nix] | each {|ext| {from: (do $path_of $old $ext), to: (do $path_of $new $ext)} } | where {|m| $m.from | path exists }
  if ($moves | is-empty) { error make {msg: $"($old) isn't in the catalog"} }
  if ([json nix] | any {|ext| do $path_of $new $ext | path exists }) {
    error make {msg: $"($new) is already in the catalog"}
  }
  let rel = {|f| $f | path relative-to $data }
  let refs = id-ref-edits $data [{from: $old, to: $new}] [$old]
  let reports = $refs.reports
  let profiles = $refs.profiles
  let deps = $refs.deps
  let groupings = $refs.groupings
  let texts = $refs.texts
  let mentions = $refs.mentions
  let grouping_file = $data | path join import grouping.json

  # top folders to pin in grouping.json: those the imports put this mod's files in,
  # unless another mod has files there too or a `groups` key already covers them
  let main = $groupings | where file == $grouping_file | get -o 0
  let named = $main != null and $main.refs > 0
  let owned = $refs.records | each {|r| $r.json.files } | flatten
  let keys = $main.json.groups? | default {} | columns
  let folders = $owned | where mod == $old and ($it.path | str contains '/') | each {|f| $f.path | path split | first } | uniq | sort
  let candidates = $folders | where {|d| not ($keys | any {|k| $d == $k or ($d | str starts-with $"($k)/") }) }
  let shared = $candidates | where {|d|
    $owned | any {|f| ($f.path | str starts-with $"($d)/") and $f.mod != null and $f.mod != $old }
  }
  let pins = if $named { [] } else { $candidates | where $it not-in $shared }
  let grouping_edit = if ($pins | is-empty) {
    if $main != null and $main.refs > 0 { $main }
  } else {
    let base = $main.json? | default {}
    let groups = $pins | reduce -f ($base.groups? | default {}) {|d, acc| $acc | insert $d {mod: $new} }
    {file: $grouping_file, refs: ($main.refs? | default 0), json: ($base | upsert groups $groups)}
  }
  let edits = [
    ...$reports ...$profiles ...$deps
    ...($groupings | where file != $grouping_file and refs > 0)
    ...(if $grouping_edit == null { [] } else { [$grouping_edit] })
  ]

  let summary = {
    from: $old
    to: $new
    moved: ($moves | each {|m| $"(do $rel $m.from) -> (do $rel $m.to)" })
    rewritten: ([...$edits ...$texts] | each {|e| {file: (do $rel $e.file), refs: $e.refs} })
    pinned: $pins
    notPinned: (if $named { 'grouping.json already names it' } else if ($folders | is-empty) { 'loose files: no folder to pin' } else { $shared })
    mentions: $mentions
    applied: false
  }
  if not $apply {
    print -e 'dry run: add --apply to move and rewrite the files'
    return $summary
  }

  for e in $edits { $e.json | to json -i 2 | save -f $e.file }
  for e in $texts { $e.text | save -f $e.file }
  for m in $moves {
    mkdir ($m.to | path dirname)
    mv $m.from $m.to
  }
  let old_dir = $data | path join mods ($old | path dirname)
  if (ls -a $old_dir | is-empty) { rm $old_dir }
  let commit = if $commit {
    let touched = [...$edits ...$texts] | get file | append ($moves | each {|m| [$m.from $m.to] } | flatten)
    commit-data $data $touched $"sims: rename ($old) to ($new)"
  }
  $summary | update applied true | insert commit $commit
}

# Commit `files` (paths in dataDir, which may have been moved away or deleted) in the
# repo holding dataDir, and nothing else that's uncommitted there. Returns the commit's
# short hash, or null when none of the files changed.
def commit-data [data: path, files: list<path>, message: string]: nothing -> any {
  let files = $files | uniq
  if ($files | is-empty) { return null }
  data-git $data add -A -- ...$files
  # exit 1: something staged in them
  if (data-git $data diff --cached --quiet -- ...$files).exit_code == 0 { return null }
  data-git $data commit -q -m $message -- ...$files
  data-git $data rev-parse --short HEAD | get stdout | str trim
}

# git in dataDir's repo; fails on an error (exit 1 is only an answer for `diff`)
def --wrapped data-git [data: path, ...args: string]: nothing -> record {
  let r = ^git -C $data ...$args | complete
  if $r.exit_code > 1 or ($r.exit_code == 1 and $args.0 != diff) {
    error make {msg: $"git ($args.0): ($r.stderr | str trim)"}
  }
  $r
}

# Set a mod's catalog fields in mods/<creator>/<mod>.json. An empty --page or --title
# clears it; --tags replaces the list. --folder is where it installs: a folder under
# Mods/, `default` for <creator>_<mod>, or '' for Mods/ itself; --flatten puts every
# file directly in that folder, dropping the path it was imported at. Writes
# right away (there's no dry run); a new folder takes effect at the next `sims switch`.
export def "sims mod set" [
  id: string@"nu-complete sims mods" # the mod's id, creator/mod
  --page: string                     # download page, an http(s) URL
  --title: string                    # display name
  --tags: list<string>               # every tag the mod has
  --folder: string                   # folder under Mods/, `default` or ''
  --flatten                          # install files directly in the folder
  --no-flatten                       # install files at the paths they were imported at
  --commit                           # commit the change
]: nothing -> record {
  let data = sims-config | get dataDir | path expand
  let file = $data | path join mods $"($id).json"
  if not ($file | path exists) {
    error make {msg: (if ($data | path join mods $"($id).nix" | path exists) {
      $"($id) is defined in Nix: edit mods/($id).nix"
    } else { $"($id) isn't in the catalog" })}
  }
  if $page != null and $page != '' and $page !~ '^https?://\S+$' {
    error make {msg: $"($page): a download page is an http\(s\) URL"}
  }
  let tags = if $tags == null { null } else {
    let tags = $tags | each { str trim } | where { is-not-empty } | uniq | sort
    let bad = $tags | where { $in !~ '^[A-Za-z0-9][A-Za-z0-9 _.-]*$' }
    if ($bad | is-not-empty) { error make {msg: $"bad tags: ($bad | str join ', ')"} }
    $tags
  }
  let blank = {|v| if $v == '' { null } else { $v } }
  let wanted = {downloadPage: $page, title: $title, tags: $tags}
    | transpose field value | where value != null
    | update value {|r| if $r.field == tags { $r.value } else { do $blank ($r.value | str trim) } }
  if $flatten and $no_flatten { error make {msg: 'give --flatten or --no-flatten, not both'} }
  if ($wanted | is-empty) and $folder == null and not $flatten and not $no_flatten {
    error make {msg: 'nothing to set: give --page, --title, --tags, --folder or --flatten'}
  }

  let mod = open $file
  let next = $wanted | reduce -f $mod {|w, m| $m | upsert $w.field $w.value }
  let next = if $folder == null { $next } else { apply-folder $next (folder-value $folder) }
  let next = if $flatten { $next | upsert flatten true } else if $no_flatten { $next | reject -o flatten } else { $next }
  let flat = collapse-flat $next
  let next = $flat.mod
  # a folder field left out is the default folder
  let shown = {|m, k|
    if $k == folder and folder not-in ($m | columns) { $id | str replace '/' '_' } else { $m | get -o $k }
  }
  let changes = [downloadPage title tags folder flatten] | each {|k|
    let was = do $shown $mod $k
    let now = do $shown $next $k
    if $was != $now { {field: $k, from: $was, to: $now} }
  }
  let changes = if $flat.collapsed == 0 { $changes } else {
    $changes | append {field: versions, from: null, to: $"($flat.collapsed) duplicate file entries removed"}
  }
  let commit = if ($changes | is-empty) { null } else {
    $next | to json -i 2 | save -f $file
    if $commit {
      commit-data $data [$file] $"sims: set ($changes | get field | str join ', ') for ($id)"
    }
  }
  {id: $id, changes: $changes, commit: $commit}
}

# The profile last switched to on an instance, else `current`
def default-profile [instance?: string]: nothing -> string {
  let i = try { instance $instance } catch { null }
  let state = if $i != null { $i.documents | path join Mods $switch_state }
  let active = if $state != null and ($state | path exists) { open $state | get -o profile }
  $active | default current
}

# Every mod in the catalog, as the web UI browses it: its fields, versions, whether
# `profile` enables it, which profiles list it, and the framework's problems that
# name it. `problems` holds all of them, for the ones that name no mod.
export def "sims mod list" [
  --profile (-p): string                                # profile (default: the one last switched to, else current)
  --instance (-i): string@"nu-complete sims instances" # instance whose last switch picks the default profile
]: nothing -> record {
  let name = $profile | default (default-profile $instance)
  let body = '
    let
      c = r.config;
      mods = lib.concatMap lib.attrValues (lib.attrValues c.mods);
    in {
      inherit (r) failures;
      profiles = lib.mapAttrs (n: p: { inherit (p) base mods remove; }) c.profiles;
      mods = map (m: {
        inherit (m) id title downloadPage tags enable folder version selectedVersion requires conflicts;
        versions = lib.mapAttrs (l: v: {
          inherit (v) added gameVersion;
          files = map (f: { inherit (f) name sha256; members = lib.length f.members; }) v.files;
        }) m.versions;
      }) mods;
    }'
  let r = simsnix-eval $name $body --raw
  if $name not-in ($r.profiles | columns) {
    error make {msg: $"unknown profile ($name) \(known: ($r.profiles | columns | str join ', ')\)"}
  }
  let data = sims-config | get dataDir | path expand
  let listed_in = $r.profiles | transpose name p | each {|e| $e.p.mods | each {|id| {id: $id, profile: $e.name} } } | flatten
  let mods = $r.mods | each {|m|
    $m | merge {
      editable: ($data | path join mods $"($m.id).json" | path exists)
      profiles: ($listed_in | where id == $m.id | get profile)
      # the id as a whole word: acme/core must not match acme/core-extra
      problems: ($r.failures | where {|f| $f =~ ('(^|[^\w/-])' + ($m.id | str escape-regex) + '($|[^\w/-])') })
    }
  } | sort-by id
  {
    profile: $name
    profiles: ($r.profiles | columns | sort)
    problems: $r.failures
    mods: $mods
  }
}

def "nu-complete sims profiles" []: nothing -> list<string> {
  let dir = sims-config | get dataDir | path expand | path join profiles
  if not ($dir | path exists) { return [] }
  glob $"($dir)/*.json" | each { path parse | get stem } | sort
}

# Turn mods on or off in profiles/<profile>.json (the JSON half of a profile; one
# defined only in Nix is edited by hand). `enable` adds them to `mods` and takes
# them out of `remove`; disabling does the opposite, and lists them in `remove`
# only when the profile has a base they could come from.
def profile-edit [profile: string, ids: list<string>, enable: bool, commit: bool]: nothing -> record {
  let data = sims-config | get dataDir | path expand
  let file = $data | path join profiles $"($profile).json"
  if not ($file | path exists) {
    error make {msg: $"no profiles/($profile).json: a profile defined in Nix is edited by hand"}
  }
  if ($ids | is-empty) { error make {msg: 'give one or more mod ids'} }
  let known = nu-complete sims mods
  let unknown = $ids | where $it not-in $known
  if ($unknown | is-not-empty) { error make {msg: $"not in the catalog: ($unknown | str join ', ')"} }

  let p = open $file
  let mods = $p.mods? | default []
  let remove = $p.remove? | default []
  let based = ($p.base? | default null) != null
  let new_mods = if $enable { $mods | append $ids | uniq | sort } else { $mods | where $it not-in $ids }
  let new_remove = if $enable { $remove | where $it not-in $ids } else if $based { $remove | append $ids | uniq | sort } else { $remove }
  let next = $p | upsert mods $new_mods
  # only write `remove` when there's one to keep or the file already has it
  let next = if 'remove' in ($p | columns) or ($new_remove | is-not-empty) { $next | upsert remove $new_remove } else { $next }
  let changed = $ids | where {|id|
    if $enable { $id not-in $mods or $id in $remove } else { $id in $mods or ($based and $id not-in $remove) }
  }
  let hash = if ($changed | is-empty) { null } else {
    $next | to json -i 2 | save -f $file
    if $commit {
      let verb = if $enable { 'enable' } else { 'disable' }
      commit-data $data [$file] $"sims: ($verb) ($changed | str join ', ') in ($profile)"
    }
  }
  {profile: $profile, enable: $enable, changed: $changed, commit: $hash}
}

# Add mods to a profile (profiles/<profile>.json). Writes right away.
export def "sims profile add" [
  profile: string@"nu-complete sims profiles" # the profile
  ...ids: string@"nu-complete sims mods"      # mod ids, creator/mod
  --commit                                    # commit the change
]: nothing -> record {
  profile-edit $profile $ids true $commit
}

# Take mods out of a profile (profiles/<profile>.json). Writes right away.
export def "sims profile remove" [
  profile: string@"nu-complete sims profiles" # the profile
  ...ids: string@"nu-complete sims mods"      # mod ids, creator/mod
  --commit                                    # commit the change
]: nothing -> record {
  profile-edit $profile $ids false $commit
}

# Serve the mod web UI on http://127.0.0.1:<port> until stopped (ctrl-c). The server
# (dashboard-ui's apps/sims/server.mjs) runs these `sims` commands for the page.
export def "sims web" [
  --port: int = 7341 # port on 127.0.0.1
  --root: path       # a local build to serve instead (dashboard-ui's dist-sims, after `npm run build-sims`)
] {
  let web = sims-config | get -o web
  # a local build is served by the server next to it, in the same checkout
  let site = if $root != null {
    let root = $root | path expand
    {root: $root, server: ($root | path join .. apps sims server.mjs | path expand)}
  } else if $web != null { $web } else {
    error make {msg: 'no web UI in the simsnix config: switch home with the sims4 feature, or pass --root'}
  }
  let node = $web.node? | default (which node | get -o 0.path) | default node
  print -e $"Sims mods: http://127.0.0.1:($port)/"
  with-env {SIMS_MODULE: $sims_module, SIMS_NU: $nu.current-exe, SIMS_WEB_ROOT: $site.root, PORT: ($port | into string)} {
    ^$node $site.server
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

# Add new index entries to archives.json, and where downloads were seen
# (`seen`: [{ sha256, file }]) to their `paths`
def save-index [data: path, index: list, added: list, seen: list] {
  let home = $env.HOME | str escape-regex
  let where_seen = $seen | group-by sha256 | transpose sha256 rows | reduce -f {} {|g, acc|
    $acc | insert $g.sha256 ($g.rows | each {|r| $r.file | str replace -r $"^($home)/" '~/' })
  }
  # a download that is also inside another archive is kept as a download
  let merged = $index | append $added | sort-by {|s| $s.in != null } | uniq-by sha256 | each {|s|
    $s | update paths ($s.paths | append ($where_seen | get -o $s.sha256 | default []) | uniq | sort)
  } | sort-by name
  $merged | to json -i 2 | save -f ($data | path join archives.json)
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
    print -e 'dry run: add --apply to unpack, index and store the new downloads'
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
  save-index $data $index $added ($hashed | each {|r| {sha256: $r.sha256, file: ($dir | path join $r.path)} })
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

# Adding downloads (the inbox). Each download becomes a version of a mod that installs
# from it: an archive's version lists the archive with its members, so `sims switch`
# unpacks them from the archive's blob, and a re-download of the same file is known by
# its hash.

def is-mod-file [p: string]: nothing -> bool { (ext-of $p) in $mod_exts }

# Every file a version installs from: [{ path, sha256 }], an archive's members by
# their path inside it
def version-entries [v: record]: nothing -> list<record> {
  $v.files | each {|f|
    if ($f.members? | default [] | is-empty) { [{path: $f.name, sha256: $f.sha256}] } else {
      $f.members | each {|m| {path: $m.path, sha256: $m.sha256} }
    }
  } | flatten
}

def strip-path [n: int, p: string]: nothing -> string {
  let parts = $p | path split
  if ($parts | length) <= $n { '' } else { $parts | skip $n | path join }
}

# The catalog mod a download is a version of: one with a version of exactly its mod
# files (`same` names that version), else the one with mostly the same file names as
# its newest version (at least half of each side's)
def match-download [mod_files: list<record>, catalog: list]: nothing -> any {
  let names = $mod_files | each {|f| $f.path | path basename | str lowercase } | uniq
  let hashes = $mod_files | get sha256 | uniq | sort
  let scored = $catalog | each {|c|
    let versions = $c.mod.versions? | default {} | transpose label v | each {|r|
      {label: $r.label, added: ($r.v.added? | default ''), entries: (version-entries $r.v | where {|e| is-mod-file $e.path })}
    }
    let same = $versions | where {|r| ($r.entries | get -o sha256 | default [] | uniq | sort) == $hashes } | get -o 0.label
    let newest = $versions | sort-by added label | last | get -o entries | default []
    let newest_names = $newest | each {|e| $e.path | path basename | str lowercase } | uniq
    {id: $c.id, same: $same, n: ($names | where $it in $newest_names | length), size: ($newest_names | length)}
  }
  let exact = $scored | where same != null | get -o 0
  if $exact != null { return {id: $exact.id, same: $exact.same} }
  let best = $scored | where n > 0 | sort-by n --reverse | get -o 0
  if $best != null and $best.n * 2 >= ($names | length) and $best.n * 2 >= $best.size {
    {id: $best.id, same: null}
  }
}

# How many leading folders of a download's members to drop so they land where the
# mod's newest version put its files (0 when nothing lines up)
def best-strip [mod_files: list<record>, mod: record]: nothing -> int {
  let newest = $mod.versions | values | sort-by {|v| $v.added? | default '' } | last
  let known = version-entries $newest | each {|e| $e.path | str lowercase }
  0..3 | each {|n|
    {n: $n, hits: ($mod_files | where {|f| (strip-path $n $f.path | str lowercase) in $known } | length)}
  } | reduce -f {n: 0, hits: 0} {|r, best| if $r.hits > $best.hits { $r } else { $best } } | get n
}

# Add downloads to the catalog. Dry run unless --apply.
#
# Each download (an archive, or a loose .package/.ts4script) is indexed and stored in
# the blob store like `sims sources index` does, then matched to a mod:
#   known        a version already has exactly its mod files: nothing to add
#   new version  mostly the same file names as a mod's newest version (or --mod names
#                an existing mod): added as a version of it, its members placed where
#                that version's files were
#   new mod      no match: a mod of its own (--mod sets its id, else unknown/<name>),
#                installed as laid out in the archive
# The version label is --version, else a version in the file name, else today. --pin
# installs the new version (sets the mod's `version`); a new mod is always pinned.
# --profile adds the mod to a profile.
export def "sims add" [
  ...paths: path            # downloads, or folders of them
  --mod (-m): string        # the mod's id (default: matched, or unknown/<name>)
  --version (-v): string    # version label
  --page: string            # download page, for a mod that has none
  --pin                     # install this version
  --profile (-p): string    # also add the mod to this profile
  --apply                   # store, index and write the catalog
  --commit                  # with --apply, commit the catalog files it changed
]: nothing -> table {
  if ($paths | is-empty) { error make {msg: 'give one or more downloads'} }
  let data = sims-config | get dataDir | path expand
  let files = $paths | each {|p|
    let p = $p | path expand
    if ($p | path type) == dir { glob $"($p)/**/*" --no-dir } else { [$p] }
  } | flatten | where {|f| (ext-of $f) in [...$archive_exts ...$mod_exts] } | uniq | sort
  if ($files | is-empty) { error make {msg: 'no archives or mod files in those paths'} }
  if ($files | length) > 1 and ($mod != null or $version != null) {
    error make {msg: '--mod and --version need a single download'}
  }
  if $profile != null and not ($data | path join profiles $"($profile).json" | path exists) {
    error make {msg: $"no profiles/($profile).json"}
  }
  let index = read-sources $data
  let catalog = read-catalog $data
  let today = date now | format date '%Y-%m-%d'
  let tmp = sims-config | get blobDir | path expand | path join tmp
  mkdir $tmp

  let plans = $files | each {|file|
    let sha = sha256-of $file
    let name = $file | path basename
    let indexed = $index | where sha256 == $sha
    let entries = if ($indexed | is-not-empty) { [] } else if (ext-of $name) in $mod_exts {
      let size = ls $file | get size.0 | into int
      [{sha256: $sha, name: $name, size: $size, paths: [], in: null, members: [{path: $name, size: $size, sha256: $sha}]}]
    } else { index-archive $file $sha $name null $tmp 0 }
    let entry = $indexed | append $entries | first
    let mod_files = $entry.members | where {|m| is-mod-file $m.path }
    let found = if ($mod_files | is-empty) { null } else { match-download $mod_files $catalog }
    let id = $mod | default $found.id? | default $"unknown/(split-version (without-ext $name) | get stem | slug | default -e mod)"
    let existing = $catalog | where id == $id | get -o 0.mod
    # an exact match only counts for the mod it was found in
    let same = if $found != null and $found.id == $id { $found.same } else { null }
    let label = $version | default (split-version (without-ext $name) | get version) | default $today
    let is_archive = (ext-of $name) in $archive_exts
    let file_entry = if $is_archive {
      {name: $name, sha256: $sha, members: ($entry.members | select path sha256)}
    } else { {name: $name, sha256: $sha} }
    let base = {
      download: $name
      sha256: $sha
      file: $file
      entries: $entries
      id: $id
      version: (if $same != null { $same } else { $label })
      files: ($mod_files | length)
    }
    if ($mod_files | is-empty) {
      $base | merge {change: 'no mod files', json: null, note: 'nothing to install in it'}
    } else if $same != null {
      let json = if $pin and $existing.version? != $same { $existing | upsert version $same } else { null }
      $base | merge {change: 'known', json: $json, note: $"same files as ($id) ($same)"}
    } else if $existing != null {
      if $label in ($existing.versions | columns) {
        $base | merge {change: 'label taken', json: null, note: $"($id) already has a version ($label): pass --version"}
      } else {
        let strip = if $is_archive { best-strip $mod_files $existing } else { 0 }
        let old_strip = $existing.stripComponents? | default 0
        let conflict = $is_archive and $old_strip != 0 and $old_strip != $strip
        let json = $existing | merge {
          downloadPage: ($existing.downloadPage? | default $page)
          tags: ($existing.tags? | default [] | append (if ($mod_files | any {|f| (ext-of $f.path) == ts4script }) { [script] } else { [] }) | uniq | sort)
          versions: ($existing.versions | insert $label {added: $today, files: [$file_entry]})
        } | upsert stripComponents (if $is_archive and not $conflict { $strip } else { $old_strip })
        let json = if $pin { $json | upsert version $label } else { $json }
        let pinned = $existing.version? | default '(newest)'
        $base | merge {
          change: (if $conflict { 'strip conflict' } else { 'new version' })
          json: (if $conflict { null } else { $json })
          note: (if $conflict { $"its members need stripComponents ($strip), the mod has ($old_strip)" } else if $pin { $"installs it \(was ($pinned)\)" } else { $"($id) stays at ($pinned): --pin to install this one" })
        }
      }
    } else {
      let json = {
        title: (without-ext $name)
        downloadPage: $page
        tags: (if ($mod_files | any {|f| (ext-of $f.path) == ts4script }) { [script] } else { [] })
        folder: null
        include: ['.*\.package' '.*\.ts4script']
        exclude: []
        stateFiles: []
        version: $label
        versions: {($label): {added: $today, files: [$file_entry]}}
      }
      $base | merge {change: 'new mod', json: $json, note: (if $profile == null { 'in no profile: --profile to add it' } else { $"added to ($profile)" })}
    }
  }
  rm -rf $tmp

  let summary = $plans | select download change id version files note
  if not $apply {
    print -e 'dry run: add --apply to store the downloads and write the catalog'
    return $summary
  }
  let root = blob-root
  mkdir $root
  for p in $plans {
    if not ($root | path join $p.sha256 | path exists) { store-blob $root $p.file $p.sha256 }
  }
  save-index $data $index ($plans | each {|p| $p.entries } | flatten) ($plans | each {|p| {sha256: $p.sha256, file: $p.file} })
  for p in ($plans | where json != null) {
    let file = $data | path join mods $"($p.id).json"
    mkdir ($file | path dirname)
    $p.json | to json -i 2 | save -f $file
  }
  let pfile = if $profile != null { $data | path join profiles $"($profile).json" }
  if $pfile != null {
    let ids = $plans | where change in ['known' 'new version' 'new mod'] | get id
    let prof = open $pfile
    $prof | update mods ($prof.mods | append $ids | uniq | sort) | to json -i 2 | save -f $pfile
  }
  if $commit {
    let written = $plans | where json != null | get id | uniq
    let touched = [($data | path join archives.json)]
      | append ($written | each {|id| $data | path join mods $"($id).json" })
      | append (if $pfile != null { [$pfile] } else { [] })
    let what = if ($written | is-empty) { $plans | get download | str join ', ' } else { $written | str join ', ' }
    let hash = commit-data $data $touched $"sims: add ($what)"
    print -e (if $hash == null { 'nothing to commit' } else { $"committed ($hash)" })
  }
  $summary
}

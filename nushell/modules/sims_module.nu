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
  for f in $remove { rm ($i.saves | path join $f.path) }
  # Drop folders the removal emptied, deepest first
  for d in (glob --no-file $"($i.saves)/**/*" | sort-by {|d| $d | str length } --reverse) {
    if (ls -a $d | is-empty) { rm $d }
  }
  let thumbs = $i.documents | path join localthumbcache.package
  if ($thumbs | path exists) { rm $thumbs }
  $plan | update backup $b.id | update applied true
}

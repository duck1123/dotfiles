#!/usr/bin/env bash
# Copy a folder from the NAS's /volume1 onto the RECOVERY_BACKUP USB drive,
# at the same relative path, and verify every copied file against a sha256
# manifest taken from the source, read back from the drive with the page
# cache dropped. The manifest stays on the drive as <folder>/.archive.sha256,
# so --verify can re-check the offline copy later. Deleting the source is
# currently left to Duck; a verified run prints the command.
#
# Runs on the NAS as root; `nur nas archive` installs it and starts it under
# systemd-run. Re-running after a failure resumes the copy and rebuilds the
# manifest. Every outcome is appended to ARCHIVE.log at the drive's root.
#
#   nas-archive [--dry-run] [--bwlimit=60M] <share>/<path>   copy and verify
#   nas-archive --verify <share>/<path>                      re-check a folder on the drive
#   nas-archive --adopt <share>/<path>                       manifest (and fix perms) for a
#                                                            folder moved some other way
set -euo pipefail

SRC_ROOT=${NAS_ARCHIVE_SRC_ROOT:-/volume1}
DRIVE_LABEL=RECOVERY_BACKUP
MANIFEST=.archive.sha256

mode=archive keep=0 dry=0 bwlimit=0
while [ $# -gt 0 ]; do
  case "$1" in
    --keep-source) keep=1 ;;
    # rsync's --bwlimit (e.g. 60M, 0 = unlimited). Heavy I/O has crashed
    # this NAS before.
    --bwlimit=*) bwlimit=${1#--bwlimit=} ;;
    --dry-run) dry=1 ;;
    --verify) mode=verify ;;
    --adopt) mode=adopt ;;
    --) shift; break ;;
    -*) echo "unknown flag: $1" >&2; exit 2 ;;
    *) break ;;
  esac
  shift
done
rel=${1:?usage: nas-archive [--keep-source|--dry-run|--verify|--adopt] <share>/<path>}
rel=${rel#/}
rel=${rel%/}
# At least <share>/<name>: never a whole share (a btrfs subvolume) or the root.
case "/$rel/" in
  */../* | */./* | *//*) echo "bad path: $rel" >&2; exit 2 ;;
esac
[[ $rel == */* ]] || { echo "path must be <share>/<folder>, got: $rel" >&2; exit 2; }

DST_ROOT=${NAS_ARCHIVE_DST_ROOT:-$(findmnt -rn -S "LABEL=$DRIVE_LABEL" -o TARGET | head -n1)}
[ -n "$DST_ROOT" ] || { echo "$DRIVE_LABEL isn't mounted" >&2; exit 1; }
src="$SRC_ROOT/$rel"
dst="$DST_ROOT/$rel"

log() { printf '%s\t%s\t%s\t%s\n' "$(date -Is)" "$1" "$rel" "${2:-}" >> "$DST_ROOT/ARCHIVE.log"; }
fail() { echo "FAILED: $*" >&2; log FAILED "$*"; exit 1; }
count_files() { find "$1" -type f ! -name "$MANIFEST" ! -name "$MANIFEST.tmp" | wc -l; }

# Check every file in $dst against $1, reading from disk, not the page cache.
verify_against() {
  sync
  echo 1 2> /dev/null > /proc/sys/vm/drop_caches || echo "warning: not root, page cache not dropped"
  (cd "$dst" && sha256sum --check --quiet --strict "$1")
}

# Hash every file under $1 (relative names, sorted) into $2.
hash_tree() {
  (cd "$1" && find . -type f ! -name "$MANIFEST" ! -name "$MANIFEST.tmp" -print0 | sort -z | xargs -0 -r sha256sum) > "$2"
}

case $mode in
  verify)
    [ -f "$dst/$MANIFEST" ] || fail "no $MANIFEST in $dst (use --adopt for folders moved without this tool)"
    echo "== verifying $dst against $MANIFEST ($(wc -l < "$dst/$MANIFEST") files)"
    verify_against "$dst/$MANIFEST" || fail "verify: checksum mismatch or missing file"
    extra=$(($(count_files "$dst") - $(wc -l < "$dst/$MANIFEST")))
    [ "$extra" -le 0 ] || echo "note: $extra file(s) on the drive aren't in the manifest"
    log VERIFIED "$(wc -l < "$dst/$MANIFEST") files"
    echo "OK"
    ;;

  adopt)
    [ ! -e "$src" ] || fail "adopt: $src still exists; archive it instead"
    [ -d "$dst" ] || fail "adopt: $dst doesn't exist"
    # UGOS File Manager leaves moved dirs as root:root 000 on this ext4 drive.
    # Give them the owner of the folder they're in, and 755 (per directory,
    # before find descends into it).
    owner=$(stat -c %u:%g "$(dirname "$dst")")
    find "$dst" -type d -perm 000 -exec chown "$owner" {} \; -exec chmod 755 {} \;
    if [ -n "$(find "$dst" -name 'cpOrMv_*.ug-tmp' -print -quit)" ]; then
      fail "adopt: $dst has UGOS temp files (an interrupted move); check it by hand"
    fi
    echo "== hashing $dst"
    hash_tree "$dst" "$dst/$MANIFEST.tmp"
    mv "$dst/$MANIFEST.tmp" "$dst/$MANIFEST"
    log ADOPTED "$(wc -l < "$dst/$MANIFEST") files, $(du -sh "$dst" | cut -f1)"
    echo "OK: $(wc -l < "$dst/$MANIFEST") files"
    ;;

  archive)
    [ -d "$src" ] || fail "$src isn't a directory"
    need=$(du -sB1 "$src" | cut -f1)
    have=$(df -B1 --output=avail "$DST_ROOT" | tail -n1)
    already=$( (du -sB1 "$dst" 2>/dev/null || echo 0) | cut -f1)
    margin=$((10 * 1024 ** 3))
    echo "source $(numfmt --to=iec "$need"), on drive already $(numfmt --to=iec "$already"), free $(numfmt --to=iec "$have")"
    [ $((need - already + margin)) -lt "$have" ] || fail "not enough space on $DRIVE_LABEL"

    # --delete mirrors $src: $dst belongs to this path, and anything else in
    # it (e.g. UGOS temp files from an interrupted move) would otherwise be
    # left unverified next to the archive.
    rsync_opts=(-aH --delete --filter="P $MANIFEST" --partial-dir=.rsync-partial --no-inc-recursive --bwlimit="$bwlimit")
    if [ "$dry" = 1 ]; then
      mkdir -p "$(dirname "$dst")"
      rsync "${rsync_opts[@]}" --dry-run --itemize-changes --stats "$src/" "$dst/" | tail -n 25
      echo "(dry run: nothing copied or deleted)"
      exit 0
    fi

    log STARTED "$(numfmt --to=iec "$need")"
    echo "== copying $src -> $dst"
    mkdir -p "$(dirname "$dst")"
    rsync "${rsync_opts[@]}" --info=progress2,stats1 "$src/" "$dst/" || fail "rsync exited $?"

    echo "== hashing source"
    hash_tree "$src" "$dst/$MANIFEST.tmp"
    files=$(wc -l < "$dst/$MANIFEST.tmp")
    [ "$(count_files "$dst")" -eq "$files" ] || fail "file count differs: source $files, drive $(count_files "$dst")"

    echo "== verifying the drive copy ($files files)"
    verify_against "$dst/$MANIFEST.tmp" || fail "checksum mismatch; source kept, re-run to retry"
    mv "$dst/$MANIFEST.tmp" "$dst/$MANIFEST"
    log VERIFIED "$files files, $(numfmt --to=iec "$need")"

    # Deleting the source is left to Duck for now: the run stops at a
    # verified copy and prints the command.
    # if [ "$keep" = 1 ]; then
    #   echo "OK: copied and verified; source kept (--keep-source)"
    # else
    #   echo "== deleting $src"
    #   rm -rf --one-file-system -- "$src"
    #   log ARCHIVED "$files files, $(numfmt --to=iec "$need") freed"
    #   echo "OK: archived $files files, freed $(numfmt --to=iec "$need")"
    # fi
    : "$keep"
    echo "OK: copied and verified $files files ($(numfmt --to=iec "$need")); source kept."
    printf 'To free the space: sudo rm -rf --one-file-system -- %q\n' "$src"
    ;;
esac

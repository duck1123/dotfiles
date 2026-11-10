# NAS (UGREEN DXP4800, UGOS)

The NAS at `192.168.0.124` runs UGOS, which is Debian 12 underneath. Its data volume is `/volume1`: btrfs, mounted with `ugacl`. Each shared folder is a separate btrfs subvolume. Every host mounts the shares over NFSv3 (`flake.lib.nas-mounts`), and the k8s apps do too (`nfsTargets.nas`). nasnix is a VM on the NAS and also reads `/volume1` directly over virtiofs.

SSH in as `duck`. Root is needed to manage the VM (libvirt) and to change anything outside the shares. `scp`/SFTP only sees the share area, so to copy files elsewhere use `ssh duck@192.168.0.124 'cat > /tmp/x' < file`.

## Share ACLs (ugacl)

UGOS manages share permissions with its own ACLs (`+` in `ls -l`; `getfacl` shows nothing). Over NFS, and to any process, the mode it reports depends on who is asking. root with no groups is told `000`, and so was duck over NFS while a share's permission list had an explicit `duck` entry.

- **Don't save share permissions in UGOS.** Saving rewrites the ACL on every file whose ACL differs from the share's. On 2026-11-07, adding an explicit `duck` entry made NFS report the whole library as `000` and refuse to open files.
- **UGOS reapplies every share's ACL at boot,** so stripping them (`chmod` over NFS removes a file's ACL) only lasts until the next reboot. Folders that aren't shares (`@kvm` etc.) are left alone.
- If files show `000` or "Permission denied" over NFS, look for `+` on the NAS first, and test by opening files: `find` and `stat` over NFS can show stale modes.

## virtiofs into nasnix

nasnix mounts the NAS's `/volume1` read-only at `/mnt/volume1` (`hosts/nasnix.nix`, virtiofs tag `volume1`). This bypasses UGOS's NFS server.

Because of the share ACLs, virtiofsd has to run as duck (uid 1000 with the NAS groups `admin`, `users`, `ughomeusers`), so UGOS reports files to it the way it does to duck. UGOS's own `/usr/lib/qemu/virtiofsd` needs root, so libvirt starts [`resources/nas/virtiofsd-duck`](../resources/nas/virtiofsd-duck) instead. That wrapper raises the open-file limit, drops to duck with `setpriv` while keeping only `CAP_DAC_READ_SEARCH`, and runs a static virtiofsd with `--sandbox=none --inode-file-handles=mandatory --readonly`.

The capability is what lets virtiofsd use file handles (`open_by_handle_at`) instead of keeping an open fd for every inode nasnix knows about. Without it, walking the 11 media shares held about 292k fds. Each held fd also kept a deleted file allocated, so deleting a show on the NAS freed nothing until nasnix forgot the inodes or restarted. To check, look for deleted files virtiofsd still holds: `ls -l /proc/$(pgrep -u duck virtiofsd)/fd | grep '(deleted)'`. A file nasnix actually has open still uses an fd, and still holds its space until it's closed. The capability also skips read-permission checks, which is why the wrapper passes `--readonly`. With `mandatory`, virtiofsd refuses to start if file handles don't work (`Refusing to use (mandatory) file handles`), rather than falling back to fds.

### What's on the NAS

A UGOS update can undo any of these. If `/mnt/volume1` breaks after an update, check them first.

| What | Where |
|---|---|
| Static virtiofsd (nixpkgs `pkgsStatic.virtiofsd`, 1.14) | `/volume1/@kvm/virtiofsd/virtiofsd` |
| Wrapper | `/volume1/@kvm/virtiofsd/virtiofsd-duck` (this repo's `resources/nas/virtiofsd-duck`) |
| VM definition: shared memory and the virtiofs device | libvirt domain `ce28a2d8-b04c-426f-9b61-795b9e737887` |
| AppArmor exception for libvirtd to run the wrapper | `/etc/apparmor.d/local/usr.sbin.libvirtd` |

### Setting it up again

As root on the NAS:

1. Install the binary and the wrapper. Build the binary with `nix build --print-out-paths nixpkgs#pkgsStatic.virtiofsd`, copy both files over (see SSH above), then:

   ```sh
   mkdir -p /volume1/@kvm/virtiofsd
   install -o root -g root -m 755 virtiofsd virtiofsd-duck /volume1/@kvm/virtiofsd/
   ```

2. Let libvirtd run the wrapper. Its AppArmor profile only allows the helpers it lists:

   ```sh
   echo '/volume1/@kvm/virtiofsd/virtiofsd-duck PUx,' >> /etc/apparmor.d/local/usr.sbin.libvirtd
   apparmor_parser -r /etc/apparmor.d/usr.sbin.libvirtd
   ```

3. Edit the VM with `TERM=xterm-256color virsh edit ce28a2d8-b04c-426f-9b61-795b9e737887`. The `TERM` is needed because the NAS has no kitty terminfo. Add this directly under `<domain>`:

   ```xml
   <memoryBacking>
     <source type='memfd'/>
     <access mode='shared'/>
   </memoryBacking>
   ```

   and this inside the existing `<devices>`:

   ```xml
   <filesystem type='mount' accessmode='passthrough'>
     <driver type='virtiofs' queue='1024'/>
     <binary path='/volume1/@kvm/virtiofsd/virtiofsd-duck'/>
     <source dir='/volume1'/>
     <target dir='volume1'/>
   </filesystem>
   ```

   libvirt 9.0 rejects `<cache mode='auto'/>`: it only accepts `none` or `always`. The wrapper sets the cache mode itself.

4. Shut the VM down fully and start it again. A reboot from inside nasnix keeps the old QEMU process.

   ```sh
   virsh shutdown ce28a2d8-b04c-426f-9b61-795b9e737887   # wait for "shut off" in `virsh list --all`
   virsh start ce28a2d8-b04c-426f-9b61-795b9e737887
   ps -eo user,args | grep '[v]irtiofsd'                # should run as duck
   grep CapEff /proc/$(pgrep -u duck virtiofsd)/status  # 0000000000000004 (dac_read_search only)
   ```

If the VM won't start, virtiofsd's log is under `/var/log/libvirt/qemu/`.

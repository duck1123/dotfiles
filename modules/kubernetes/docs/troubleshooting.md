# Troubleshooting Playbook

Incident-derived debugging notes for failure modes that have recurred, or are distinctive enough to be worth recognizing quickly next time. All node names (`nixmini`, `nasnix`, `edgenix`, `inspernix`, `powerspecnix`) refer to the cluster's physical hosts.

See [deployment-workflow.md](deployment-workflow.md) first if any fix below involves scaling or pausing an app — `kubectl scale` directly gets reverted by ArgoCD's `selfHeal`.

## Longhorn: SCSI Medium Error / `SQLITE_CORRUPT` / `mke2fs` I/O failure — usually not real corruption

**Symptom:** A pod using a single-replica (`numberOfReplicas: 1`) Longhorn volume gets stuck in a mount-retry loop. Host `dmesg`/`journalctl` on the node backing the volume shows repeated `critical medium error` / `Sense Key: Medium Error / Unrecovered read error` at specific fixed sectors on the underlying `/dev/sdX`. The app itself reports corruption (`SQLITE_CORRUPT: database disk image is malformed`, `mke2fs: Input/output error`). Longhorn's own volume CR may still report `robustness: healthy` throughout — the corruption is invisible at the Longhorn CR level, only visible in host kernel logs.

This has happened at least twice (`stashapp-config`, then `tdarr-tdarr-config`, both on `nixmini`/`/dev/sdb`) with identical symptoms and an identical fix, so treat it as a recurring class of issue on this cluster rather than a one-off.

**Do not** let the CSI mount-retry loop keep running while you investigate — each retry's partial `mke2fs` attempt actively overwrites more blocks, turning a false alarm into real data loss.

**Fix:**

1. Stop the retry loop via the Nix-source scale-to-0 method (see [deployment-workflow.md](deployment-workflow.md) — not raw `kubectl scale`).
2. Once the pod/VolumeAttachment is gone, the volume auto-detaches (`status.state: detached`).
3. Reattach manually for inspection, *without* a consuming pod, using Longhorn's attachment-ticket mechanism (patching `spec.nodeID` directly on `volumes.longhorn.io` gets silently reverted — Longhorn now brokers attach/detach exclusively through `volumeattachments.longhorn.io` tickets):
   ```sh
   kubectl patch volumeattachments.longhorn.io -n longhorn-system <volname> --type=merge \
     -p '{"spec":{"attachmentTickets":{"recovery-inspect":{"id":"recovery-inspect","type":"longhorn-api","nodeID":"<node>","parameters":{"disableFrontend":"false"}}}}}'
   ```
   Release it afterward by merge-patching that specific key to `null` (an empty-object patch is a no-op under RFC 7386 semantics and won't clear existing keys):
   ```sh
   kubectl patch volumeattachments.longhorn.io -n longhorn-system <volname> --type=merge \
     -p '{"spec":{"attachmentTickets":{"recovery-inspect":null}}}'
   ```
4. Get a raw, privileged shell on the node backing the volume: `kubectl debug node/<node> --profile=sysadmin -- sleep 3600` — use `--profile=sysadmin`, *not* the default profile (the default isn't privileged enough for raw block device access and fails with `Operation not permitted` even as uid 0). Host binaries aren't on `PATH` under `chroot /host`; find them via `find /nix/store -maxdepth 1 -iname '*e2fsprogs*'` or under `/run/current-system/sw/bin/`.
5. `e2fsck -fn /dev/longhorn/<volname>` (forced, **read-only** — no writes) first. A clean pass through all 5 phases (maybe with a trivial free-block/inode count mismatch, normal after an unclean shutdown) confirms it's not real corruption.
6. Only then `e2fsck -fy` to fix safely, mount read-only, and spot-check (`ls`/`du`, or `sqlite3 <db> "PRAGMA integrity_check;"` for a sqlite-backed app) before trusting it.
7. Release the ticket, scale back up via the Nix-source route, hard-refresh the Application for immediate sync.

**Why:** Best working theory — some transient event (observed once following a stuck Longhorn live engine-image upgrade retry storm, see below; the second occurrence had no known trigger) leaves the SCSI target / Longhorn engine-replica session state inconsistent, causing spurious Medium Errors for specific LBAs without actual on-disk damage. A full detach (tears down the iSCSI session and engine/replica processes) + fresh attach rebuilds that state from scratch.

If this recurs a third time, it's worth investigating `/dev/sdb`/iSCSI on `nixmini` directly rather than treating each occurrence as isolated.

## Longhorn: node stuck `DiskPressure`/unschedulable despite real free disk space

**Symptom:** A Longhorn node/disk shows `Schedulable: False` (disk condition message cites `ScheduledTotal greater than ProvisionedLimit`) even though `df`/actual usage doesn't support it, and the k8s node itself is `Ready`.

**Likely cause:** A failed **live** (hot) Longhorn engine-image upgrade. The new engine controller refuses to hot-attach to a replica whose on-disk state is `dirty` (normal for an actively-serving replica) — `error="replica must be closed, cannot add in state: dirty"`, retried forever. Each failed retry leaves behind an orphaned "replacement" Replica CRD (`spec.active: false`) that reserves the volume's **full size again** in Longhorn's scheduling math, phantom-doubling reserved space for every volume stuck in this state.

**Diagnose:**
```sh
kubectl get volumes.longhorn.io -n longhorn-system -o json | jq '.items[] | select(.spec.image != .status.currentImage)'
```
Volumes where `spec.image != status.currentImage` are candidates. Cross-reference `nodes.longhorn.io <node>` disk `scheduledReplica` against `replicas.longhorn.io` grouped by `spec.nodeID` — a stuck volume shows *two* Replica objects on the same node/disk (the real one with `started=true`, plus an orphaned `started=false` one).

**Fix:** Revert `spec.image` back to match `status.currentImage` to cancel the retry loop:
```sh
kubectl patch volumes.longhorn.io -n longhorn-system <name> --type=merge -p '{"spec":{"image":"<status.currentImage value>"}}'
```
Longhorn garbage-collects the orphaned Replica CR within ~20s and the disk's `Schedulable` condition recovers. No downtime — the volume stays healthy/attached on the old engine image throughout.

If the upgrade is actually wanted, do it **offline** instead (scale the workload to 0 first via the Nix-source route so the volume cleanly detaches) rather than live/hot — that avoids the dirty-replica rejection entirely.

## iSCSI: mass pod failures across (almost) every app simultaneously

**Symptom:** Nearly every app in the cluster stuck `ContainerCreating`/`Init`/`CrashLoopBackOff` at once. Events show `FailedAttachVolume` (`DeadlineExceeded`), engine stuck in `starting`, CSI `NodeStageVolume` reporting "hasn't been attached yet."

**Root cause (seen once):** A corrupt host-level `iscsiadm` node record. `/etc/iscsi/nodes/iqn.../default` on one or more nodes contained a parameter (`node.session.conn_reopen_log_freq`) the host's currently-running `iscsiadm` doesn't recognize. `iscsiadm -m node` aborts entirely (exit 7) the instant it hits *any* single bad record, and Longhorn's engine frontend shells out to `iscsiadm` on every volume attach — so one corrupt file on a node breaks attachment for every volume scheduled there.

**Diagnose:** From a `longhorn-system` instance-manager pod on the suspect node (already has `/host` mounted + `hostPID`):
```sh
nsenter --mount=/host/proc/1/ns/mnt --net=/host/proc/1/ns/net iscsiadm -m node
```
An `Unknown parameter name` parse error here (not a network/disk problem) is the signature of this bug.

**Fix:** `sed -i '/conn_reopen_log_freq/d'` the affected record file(s) (via the same `nsenter` shell), or `rm -rf` fully-orphaned record dirs for already-deleted volumes. Safe and surgical — Longhorn regenerates records on next login. This directory isn't Nix-managed (it's `iscsiadm` runtime state, not declared by `services.openiscsi` in dotfiles), so the fix persists across `nixos-rebuild`/`nur switch`.

**Why it can recur:** `openiscsi` isn't version-pinned in dotfiles, so it drifts with flake bumps; a future nixpkgs bump could ship a build that writes this parameter again. Not something that happens on every deploy, but worth a quick recheck after a nixpkgs/flake bump that touches a node's generation.

## Gluetun: `Unhealthy`/`0/1 Ready` with HTTP 500 readiness probe, even though the VPN works fine

**Symptom:** `gluetun` stuck not-Ready for hours. Readiness probe fails (`HTTP probe failed with statuscode: 500`), but the control-server API (`/v1/vpn/status` → `running`, `/v1/publicip/ip` → correct exit IP) and actual proxied HTTP traffic both work the whole time. Cascades: any app gated behind gluetun's proxy (e.g. `slskd`'s `wait-for-gluetun` init container polling gluetun's Service on port 8888) hangs forever waiting for it to go Ready.

**Cause:** Gluetun's periodic health check pings `1.1.1.1`/`8.8.8.8` over ICMP through the tunnel. Some Mullvad WireGuard exit servers silently drop/rate-limit ICMP for abuse prevention — the tunnel and proxy are completely fine, but gluetun treats the ICMP failure as fatal.

**Diagnose:** `curl`/`nc` the health endpoint body from inside the pod (`http://127.0.0.1:9999/`). If it cites ICMP echo timeouts specifically (as opposed to DNS/HTTP dial failures, which look different and mean something else, e.g. a real cold-start issue), it's this.

**Fix:** `kubectl delete pod -n gluetun <pod>` — the Deployment recreates it, gluetun reconnects (often to a different Mullvad server), and the ICMP check usually passes on the new connection. Recheck any app gated behind it afterward; they typically self-resolve once gluetun goes Ready without needing their own restart.

## Self-hosted binary cache / nix-csi build failures

See [nix-csi-and-binary-cache.md](nix-csi-and-binary-cache.md) — covers the pinned `nix-csi` flake input, the self-hosted Attic Nix cache (currently backed by Garage; RustFS was the original backend, now disabled but still referenced by a couple of unmigrated apps), and how failures in either cascade into `nix-csi`/build failures in ways that aren't obvious from the nix-csi side alone.

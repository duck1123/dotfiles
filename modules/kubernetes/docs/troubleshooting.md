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

## Longhorn: single volume stuck `detaching`/`faulted` forever — stale `tgtd` iSCSI target blocks re-attach

**Symptom:** One specific PVC's pod stuck `ContainerCreating` indefinitely (unlike the mass-failure case above, this doesn't spread to other apps). `kubectl get volumes.longhorn.io <name>` cycles `detaching`/`faulted` → `attaching`/`unknown` → back to `detaching`/`faulted` every ~10-20s, never settling. The volume's replica CR reports `currentState: running` the whole time (the data itself is fine) — only the engine CR flaps, `currentState: starting` → `error` → `starting`, with `errorMsg: exit status 1` and a `generation` counter in the thousands from retrying for hours. The Volume CR's `lastAutoSalvagedAt` may show Longhorn already tried and failed to self-heal.

Seen taking down `garage`'s `garage-meta` PVC (2Gi, on `nasnix`), which cascades into Attic/nix-csi build failures since Garage backs the self-hosted binary cache — see [nix-csi-and-binary-cache.md](nix-csi-and-binary-cache.md).

**Root cause:** `tgtd` (the iSCSI target daemon Longhorn's engine shells out to via `tgtadm`, running inside the node's `instance-manager` pod and shared across every engine on that node) is left holding a half-torn-down target from an earlier crash — the controller LUN (LUN 0) still exists but the data LUN (LUN 1) is already gone. Every new engine attempt tries to clean up the stale target *before* creating a fresh one, calls `tgtadm --op delete --mode logicalunit --tid <N> --lun 1`, gets `tgtadm: can't find the logical unit` (exit 22) because LUN 1 is already gone, and aborts the whole frontend-init step instead of proceeding past it — so the engine process exits 1 and Longhorn retries from scratch, forever, without ever getting a working device.

**Diagnose:** The real error is buried among routine `Creating volume controller`/`Starting with replicas` lines from every retry — filter those out:
```sh
kubectl logs -n longhorn-system <instance-manager-pod-on-that-node> --since=90s | grep -v level=info
```
Look for `failed to init frontend: ... failed to delete target ... tgtadm: can't find the logical unit`. Cross-check the stale target directly:
```sh
kubectl exec -n longhorn-system <instance-manager-pod> -- tgtadm --lld iscsi --op show --mode target
```
The volume's target (`iqn.2019-10.io.longhorn:<volume-name>`) shows only `LUN: 0` (the controller LUN) and no `ACL information: ALL` line — a healthy target always has both.

**Fix:** Manual `tgtadm` cleanup (delete connection, then LUN, then target, in that order) is unreliable once a target is in this state — deleting the target itself reliably fails with `tgtadm: this target is still active` even after its connection/nexus is already gone. The fix that actually works is a full restart of that node's `instance-manager` pod, which restarts `tgtd` from scratch:
```sh
kubectl delete pod -n longhorn-system <instance-manager-pod-on-that-node>
```
Longhorn recreates it automatically (same name). Confirm with `kubectl get volumes.longhorn.io <name>` → `state: attached`, `robustness: healthy`.

**Caveat:** That instance-manager hosts *every* engine/replica scheduled on that node, not just the stuck one — deleting it briefly disrupts every other volume there too, not just a targeted fix. They re-attach automatically once the new instance-manager pod starts; confirmed safe in practice (`garage-data`, on the same node, round-tripped through `attached`/`healthy` with no data loss).

**Why it can recur:** Nothing here is pinned to a specific trigger — any abrupt engine kill mid-teardown (OOM, node pressure, a race during a previous recovery attempt) can leave `tgtd` in this half-deleted state. If it recurs often for the same volume, worth checking whether something keeps SIGKILLing the engine process rather than treating each occurrence as isolated.

## Gluetun: `Unhealthy`/`0/1 Ready` with HTTP 500 readiness probe, even though the VPN works fine

**Symptom:** `gluetun` stuck not-Ready for hours. Readiness probe fails (`HTTP probe failed with statuscode: 500`), but the control-server API (`/v1/vpn/status` → `running`, `/v1/publicip/ip` → correct exit IP) and actual proxied HTTP traffic both work the whole time. Cascades: any app gated behind gluetun's proxy (e.g. `slskd`'s `wait-for-gluetun` init container polling gluetun's Service on port 8888) hangs forever waiting for it to go Ready.

**Cause:** Gluetun's periodic health check pings `1.1.1.1`/`8.8.8.8` over ICMP through the tunnel. Some Mullvad WireGuard exit servers silently drop/rate-limit ICMP for abuse prevention — the tunnel and proxy are completely fine, but gluetun treats the ICMP failure as fatal.

**Diagnose:** `curl`/`nc` the health endpoint body from inside the pod (`http://127.0.0.1:9999/`). If it cites ICMP echo timeouts specifically (as opposed to DNS/HTTP dial failures, which look different and mean something else, e.g. a real cold-start issue), it's this.

**Fix:** `kubectl delete pod -n gluetun <pod>` — the Deployment recreates it, gluetun reconnects (often to a different Mullvad server), and the ICMP check usually passes on the new connection. Recheck any app gated behind it afterward; they typically self-resolve once gluetun goes Ready without needing their own restart.

## Self-hosted binary cache / nix-csi build failures

See [nix-csi-and-binary-cache.md](nix-csi-and-binary-cache.md) — covers the pinned `nix-csi` flake input, the self-hosted Attic Nix cache (currently backed by Garage; RustFS was the original backend, now disabled but still referenced by a couple of unmigrated apps), and how failures in either cascade into `nix-csi`/build failures in ways that aren't obvious from the nix-csi side alone.

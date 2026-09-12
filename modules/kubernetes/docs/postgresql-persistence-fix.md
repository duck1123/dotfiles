# PostgreSQL Persistence Fix

## Problem
PostgreSQL was using `emptyDir` volumes instead of PersistentVolumeClaims, causing data loss on pod restarts or node updates.

## Solution
1. **Enable persistence in Helm values** - Updated `applications/postgresql/default.nix` to explicitly enable persistence
2. **Regenerate manifests** - Run `nur switch` to apply changes
3. **Migrate existing data** - If data exists, back it up before applying changes

## Backup Strategy

### Automated Backups
Consider implementing automated PostgreSQL backups using one of:
- **Velero** - Cluster-level backup solution
- **Longhorn snapshots** - If using Longhorn storage
- **CronJob with pg_dump** - Simple scheduled backups

### Manual Backup
```bash
# Backup all databases
kubectl exec -n postgresql postgresql-0 -- pg_dumpall -U postgres > postgres-backup-$(date +%Y%m%d).sql

# Backup specific database
kubectl exec -n postgresql postgresql-0 -- pg_dump -U sonarr sonarr > sonarr-backup-$(date +%Y%m%d).sql
```

## Prevention
- Always verify PVCs exist: `kubectl get pvc -n postgresql`
- Check StatefulSet uses `volumeClaimTemplates`, not `emptyDir` for data
- Set up automated backups before major updates
- Test restore procedures regularly

# Databricks CLI
export extern "databricks" []

export extern "databricks account" []
export extern "databricks alerts" []
export extern "databricks alerts-legacy" []
export extern "databricks apps" []
export extern "databricks api" []
export extern "databricks artifact-allowlists" []
export extern "databricks auth" []

export extern "databricks bundle" []

export extern "databricks catalogs" []
export extern "databricks catalogs create" []
export extern "databricks catalogs delete" []
export extern "databricks catalogs get" [
name # The name of the catalog.
]

export def --wrapped "databricks catalogs list" [
  --include-browse
  --output = "json"
  --help
  ...rest
] {
  let response = ^databricks catalogs list --output $output ...$rest

  if $output == "json" and not $help {
    $response | from json
  } else {
    $response
  }
}

export def "nu-complete databricks catalogs name" [] {
  (databricks catalogs list --include-browse).name
}

export extern "databricks catalogs update" []

export extern "databricks clean-room-assets" []
export extern "databricks clean-room-task-runs" []
export extern "databricks clean-rooms" []
export extern "databricks cluster-policies" []
export extern "databricks clusters" []
export extern "databricks completion" []
export extern "databricks configure" []
export extern "databricks connections" []
export extern "databricks consumer-fulfillments" []
export extern "databricks consumer-installations" []
export extern "databricks consumer-listings" []
export extern "databricks consumer-personalization-requests" []
export extern "databricks consumer-providers" []
export extern "databricks credentials" []
export extern "databricks current-user" []

export extern "databricks data-sources" []
export extern "databricks dashboards" []

export extern "databricks experiments" []
export extern "databricks external-locations" []

export extern "databricks fs" []

export extern "databricks functions" []
export extern "databricks functions create" []
export extern "databricks functions delete" []
export extern "databricks functions get" []

export extern "databricks functions list" [
  catalogName: string@"nu-complete databricks catalogs name"
  schemaName: string@"nu-complete databricks schema names"
]
export extern "databricks functions update" []

export extern "databricks git-credentials" []
export extern "databricks global-init-scripts" []
export extern "databricks grants" []
export extern "databricks groups" []

export extern "databricks help" []

export extern "databricks instance-pools" []
export extern "databricks instance-profiles" []
export extern "databricks ip-access-lists" []


export extern "databricks jobs" []

export extern "databricks labs" []
export extern "databricks lakeview" []
export extern "databricks lakeview create" []
export extern "databricks lakeview create-schedule" []
export extern "databricks lakeview delete-schedule" []
export extern "databricks lakeview delete-subscription" []
export extern "databricks lakeview get" []
export extern "databricks lakeview get-published" []
export extern "databricks lakeview get-schedule" []
export extern "databricks lakeview get-subscription" []
export extern "databricks lakeview list" []
export extern "databricks lakeview list-schedules" []
export extern "databricks lakeview list-subscriptions" []
export extern "databricks lakeview migrate" []
export extern "databricks lakeview publish" []
export extern "databricks lakeview trash" []
export extern "databricks lakeview unpublish" []
export extern "databricks lakeview update" []
export extern "databricks lakeview update-schedule" []
export extern "databricks libraries" []
export extern "databricks libraries all-cluster-statuses" []
export extern "databricks libraries cluster-status" []
export extern "databricks libraries install" []
export extern "databricks libraries uninstal" []

export extern "databricks metastores" []
export extern "databricks metastores assign" []
export extern "databricks metastores create" []
export extern "databricks metastores current" []
export extern "databricks metastores delete" []
export extern "databricks metastores get" []
export extern "databricks metastores list" []
export extern "databricks metastores summary" []
export extern "databricks metastores unassign" []
export extern "databricks metastores update" []
export extern "databricks metastores update-assignment" []


export extern "databricks model-registry" []
export extern "databricks model-versions" []

export extern "databricks notification-destinations" []

export extern "databricks online-tables" []

export extern "databricks permissions" []
export extern "databricks pipelines" []
export extern "databricks policy-compliance-for-clusters" []
export extern "databricks policy-compliance-for-jobs" []
export extern "databricks policy-families" []
export extern "databricks provider-exchange-filters" []
export extern "databricks provider-exchanges" []
export extern "databricks provider-files" []
export extern "databricks provider-listings" []
export extern "databricks provider-personalization-requests" []
export extern "databricks provider-provider-analytics-dashboards" []
export extern "databricks provider-providers" []
export extern "databricks providers" []


export extern "databricks quality-monitors" []
export extern "databricks queries" []
export extern "databricks queries-legacy" []
export extern "databricks query-history" []


export extern "databricks recipient-activation" []
export extern "databricks recipients" []
export extern "databricks registered-models" []
export extern "databricks repos" []
export extern "databricks resource-quotas" []

export extern "databricks schemas" []
export extern "databricks schemas create" []
export extern "databricks schemas delete" []
export extern "databricks schemas get" []

export def --wrapped "databricks schemas list" [
  catalogName: string@"nu-complete databricks catalogs name"
  ...rest
] {
  ^databricks schemas list $catalogName -o json ...$rest | from json
}

export def "nu-complete databricks schema names" [
  context
] {
  let words = $context | str trim | split row ' '
  let catalogName = $words | last
  (databricks schemas list $catalogName).name
}

export extern "databricks secrets" []
export extern "databricks serving-endpoints" []
export extern "databricks service-principals" []
export extern "databricks settings" []
export extern "databricks shares" []
export extern "databricks storage-credentials" []
export extern "databricks sync" []
export extern "databricks system-schemas" []

export extern "databricks table-constraints" []
export extern "databricks tables" []
export extern "databricks temporary-table-credentials" []
export extern "databricks token-management" []
export extern "databricks tokens" []

export extern "databricks users" []

export extern "databricks vector-search-endpoints" []
export extern "databricks vector-search-indexes" []
export extern "databricks version" []
export extern "databricks volumes" []

export extern "databricks warehouses" []
export extern "databricks warehouses create" []
export extern "databricks warehouses delete" []
export extern "databricks warehouses edit" []
export extern "databricks warehouses get" []
export extern "databricks warehouses get-permission-levels" []
export extern "databricks warehouses get-permissions" []
export extern "databricks warehouses get-workspace-warehouse-config" []

export def "databricks warehouses list" [] {
  ^databricks warehouses list -o json | from json
}

export extern "databricks warehouses set-permissions" []
export extern "databricks warehouses set-workspace-warehouse-contig" []
export extern "databricks warehouses start" []
export extern "databricks warehouses stop" []
export extern "databricks warehouses update-permissions" []

export extern "databricks workspace" []
export extern "databricks workspace delete" []
export extern "databricks workspace export" []
export extern "databricks workspace export-dir" []
export extern "databricks workspace get-permission-levels" []
export extern "databricks workspace get-permissions" []
export extern "databricks workspace get-status" []
export extern "databricks workspace import" []
export extern "databricks workspace import-dir" []
export extern "databricks workspace list" []
export extern "databricks workspace mkdirs" []
export extern "databricks workspace set-permissions" []
export extern "databricks workspace update-permissions" []


export extern "databricks workspace-bindings" []
export extern "databricks workspace-conf" []

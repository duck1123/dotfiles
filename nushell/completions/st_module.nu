const devAirflow = "https://airflow.local/airflow/"
const defaultDataset = ''
const defaultWorkspace = ''

export alias pbicli = npx @powerbi-cli/powerbi-cli

# TODO: move the following to a different module

# Convert data to edn
export def "to edn" []: any -> string {
  to json
    | jet --from=json --keywordize
}

# Take data as a honeysql data and output as sql
export def "to sql" [] {
  to json
    | jet --from=json
    | clojure -M -e "(require '[honey.sql :as sql]) (prn (sql/format (read-string (slurp *in*))))"
}

## Parsers

# Parse a mssql result into structured data
def "mssql split result" [] {
  lines | drop 2 | each { split column "|" | first }
}

# Get the names of running worker pods
def "_st airflow workers running names" []: nothing -> list<string> {
  (st airflow workers | where {|$row| $row.status == 'Running'}).name
}

def "_st devspace vars row parse" [
  row # row from devspace vars list
] {
  ($row | split column '|').0
    | {
      ($in.column1 | str trim):
      ($in.column2 | str trim)
    }
}

## Completers

export def "nu-completer st airflow extracts list" [
  context: string
] {
  let words = $context | str trim | split row --regex '\s+'
  let source = $words | drop 1 | last
  let table = $words | last
  st airflow extracts list $source $table
}

# Get the names of worker pods
def "nu-completer st airflow workers logs" [] {
  _st airflow workers running names
}

# completer for powerbi workspaces
export def "nu-complete st pbi workspace ids" []: nothing -> table<value: string, description: string> {
  let workspaces = st pbi workspace list | select id name
  (($workspaces | rename value description)
    ++ ($workspaces | rename description value))
}

# completer for powerbi datasets where the workspaceId is in context
export def "nu-complete st pbi dataset ids" [
  context: string # The completion context
]: nothing -> table<value: string, description: string> {
  # print $"Context: `($context)`"
  let words = $context | str trim | split row ' '
  let workspaceId = $words | last
  # print $"Using workspace id ($workspaceId)"
  st pbi dataset list $workspaceId
    | select id name
    | rename value description
}

# completer for powerbi reports where the workspaceId is in context
export def "nu-complete st pbi report ids" [
  context: string # The completion context
]: nothing -> table<value: string, description: string> {
  let words = $context | split row ' '
  let workspaceId = $words | last
  # print $"Using workspace id ($workspaceId)"
  st pbi report list $workspaceId
    | select id name
    | rename value description
}

# Source name completer
def "nu-completer st sources names" [] {
  st sources list
}

def "nu-completer st pbi dw-table name" [
  context: string # The completion context
]: nothing -> table<value: string, description: string> {
  let words = $context | split words
  let source = $words | last
  st sources dw-tables names $source
}

export def "nu-completer st sources table names" [
  context: string
] {
  let words = $context | str trim | split row ' '
  let source = $words | last
  st sources table names $source
}

## Commands

# Produce a dags report as structured data
export def "st airflow dags report" [] {
  st airflow scheduler exec "airflow dags report -o json"
    | from json
}

# List extract files for a source table
export def "st airflow extracts list" [
  source: string@"nu-completer st sources names" # The source to use
  table: string@"nu-completer st sources table names"
] {
  st airflow scheduler exec $"ls /opt/project/st-output/($source)/($table)/"
    | lines
    | filter { str ends-with '.csv.gz' }
}

# Downloads an extract file
export def "st airflow extracts download" [
  source: string@"nu-completer st sources names" # The source to use
  table: string@"nu-completer st sources table names"
  filename: string@"nu-completer st airflow extracts list"
] {
  let name = st airflow scheduler name
  kubectl cp $"($name):/opt/project/st-output/($source)/($table)/($filename)" $filename
}

# Open a shell on the scheduler pod
export def "st airflow scheduler attach" [] {
  st airflow scheduler name | ka $in
}

# Execute a command on the scheduler pod
export def "st airflow scheduler exec" [
  command: string # The command to execute
] {
  let name = st airflow scheduler name
  $command | xargs kubectl exec $name -c scheduler --
}

# Get info about scheduler container
export def "st airflow scheduler name" []: nothing -> string {
  # This assumes that there is exactly 1 running schefuler
  (kgp -l component=scheduler | where status == 'Running').0.name
}

# Fetch airflow dag info as a table
export def "st airflow dags list" []: nothing -> list<any> {
  st airflow scheduler exec "airflow dags list --output json"
    | from json
}

# Fetch list of jobs
export def "st airflow jobs" [
  limit = 20 # The number of records to fetch
] {
  let name = st airflow scheduler name
  st airflow scheduler exec $"airflow dags list-jobs --limit ($limit) -o json"
    | from json
}

# Fetch failing airflow jobs
export def "st airflow jobs failed" [
  limit = 20 # The number of records to fetch
] {
  let state = 'failed'
  let name = st airflow scheduler name
  st airflow scheduler exec $"airflow dags list-jobs --limit ($limit) --state ($state) -o json"
    | from json
}

# Get list of worker pods
export def "st airflow workers" [] {
  kgp -l component=worker
}

# Get the logs for a worker
export def "st airflow workers logs" [
  name: string@"nu-completer st airflow workers logs" # The name of a pod
] {
  kl $name
}

# Get the names of worker pods
export def "st airflow workers names" [] {
  (st airflow workers).name
}

# List all audit-files
export def "st audit-files list" [] {
  let query = "SELECT * FROM st.audit_table_files"
  st db query $query
}

# Build the project
export def "st build" [] {
  bb build
}

# Get a list of all aliases defined
export def "st clojure deps aliases" [] {
  (open deps.edn).aliases
    | columns
}

# Fix all formatting issues
export def "st code format" [] {
  bb format
}

# Run the tests
export def "st code test" [] {
  bb ci-all
}

# Run the integration tests
export def "st code test integration" [] {
  bb test-integration
}

# Get Current config
export def "st config" [] {
  # print --stderr "Reading st config"
  open $"($env.VST_HOME)/vst.edn"
}

# get the st-db db config
export def "st db config" [] {
  (st config).st_db
}

# Execute a query against the st-db database
export def "st db exec" [
  query: string # The query to execute
] {
  let config = st db config
  st exec $config $query
}

# Test if st-db database exists
export def "st db exists" [] {
  let config = (st db config)
  let query = $"IF EXISTS \(SELECT 1 FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = '($config.dbname)') PRINT 'true' ELSE PRINT 'false'";
  st db exec $query
}

# Run a query against the st-db database and parse results
export def "st db query" [
  query: string # The query to execute
] {
  let config = st db config
  st exec $config $query
    | mssql split result
}

# Setup Single Truth DB
export def "st db setup" [
  --plan # Dry run?
] {
  let filename = mktemp -t st-db-setup.XXX
  let action = if ($plan) { "plan-setup-st-db" } else { "run-setup-st-db" }
  ./vst.sh $action -O $filename
  open $filename | from edn
}

# Select all data about data mart jobs
export def "st db tables dm job list" [] {
  st db query "SELECT * FROM st_dm_job"
}

# Select all data about dw jobs
export def "st db tables dw job list" [] {
  st db query "SELECT * FROM st_dw_job"
}

# Select all data about dw tables
export def "st db tables dw list" [] {
  st db query "SELECT * FROM dw_tables"
}

# Select all data about dw locks
export def "st db tables dw locks list" [] {
  st db query "SELECT * FROM dw_tables_locks"
}

# Select all data about dw resonstructs
export def "st db tables dw reconstruct list" [] {
  st db query "SELECT * FROM dw_reconstruct"
}

# Get the list of st-db tables
export def "st db tables list" [] {
  let query = "SELECT * FROM INFORMATION_SCHEMA.TABLES"
  st db exec $query
    | mssql split result
    | rename catalog schema name type
}

# Select all data about source ddl
export def "st db tables source ddl list" [] {
  st db query "SELECT * FROM source_ddl"
}

# Start devspace dev session
export def "st dev" [] {
  devspace dev
}

# Build the devimage
export def "st devimage build" [] {
  devspace run devimage-build
}

# Run a command against the devimage
export def "st devimage exec" [
  cmd: string
] {
  let name = st devimage name
  $cmd | xargs kubectl exec $name -c single-truth --
}

# Get the name of the devimage
export def "st devimage name" []: nothing -> string {
  (kgp -l app.kubernetes.in/name=single-truth -l devspace.sh/replaced=true).0.name
}

# Read devspace config
export def "st devspace config" [] {
  open devspace.yaml
}

# list all devspace pipelines
export def "st devspace piplines list" [] {
  let config = st devspace config
  $config.pipelines | columns
}

# List all vars defined in devspace
export def "st devspace vars list" [] {
  devspace list vars
    | lines
    | drop 1
    | skip 3
    | each {|row| _st devspace vars row parse $row }
    | reduce {|a b| $a | merge $b}
}

# Get the dw database config
export def "st dw config" [] {
  let config = st config
  $config.dw
}

# List all currency formats in the dw
export def "st dw currencies list" [] {
  st dw tables dim_currency_format list
    | lines
    | drop 2
    | split column '|'
    | rename code locale fmt sign is_base_currency
}

# Load currencies into the data warehouse
export def "st dw currencies setup" [
  --plan # Dry run?
] {
  let action: string = if $plan { "plan-load-currencies" } else { "run-load-currencies" }
  let filename: string = mktemp -t st-dw-currencies-setup.XXX
  ./vst.sh $action -O $filename
  open $filename | from edn
}

# List all DW jobs
export def "st dw jobs list" [] {
  let query = "SELECT * FROM st.st_dw_job"
  st db query $query
}

# run a query against a dw database
export def "st dw query" [
  query: string # The query to run
] {
  let config = st dw config
  st exec $config $query
}

# Read the security config
export def "st dw security config" [] {
  # print --stderr "Reading dw security config"
  let config = st dw config
  $config.security-config-path | open
}

# Find objects in dw config that are not referenced in the security config
export def "st dw security diff" [] {
  let $dwNames = st dw warehouse names
  let $securityNames = st dw security tables
  $dwNames
    | reduce --fold [] {|it acc|
        if ($securityNames | find $it | is-empty) {
          $acc | append $it
        } else {
          $acc
        }
      }
    | uniq
}

# Fetch DW tables in security config
export def "st dw security tables" []: nothing -> list<string> {
  let config = st dw security config
  ($config.roles.dw_tables | flatten).dw_table | uniq
}

# Setup Single Truth DB
export def "st dw setup" [
  --plan # Dry Run
] {
  let action = if $plan { "plan-setup-dw" } else { "run-setup-dw" }
  let filename = mktemp -t st-dw-setup.XXX
  ./vst.sh $action -O $filename
  open $filename | from edn
}

# List all records in currency formats table
export def "st dw tables dim_currency_format list" [] {
  let tableName = "dw.dim_currency_format"
  let query = $"SELECT * FROM ($tableName)"
  st dw query $query
}

# list all DW tables in schema
export def "st dw tables list" []: nothing -> table<catalog: string, schema: string, name: string, type: string> {
  let query = "SELECT * FROM INFORMATION_SCHEMA.TABLES"
  st dw query $"($query)"
    | mssql split result
    | rename catalog schema name type
}

# Get data for all dw columns
export def "st dw warehouse columns" [] {
  (st dw warehouse config).dimensions
    | each {|d|
        $d.attributes
          | each { { dimension: $d.tn ...$in } }
      }
    | flatten
    | sort-by cn
}

# Get data for all dw columns (with source and table names)
export def "st dw warehouse columns full" [] {
  (st dw warehouse config).dimensions
    | reduce --fold [] {|d acc|
        ($acc ++
          ($d.sources
            | reduce --fold [] {|source acc2|
                (
                  $acc2 ++
                  ($d.attributes
                    | each {|a| {
                          source: $source
                          tn: $d.tn
                          ...$a
                        }
                      })
                )
              }
          )
        )
      }
}

# Read the data warehouse config
export def "st dw warehouse config" [] {
  print --stderr  "Reading dw warehouse config"
  let config = st dw config
  $config.config-path | open
}

# Get all the data marts in the dw config
export def "st dw warehouse data-marts" [] {
  (st dw warehouse config).data_marts
}

# Get all the data mart names in the dw config
export def "st dw warehouse data-marts names" []: nothing -> list<string> {
  (st dw warehouse data-marts).tn
}

# Get all the dimensions in the dw config
export def "st dw warehouse dimensions" [] {
  (st dw warehouse config).dimensions
}

# Get all the dimensions in the dw config
export def "st dw warehouse dimensions get" [
  dimension: string@"st dw warehouse dimensions names"
] {
  let dimensions = st dw warehouse dimensions
  $dimensions
    | filter { $in.tn == $dimension }
    | first
}

# Get all the dimension names in the dw config
export def "st dw warehouse dimensions names" [] {
  (st dw warehouse dimensions).tn
}

# Get all the facts in the dw config
export def "st dw warehouse facts" [] {
  (st dw warehouse config).facts
}

# Get all the fact names in the dw config
export def "st dw warehouse facts names" [] {
  (st dw warehouse facts).tn
}

# Get the list of objects in the data warehouse
export def "st dw warehouse names" [] {
  let marts = st dw warehouse data-marts names
  let dimensions = st dw warehouse dimensions names
  let facts = st dw warehouse facts names
  $marts ++ $dimensions ++ $facts
}

# Get all defined earthly tasks
export def "st earthly tasks" [] {
  earthly ls
    | lines
    | each {|target| { $target: (bb get-task-args --target $target) } }
    | reduce {|o acc| $acc | merge $o }
    | sort
}

# Execute a query against a datasource config
export def "st exec" [
  config: record # The database config
  query: string  # The query to run
] {
  print --stderr $"sqlcmd -h-1 -W -S ($config.host) -U ($config.user) -P \"($config.pswd)\" -d ($config.dbname) -Q \"($query)\""
  sqlcmd -h-1 -W -s '|' -S $config.host -U $config.user -P $"($config.pswd)" -d $config.dbname -Q $"($query)"
}

# Get sample data for an extract job
export def "st extract job data" [
  source: string@"nu-completer st sources names" # The source to use
] {
  {
    source: $source
    extract_id: 1
    date_inserted: (date now |  format date "%Y-%m-%d %H:%M:%S")
    data_imported: 0
    imported_staging_id: '00000000-0000-0000-0000-000000000000'
    source_provider: 'mssql'
  }
}

# Fetch a list of extract jobs
export def "st extract job list" [] {
  let query = "SELECT * FROM st.st_extract_job"
  st db exec $query
    | mssql split result
    | rename source extract_id date_inserted data_imported imported_staging_id source_provider
}

alias "st jobs extract list" = st extract job list

# Set the status of an extract job to non-imported
export def "st jobs extract reset-imported" [
  extractId: int # The Extract id
] {
  let tableName = "st.st_extract_job"
  let query = $"UPDATE ($tableName) SET data_imported = 0 WHERE data_imported = 1 AND extract_id = ($extractId)"
  st db exec $query
}

# List all staging jobs
export def "st jobs staging list" [] {
  let query = "SELECT * FROM st.st_staging_job"
  st db exec $query
    | mssql split result
    | rename staging_id source is_processing is_complete job_start_time job_end_time load_job_id
}

# Mark a staging job as not complete
export def "st jobs staging reset-complete" [
  stagingId: int # The id of the staging job to change
] {
  print --stderr $"Updating completion state for ($stagingId)"
  let query = $"UPDATE st.st_staging_job SET is_complete = 0 WHERE staging_id = '($stagingId)' AND is_complete = 1"
  st db exec $query
}

export def "st pbi dataset datasource list" [
  workspace: string@"nu-complete st pbi workspace ids" = $defaultWorkspace # The workspace this dataset belongs to
  dataset: string@"nu-complete st pbi dataset ids" = $defaultDataset # The dataset to show
] {
  pbicli dataset datasource list -w $workspace -d $dataset | from json
}

# List all powerbi datasets in the workspace
export def "st pbi dataset list" [
  workspace: string@"nu-complete st pbi workspace ids" = $defaultWorkspace # The workspace to use
]: nothing -> table<id: string, name: string, webUrl: string, addRowsAPIEnabled: bool, configuredBy: string, isRefreshable: bool, isEffectiveIdentityRequired: bool, isEffectiveIdentityRolesRequired: bool, isOnPremGatewayRequired: bool, targetStorageMode: string, createdDate: string, createReportEmbedURL: string, qnaEmbedURL: string, upstreamDatasets: list<any>, users: list<any>, queryScaleOutSettings: record<autoSyncReadOnlyReplicas: bool, maxReadOnlyReplicas: int>> {
  pbicli dataset list -w $workspace
    | from json
}

# Get parameter list for a dataset
export def "st pbi dataset parameter list" [
  workspace: string@"nu-complete st pbi workspace ids" # The workspace this dataset belongs to
  dataset: string@"nu-complete st pbi dataset ids" # The dataset to show
] {
  pbicli dataset parameter list -w $workspace -d $dataset
}

# Run a query against a pbi dataset
export def "st pbi dataset query" [
  workspace: string@"nu-complete st pbi workspace ids" # The workspace this dataset belongs to
  dataset: string@"nu-complete st pbi dataset ids" # The dataset to show
  query: string
] {
  pbicli dataset query -w $workspace -d $dataset --dax $query
}

# Show a powerbi dataset
export def "st pbi dataset show" [
  workspace: string@"nu-complete st pbi workspace ids" # The workspace this dataset belongs to
  dataset: string@"nu-complete st pbi dataset ids" # The dataset to show
] {
  pbicli dataset show -w $workspace -d $dataset
    | from json
}

# Authenticate the cli against powerbi
export def "st pbi login" [
  --tenant: string = "TENANT_NAME" # The PowerBI tenant domain to authenticate against
] {
  pbicli login -t $tenant
}

# Get the PBI model for the given source and DW table
#
# This assumes that the powerbi models have been generated
export def "st pbi model config" [
  source: string@"nu-completer st sources names" # The source to use
  table: string@"nu-completer st pbi dw-table name" # The DW Table
] {
  open $"target/powerbi/($source)__($table).json"
}

# Get all powerbi tables for the given source and dw table
export def "st pbi model tables" [
  source: string@"nu-completer st sources names" # The source to use
  table: string@"nu-completer st pbi dw-table name" # The DW Table
]: nothing -> list<any> {
  let config = st pbi model config $source $table
  $config.model.tables
}

# Get the names of all the pbi tables for a given source and dw table
export def "st pbi model tables names" [
  source: string@"nu-completer st sources names" # The source to use
  table: string@"nu-completer st pbi dw-table name" # The DW Table
]: nothing -> list<string> {
  (st pbi model tables $source $table).name
}

# List all powerbi reports
export def "st pbi report list" [
  workspace: string@"nu-complete st pbi workspace ids" # The workspace to operate on
]: nothing -> table<id: string, reportType: string, name: string, webUrl: string, embedUrl: string, isFromPbix: bool, isOwnedByMe: bool, datasetId: string, datasetWorkspaceId: string, users: list<any>, subscriptions: list<any>, reportFlags: int> {
  pbicli report list -w $workspace | from json
}

# Show a PowerBI workspace
export def "st pbi report show" [
  workspace: string@"nu-complete st pbi workspace ids" # The workspace to operate on
  report: string@"nu-complete st pbi report ids" # The report to show
] {
  pbicli report show -w $workspace -r $report | from json
}

# List all PowerBI workspaces
export def "st pbi workspace list" []: nothing -> table<id: string, isReadOnly: bool, isOnDedicatedCalacity: bool, capacityId: string, defaultDatasetStorageFormat: string, type: string, name: string> {
  pbicli workspace list | from json
}

# Run a query against the staging database
export def "st query staging" [
  query: string # A SQL query to run on the database
] {
  let config = st staging config
  st exec $config $query
}

# Run ICSW check
export def "st query icsw" [] {
  let whse = 'DBEL'
  let transdt = '2025-02-01'
  let query = $"SELECT whse, MAX\(transdt) AS max_transdt_ICSW_STG
    FROM \(
      SELECT whse, transdt
      FROM staging.stg.stg_pro2inv_icsw WITH \(NOLOCK)
      WHERE st_row_current_ = 1 AND whse = '($whse)' and transdt > '($transdt)'
    ) AS FilteredData
    GROUP BY whse;"
  st query staging $query
}

# Get the shire catalog 001
export def "st shire catalog1" [] {
  (open .pipelines/samples/data/shire-catalog-001.csv)
    | each {|$row|
      $row.'item|quantity|cost'
        | split column '|'
        | { item: $in.column1.0 quantity: $in.column2.0 cost: $in.column3.0 }
    }
}

# Get the shire catalog 002
export def "st shire catalog2" [] {
  (open .pipelines/samples/data/shire-catalog-002.csv)
    | each {|$row|
      $row.'item|quantity|cost'
        | split column '|'
        | { item: $in.column1.0 quantity: $in.column2.0 cost: $in.column3.0 }
    }
}

# List all tasks defined in readme
export def "st runme tasks list" [] {
  runme list --json
    | from json
}

# Get All aliases for this source
export def "st sources aliases" [
  source: string@"nu-completer st sources names" # The source to use
]: nothing -> list<string> {
  let config = st sources config $source
  $config.aliases.tn
}

# Get all columns belonging to a source
export def "st sources columns" [
  source: string@"nu-completer st sources names" # The source to use
] {
  # print --stderr $"Getting columns for ($source)"
  let config = st sources config $source

  if $config != null {
    $config.tables
      | each {|table| $table.columns | each { { table: $table.tn ...$in } } }
      | flatten
  } else {
    []
  }
}

# Get the names of all columns for a source
export def "st sources columns names" [
  source: string@"nu-completer st sources names" # The source to use
] {
  let col = (st sources config $source).tables.columns | flatten;
  $col.dcn? | sort
}

# Get the information of all columns for all sources
export def "st sources columns3" []: nothing -> table<dcn: string, tn: string, source: string, role: string> {
  let sources = st sources list
  $sources
    | each {|source|
        let tables = (st sources config $source).tables
        $tables
          | each {|table|
              $table.columns
                | each {|col|
                    if $col.dcn? != null {
                      { dcn: $col.dcn tn: $table.tn source: $source role: $col.role? }
                    } else {
                      null
                    }
                  }
            }
          | flatten
      }
    | flatten
    | sort-by dcn tn source
}

def "_st sources columns4 attribute2" [
  dimension
  source
  attribute
  config
  targetColumns
]: nothing -> record<dw_column_name: string, dw_table: string, source: string, source_table: string, logic: string, cn: string, ea_notes: string, canada_notes: string> {
  let aliasColumns = $config
    | default [] aliases
    | get aliases
    | each {|aliasR|
        print $"Processing alias: ($aliasR)"
        let targetColumns = $aliasR
          | default [] columns
          | get columns
          | filter { $in.dcn? == $attribute.cn }
        if ($targetColumns | length) > 0 {
          let dcn = $attribute.cn
          let tn = $dimension.tn
          let source = (if ($aliasR | columns | find source | length) > 0 { $aliasR.source } else { $source })
          let table = $aliasR.alias_of
          let cn = $targetColumns.0.cn
          {
            dw_column_name: $dcn
            dw_table: $tn
            source: $source
            source_table: $table
            logic: ""
            cn: $cn
            ea_notes: ""
            canada_notes: ""
          }
        } else {
          null
        }
      }
    | flatten
  if ($aliasColumns | length) > 0 {
    $aliasColumns | first
  } else {
    null
  }
}

def "_st sources columns4 attribute" [
  dimension
  source
  attribute
]: nothing -> record<dw_column_name: string, dw_table: string, source: string, source_table: string, logic: string, cn: string, ea_notes: string, canada_notes: string> {
  let config = st sources config $source
  let columns = st sources columns $source
  let targetColumns = $columns | filter { $in.dcn? == $attribute.cn }

  if ($targetColumns | length) > 0 {
    let dcn = $attribute.cn
    let tn = $dimension.tn
    let table = $targetColumns.0.table
    let cn = $targetColumns.0.cn
    # let logic = (if ($attribute.trf? | is-not-empty) { $attribute.trf? } else { " " })
    let logic = "none"
    let ea_notes = ""
    let canada_notes = ""
    print --stderr $"    - Targeting column: ($targetColumns)"
    let record = {
      dw_column_name: $dcn
      dw_table: $tn
      dw_column: (if $attribute.name? { $attribute.name? } else { " " })
      data_type: $attribute.dtk
      source: $source
      source_table: $table
      logic: $logic
      cn: $cn
      # ea_notes: $ea_notes
      # canada_notes: $canada_notes
    }
    print --stderr $"      - Record A: ($record)"
    $record
  } else {
    let record = _st sources columns4 attribute2 $dimension $source $attribute $config $targetColumns
    print --stderr $"      - Record B: ($record)"
    $record
  }
}

def "_st sources columns4 source" [dimension attribute] {
  # print --stderr $"  - Processing attribute: ($attribute)"
  print --stderr $"  - Processing attribute: ($attribute.cn)"
  let lines = $dimension.sources
    | each { _st sources columns4 attribute $dimension $in $attribute }
    | flatten

  if ($lines | length) > 0 {
    $lines
  } else {
    [
      {
        dw_column_name: $attribute.cn
        dw_table: $dimension.tn
        dw_column: (if $attribute.name? { $attribute.name? } else { " " })
        data_type: $attribute.dtk
        source: null
        source_table: null
        logic: null
        # table: null
        cn: null
        # ea_notes: null
        # canada_notes: null
      }
    ]
  }
}

export def "_st sources columns4 dimension" [dimension] {
  print --stderr $"- Processing dimension: ($dimension.tn)"
  let attributes = $dimension.attributes
  let dates = if ($dimension | columns | find dates | length) > 0 { $dimension.dates } else { [] }
  ($attributes ++ $dates)
    | each { _st sources columns4 source $dimension $in }
    | flatten
}

# Get the information from the warehouses combined with the sources
export def "st sources columns4" [] {
  let dimensions = st dw warehouse dimensions
  # let dimensions = []
  # let facts = st dw warehouse facts
  let facts = []
  ($dimensions ++ $facts)
    | each { _st sources columns4 dimension $in }
    | flatten
}

# List all the data in the st-output directory for this source
export def "st sources data ls" [
  source: string@"nu-completer st sources names" # The source to use
] {
  st airflow scheduler exec $"ls /mnt/st-output/($source)/"
}

# list all tables in source db
export def "st sources db tables list" [
  source: string@"nu-completer st sources names" # The source to use
] {
  let query = "SELECT * FROM INFORMATION_SCHEMA.TABLES"
  let config = st sources connection $source
  st dw query $"($query)"
    | mssql split result
    | rename catalog schema name type
}

# Perform an extract on the source
export def "st sources extract" [
  source: string@"nu-completer st sources names" # The source to use
  --plan # Dry run only?
] {
  let action = if ($plan) { "plan-extract" } else { "run-extract" }
  let filename = mktemp -t source-extract.XXX
  ./vst.sh $action --source $source -O $filename
  open $filename | from edn
}

# Get the config for a source
export def "st sources connection" [
  source: string@"nu-completer st sources names" # The source to use
] {
  let sources = (st config).sources
  let targetSources = $sources | filter { $in.source_name == $source}
  # print $"Found ($targetSources | length) for ($source)"

  if ($targetSources | length) > 0 {
    $targetSources | first
  } else {
    print --stderr "no connection"
    null
  }
}

# Get the config for a source
export def "st sources config" [
  source: string@"nu-completer st sources names" # The source to use
] {
  let config = st sources connection $source
  if $config != null {
    $config.config-path | open
  } else {
    print --stderr "no config"
    null
  }
}

# Get All DW Tables for this source
export def "st sources dw-tables" [
  source: string@"nu-completer st sources names" # The source to use
] {
  (st sources config $source).dw_tables
}

# Get the names of the dw tables belonging to this source
export def "st sources dw-tables names" [
  source: string@"nu-completer st sources names" # The source to use
] {
  (st sources dw-tables $source).dw_table
    | filter { $in.enable }
    | each { str replace '/' '_' }
}

# Execute a query against a source database
#
# This assumes the source is using mssql
export def "st sources exec" [
  source: string@"nu-completer st sources names" # The source to use
  query: string # The query to execute
] {
  let config = st sources connection $source
  st exec $config $query
}

# list all sources defined in vst config
export def "st sources list" []: nothing -> list<string> {
  bb list-sources | lines
}

# Get all roles applied to the tables for a source
export def "st sources roles" [
  source: string@"nu-completer st sources names" # The source to use
] {
  (st sources config $source).dw_tables
  | each {
      let hasRoles = ($in | columns | find roles | length) > 0
      if $hasRoles { $in.roles } else { null }
    }
  | flatten
  | uniq
}

# Get all source table names for a source
export def "st sources table names" [
  source: string@"nu-completer st sources names" # The source to use
] {
  (st sources config $source).tables.tn
}

# The names of all views associated with source
export def "st sources views names" [
  source: string@"nu-completer st sources names" # The source to use
] {
  (st sources config $source).views.tn
}

# st staging

# Get the config for the staging database
export def "st staging config" [] {
  (st config).staging
}

alias "st staging jobs list" = st jobs staging list
alias "st staging jobs set-complete" = st jobs staging set-complete
alias "st staging query" = st query staging

# List all tables in the staging schema
export def "st staging tables list" [] {
  let query = "SELECT * FROM INFORMATION_SCHEMA.TABLES"
  st staging query $"($query)"
    | mssql split result
    | rename catalog schema name type
}

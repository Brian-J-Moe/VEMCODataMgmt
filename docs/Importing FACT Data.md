# FACT Network Data Processing

## Overview

This workflow processes acoustic telemetry detection data from the FACT (Florida Acoustic Cooperative Telemetry) network. It imports raw CSV exports, applies standardized corrections, and maintains a consolidated database.

## Package Functions

The `VEMCODataMgmt` package provides six main functions for FACT data processing:

### Core Processing Functions

1. **`import_fact_csvs()`** - Import and consolidate multiple CSV files
2. **`process_fact_agencies()`** - Filter unwanted agencies and standardize names
3. **`apply_fact_corrections()`** - Apply station name, agency, and coordinate corrections
4. **`merge_fact_databases()`** - Merge new data with existing database, handling duplicates
5. **`validate_fact_database()`** - Quality control checks for common data issues
6. **`process_fact_workflow()`** - Complete end-to-end workflow (wrapper function)

## Directory Structure

```
Project_Root/
├── Reference Files/
│   ├── unwanted_agencies.csv
│   ├── agency_lookup.csv
│   ├── station_agency_reassign.csv
│   ├── station_name_corrections.csv
│   └── MASTER_RECEIVERS.csv
├── process_fact_data.R              # Main workflow script
└── [Database Directory]/
    ├── UPDATED_FACT_detections.RData
    └── [Validation outputs]
```

## Reference Files

All correction rules are stored in CSV files in the `Reference Files/` directory. This makes the workflow transparent, editable, and version-controllable.

### 1. unwanted_agencies.csv

Agencies to exclude from the database (typically non-FL researchers whose detections are false positives).

**Format:**
```csv
Agency
Allen Curry
Marc Trudel
...
```

**When to update:** Add agencies when you identify new sources of false detections.

### 2. agency_lookup.csv

Pattern-matching rules to standardize agency/PI names.

**Format:**
```csv
pattern,standardized
Barbieri,FWC-Barbieri
Reyier|Reiyer,NASA-Reyier
...
```

**Notes:**
- Patterns are regex-compatible (e.g., `|` for "or")
- More specific patterns should come first
- Case-sensitive matching

**When to update:** 
- New collaborators join FACT network
- Existing agencies change naming conventions
- You identify variations in PI names

### 3. station_agency_reassign.csv

Station-specific agency reassignments (for receivers that changed ownership).

**Format:**
```csv
Station.Name,Agency
5FNF,FWC-Young
SOUTH BEACH,UM-Macdonald
...
```

**When to update:** When receivers are transferred between research groups.

### 4. station_name_corrections.csv

Corrections for station name typos, standardizations, or changes.

**Format:**
```csv
old_name,new_name,Agency
LKABTT7,FKABTT7,
BTT31,BTT30,
24,24_RANGETEST,MMF-Pate
...
```

**Notes:**
- `Agency` column is optional (leave blank for global corrections)
- Agency-specific corrections only apply to that agency's data
- Corrections are applied in order

**When to update:**
- Station names change in the field
- Typos identified in FACT exports
- Standardization needed across agencies

### 5. MASTER_RECEIVERS.csv

Authoritative source for station coordinates. Updates all detections to use standardized coordinates.

**Format:**
```csv
Station.Name,Agency,Latitude,Longitude
GB1,FWC-Barbieri,26.54010,-81.9914
...
```

**Notes:**
- Matches on both station name AND agency
- Coordinates should be decimal degrees (7+ decimal places for accuracy)
- This is your source of truth for receiver locations

**When to update:**
- New receivers deployed
- Receiver locations change
- Coordinate corrections needed

## Quick Start

### 1. Set up reference files

Copy templates from `Reference_File_Templates/` to `Reference Files/` and customize:

```r
# Check which reference files exist
list.files("Reference Files")

# If starting fresh, copy templates
file.copy(
  from = list.files("Reference_File_Templates", full.names = TRUE),
  to = "Reference Files",
  overwrite = FALSE
)
```

### 2. Edit `process_fact_data.R`

Update the paths section:

```r
# Path to new FACT CSV exports
csv_dir <- "Z:/FACT/2025 FACT Data Exports/"

# Path to reference files
reference_dir <- "Reference Files"

# Path to existing database
existing_db <- "Z:/Database/UPDATED_FACT_detections.RData"

# Path for updated database
output_db <- "Z:/Database/UPDATED_FACT_detections.RData"
```

### 3. Run workflow

```r
source("process_fact_data.R")
```

Or run step-by-step for more control (see "Advanced Usage" below).

## Workflow Steps

The complete workflow executes these steps in order:

### Step 1: Import CSVs

Reads all CSV files from `csv_dir`, standardizes column names, removes release records, and consolidates into single data.table.

### Step 2: Process Agencies

- Filters out unwanted agencies (false positives)
- Standardizes agency names using pattern matching

### Step 3: Apply Corrections

Applied in this order:
1. Agency-specific pattern corrections (e.g., adding "-NASA" suffix)
2. Station-specific agency reassignments
3. Station name corrections
4. Coordinate updates from master receiver list

### Step 4: Merge Databases

- Loads existing database
- Handles timestamp precision mismatches (seconds vs minutes)
- Removes duplicates
- Combines datasets

### Step 5: Validate

Quality control checks:
- Stations with multiple coordinate pairs (coordinate drift)
- Duplicate station names (same name, different coordinates/agencies)
- Duplicate coordinates (multiple stations at same location)
- Summary statistics by station

### Step 6: Save

Saves updated database and validation results (if issues found).

## Advanced Usage

### Running individual functions

For more control or troubleshooting, run functions separately:

```r
library(data.table)
library(VEMCODataMgmt)

# Import
fact_raw <- import_fact_csvs(csv_dir = "Z:/FACT/2025 FACT Data Exports/")

# Process agencies
fact_filtered <- process_fact_agencies(
  fact_raw,
  unwanted_agencies_file = "Reference Files/unwanted_agencies.csv",
  agency_lookup_file = "Reference Files/agency_lookup.csv"
)

# Apply corrections
fact_corrected <- apply_fact_corrections(
  fact_filtered,
  station_agency_file = "Reference Files/station_agency_reassign.csv",
  station_name_file = "Reference Files/station_name_corrections.csv",
  master_receivers_file = "Reference Files/MASTER_RECEIVERS.csv"
)

# Merge
merge_result <- merge_fact_databases(
  fact_corrected,
  existing_rdata_file = "UPDATED_FACT_detections.RData"
)

fact_combined <- merge_result$combined_data

# Validate
validation <- validate_fact_database(fact_combined)

# Save
FACT_detections <- fact_combined
save(FACT_detections, file = "UPDATED_FACT_detections.RData")
```

### Using vectors instead of reference files

You can supply correction rules directly:

```r
fact_filtered <- process_fact_agencies(
  fact_raw,
  unwanted_agencies = c("Allen Curry", "Marc Trudel"),
  agency_lookup = data.table(
    pattern = c("Barbieri", "Reyier"),
    standardized = c("FWC-Barbieri", "NASA-Reyier")
  )
)
```

### Agency-specific pattern corrections

For complex station name rules:

```r
fact_corrected <- apply_fact_corrections(
  fact_filtered,
  agency_specific_corrections = list(
    NASA = list(pattern = "CS", suffix = "-NASA"),
    FWC = list(pattern = "FPCC", replacement = "FPLR")
  ),
  # ... other arguments
)
```

## Validation Output

When validation detects issues, it saves CSV files to the database directory:

- `station_summary_YYYY-MM-DD.csv` - Summary statistics for all stations
- `duplicate_stations_YYYY-MM-DD.csv` - Stations with same name, different coords/agencies
- `duplicate_coordinates_YYYY-MM-DD.csv` - Coordinate pairs shared by multiple stations
- `coordinate_drift_YYYY-MM-DD.csv` - Stations with multiple coordinate pairs

### Interpreting validation issues

**Duplicate station names:**
- Usually indicates receivers that changed hands
- Update `station_agency_reassign.csv` or `station_name_corrections.csv`

**Duplicate coordinates:**
- May indicate typos in station names
- Could be legitimate (multiple receivers at same location)
- Review and update `station_name_corrections.csv` if needed

**Coordinate drift:**
- Station coordinates changed over time in FACT exports
- Update `MASTER_RECEIVERS.csv` with authoritative coordinates
- Workflow will standardize all coordinates to master list

## Troubleshooting

### "No CSV files found"

Check that `csv_dir` path is correct and contains `.csv` files.

### "Required columns missing"

FACT export format may have changed. Check source CSV column names and update function arguments:

```r
fact_raw <- import_fact_csvs(
  csv_dir = csv_dir,
  datetime_col = "NEW_DATE_COLUMN_NAME",
  transmitter_col = "NEW_TAG_COLUMN_NAME"
  # ... etc
)
```

### "Reference file not found"

Ensure reference files exist in `Reference Files/` directory. Functions will skip corrections if files are missing (with a warning).

### Unexpected agency assignments

Check `agency_lookup.csv`:
- Patterns are matched in order
- More specific patterns should come first
- Patterns are case-sensitive and regex-compatible

### Coordinate problems persist after workflow

Update `MASTER_RECEIVERS.csv` with correct coordinates. The workflow uses this as the authoritative source and will overwrite any other coordinates for matching Station.Name + Agency combinations.

## Maintenance

### Regular updates

1. **After each FACT download:**
   - Run workflow to integrate new data
   - Review validation output
   - Update reference files if new issues identified

2. **When receivers change:**
   - Update `station_agency_reassign.csv`
   - Update `MASTER_RECEIVERS.csv` if coordinates change

3. **When collaborators join/leave:**
   - Update `agency_lookup.csv` with new patterns
   - Add to `unwanted_agencies.csv` if needed

### Version control

Keep reference files under version control (e.g., Git):

```bash
git add Reference\ Files/*.csv
git commit -m "Updated agency assignments for 2025 receivers"
```

This provides an audit trail of correction rules over time.

## Function Reference

Detailed documentation for each function is available via:

```r
?import_fact_csvs
?process_fact_agencies
?apply_fact_corrections
?merge_fact_databases
?validate_fact_database
?process_fact_workflow
```

## Contact

For questions or issues with the FACT data workflow, contact:
[Your name/email]

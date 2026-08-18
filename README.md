VEMCODataMgmt: Working with Passive Acoustic Data
================
Brian Moe
2026-08-18

- [Load the R package VEMCODataMgmt](#load-the-r-package-vemcodatamgmt)
- [Clean and Standardize new FACT
  data](#clean-and-standardize-new-fact-data)
  - [File path configuration for
    `VEMCODataMgmt`](#file-path-configuration-for-vemcodatamgmt)
  - [Execute FACT workflow](#execute-fact-workflow)
  - [Post Processing Quality Control
    Checks](#post-processing-quality-control-checks)
  - [Additional details](#additional-details)
- [Clean and Standardize new FWC Charlotte Harbor
  data](#clean-and-standardize-new-fwc-charlotte-harbor-data)
  - [Importing new data](#importing-new-data)
  - [Post processing and data
    summarizations](#post-processing-and-data-summarizations)
- [Merge new FACT data and new Charlotte Harbor data with existing
  database](#merge-new-fact-data-and-new-charlotte-harbor-data-with-existing-database)
- [Examples of summary statistics](#examples-of-summary-statistics)

This README guides the user through processing new passive acoustic
detection data. In it, we walk through cleaning and standardizing new
FACT network detections and merge them with the existing database. We
then do the same with newly downloaded FWC Charlotte Harbor detections
and merge them with the existing database. Once both new FACT and new
FWC data are merged with the existing database (FWC, FACT, and iTag), we
run a final quality control to ensure no repeat detections are present.

All correction rules are define in reference files stored within the
“Reference Files/” directory. If working in the “Acoustic Database R
Project”, this will already be set up correctly. While we standardize
the data and remove erroneous detections, additional quality control
measures such as removing single detections (unless obviously erroneous)
are not carried out at this stage.

Workflow:

1.  Install (if not already done) and load the package
    [`VEMCODataMgmt`](https://github.com/Brian-J-Moe/VEMCODataMgmt)
2.  Set directory paths (csv_dir and reference_dir)
3.  Clean and standardize new FACT data
    1.  Review validation report and update reference files
    2.  Rerun the newly cleaned and merged FACT data if reference files
        were updated
    3.  Post-processing quality control checks
4.  Clean and standardize newly downloaded FWC Charlotte Harbor
    detections
5.  Merge the new FACT data and new FWC Charlotte Harbor data with the
    existing database
6.  Quality control sanity checks

## Load the R package VEMCODataMgmt

``` r
# If VEMCODataMgmt is not already installed:
install.packages("pak") # skip if `pak` is already installed
pak::pak("Brian-J-Moe/VEMCODataMgmt") 

# Load the package
library(VEMCODataMgmt)
```

## Clean and Standardize new FACT data

The following code is used to clean and standardize the new FACT data.
In this step, we identify likely erroneous detections (e.g., detections
in Canadian waters) and remove any duplicate detections that the FACT
Network identifies as “new”. Given the recent push for iTag users to
transition to the FACT Network, it is likely that some portion of the
“new” detections are already in the database.

### File path configuration for `VEMCODataMgmt`

The below configurations assume that you are working in the “Acoustic
Database R Project” directory. If working in a different project which
does not house a “Reference Files/” or “Data/” subdirectory within the
main project directory, the full file path must be used to locate those
directories.

``` r
# Path to new FACT CSV exports folder (REQUIRED - changes with each download)
csv_dir <- "G:/Charlotte Harbor Common/Sawfish/Long Term_InternalTags/Data Received/FACT/April 2026 Dump"

# Path to reference files directory (contains correction look-up tables)
reference_dir <- "Reference Files"

# Path to existing FACT RData object
existing_db <- "Data/UPDATED_FACT_detections.RData"
```

*Note that when loading the “Data/UPDATED_FACT_detections.RData” data
file, the object that is loaded into the environment is named
`FACT_detections`. To manually load and inspect this object run:*

``` r
utils::data("UPDATED_FACT_detections")
FACT_detections
```

    #> Indices: <Station.Name>, <Agency__Station.Name>
    #>                   Date.Time Station.Name    Transmitter Latitude Longitude
    #>                      <POSc>       <char>         <char>    <num>     <num>
    #>      1: 2019-05-24 10:42:53 REDFISH PASS A69-1602-13721 26.55303 -82.19692
    #>      2: 2019-05-24 11:44:42 REDFISH PASS A69-1602-13721 26.55303 -82.19692
    #>      3: 2019-05-24 11:46:53 REDFISH PASS A69-1602-13721 26.55303 -82.19692
    #>      4: 2019-05-26 00:53:42 REDFISH PASS A69-1602-13721 26.55303 -82.19692
    #>      5: 2019-05-27 01:56:13 REDFISH PASS A69-1602-13721 26.55303 -82.19692
    #>     ---                                                                   
    #> 357221: 2024-02-21 08:37:40          RL7  A69-9001-7620 24.48320 -81.77808
    #> 357222: 2024-02-21 08:41:57          RL7  A69-9001-7620 24.48320 -81.77808
    #> 357223: 2024-02-21 08:51:19          RL7  A69-9001-7620 24.48320 -81.77808
    #> 357224: 2024-02-21 09:24:53          RL7  A69-9001-7620 24.48320 -81.77808
    #> 357225: 2024-02-21 09:36:02          RL7  A69-9001-7620 24.48320 -81.77808
    #>         Agency
    #>         <char>
    #>      1:    BTT
    #>      2:    BTT
    #>      3:    BTT
    #>      4:    BTT
    #>      5:    BTT
    #>     ---       
    #> 357221:    BTT
    #> 357222:    BTT
    #> 357223:    BTT
    #> 357224:    BTT
    #> 357225:    BTT

``` r
class(FACT_detections)
#> [1] "data.table" "data.frame"
```

*The column names seen above in the data object `FACT_detections` are
the standardized column names used by `VEMCODataMgmt` and do not
necessarily reflect those used by the FACT Network. This ensures smooth
and efficient merging of data regardless of if the FACT Network changes
their standard column names.*

### Execute FACT workflow

The function `VEMCODataMgmt::process_fact_workflow()` is a complete data
processing and cleaning function. To view the help file for details on
function arguments, run `?process_fact_workflow()`.

``` r
result <- VEMCODataMgmt::process_fact_workflow(
  csv_dir = csv_dir,
  reference_dir = reference_dir,
  existing_rdata = existing_db,
  output_rdata = NULL,
  datetime_col = "dateCollectedUTC",
  transmitter_col = "tagName",
  station_col = "station",
  lat_col = "decimalLatitude",
  lon_col = "decimalLongitude",
  poc_col = "contactPOC",
  pi_col = "contactPI",
  validate = TRUE,
  save_validation = TRUE,
  progress_bar = FALSE
) |> suppressWarnings()
#> 
#> ========================================
#> FACT DATA PROCESSING WORKFLOW
#> Started: 2026-08-18 11:17:11
#> ========================================
#> Importing FACT CSV files...
#> Found 12 CSV file(s)
#> Importing and consolidating...
#> Imported 172,626 total records
#> ✅ Import complete: 172,626 records
#> 
#> Processing agency assignments...
#> Filtering 18 unwanted agencies...
#>   Removed 1,689 records from unwanted agencies
#> Standardizing agency names (28 patterns)...
#>   ✅ Agency standardization complete
#> 
#> Applying station corrections...
#> Applying agency-specific corrections...
#>   ✅ Agency-specific corrections applied
#> Applying station name corrections (86 corrections)...
#>   ✅ Station name corrections complete
#> Applying station-specific agency reassignments (31 stations)...
#>   ✅ Agency reassignments complete
#> Updating coordinates from master receiver list (1288 stations)...
#>   ✅ Coordinate updates complete
#> 
#> Merging with existing database...
#> Removed 4 exact duplicate row(s) from incoming data
#> Loading existing database from: example_FACT_data.RData
#> Removed 2 exact duplicate row(s) from existing data
#> Existing database: 357,223 records
#> Incoming database: 170,933 records
#> Matching detections using exact timestamps...
#>   • Exact overlaps: 112,261
#> Checking unmatched detections for timestamp precision differences...
#>   • Precision-matched overlaps: 338
#> ✅ Merge complete:
#>    • Total records: 415,557
#>    • Genuinely new detections: 58,334
#>    • Existing detections updated: 112,599
#>        - exact timestamp: 112,261
#>        - minute-precision fallback: 338
#> Running validation checks...
#> Validating FACT database...
#>   • Total unique stations: 1144
#>   • Total station-agency combinations: 1144
#>   ✅ No duplicate station names found
#>   ✅ No duplicate coordinate pairs found
#>   ✅ No coordinate drift detected
#> 
#> ✅ Database validation complete - no issues found
#> 
#> Saving updated database...
#>   ✅ Saved: example_folder/Data/UPDATED_FACT_detections.RData
#> 
#> ========================================
#> WORKFLOW COMPLETE
#> Finished: 2026-08-18 11:17:18
#> Elapsed: 7.2 seconds
#> ========================================
```

<span style="font-size: 0.8em;">***The column names listed in the code
above (also set as the default names) reflect those used by the FACT
network as of the date this article was written. Be sure to check the
new data files to be imported and modify those column names if
needed.***</span>

<span style="font-size: 0.8em;">*Note: Recent FACT data dumps contain
“new” detections which have already been sent in previous dumps. Any
changes to previously uploaded FACT data (e.g., new list of PIs or POCs)
will cause the FACT database to treat them as `new` detections and
include them in the next data dump.*</span>

The output of `process_fact_workflow()` is a list object containing:

1.  The updated dataset of fact detections
2.  The newly added FACT detections
3.  Statistics pertaining to the merger of the new and existing datasets
4.  Results from the validation process

``` r
names(result)
#> [1] "all_detections" "new_detections" "merge_stats"    "validation"
```

When `output_rdata = NULL`, the updated FACT dataset is saved in the
root directory of the `existing_rdata` path under the data file name
`UPDATED_FACT_detections.RData`.

Running `process_fact_workflow()` automatically imports all `.csv` files
housed in the assigned `csv_dir` directory. Files are then combined and
scrutinized for duplicate detection data.

*NOTE: If `csv_dir` is housed in the shared drive, you’ll likely see a
series of the following warning:*

    Warning in file.info(file): cannot resolve owner of file 'FACT csv
    files/fwcsaw_matched_detections_2025.csv': No mapping between account names and
    security IDs was done

*This warning is an artifact of using a shared network drive and can be
disregarded.*

Within the subdirectory `Reference Files`, there are five `.csv` files
with which `process_fact_workflow()` uses as reference files to clean
and standardize the data. These files **must** remained named as follows
to prevent the `VEMCODataMgmt` functions from breaking.

1.  “unwanted_agencies.csv” contains a list of known PIs which
    consistently provide false detections (e.g., Canadian institutions
    or groups). All data associated with these PIs are removed
2.  “agency_look-up.csv” is a list of past PIs and their associated
    agencies. This is use for standardizing the names of the agencies
    owning each receiver. Not that some of these pairings may be
    outdated. This is primarily used for consistency with past data to
    prevent the same data points being associated with multiple
    agencies. These pairings can be changed, however, some PIs upload
    “new” data which we already have, primarily from iTag data. If you
    change the agency label associated with a specific PI, post
    processing of the newly updated FACT data will be required to ensure
    no duplicate detections are present (though the validation step in
    the function should streamline this).
3.  “station_agency_reassign.csv” serves a similar purpose
    “agency_look-up.csv” and expressly looks for stations in which PIs
    have changed, the listed order of PIs has changed, etc., and
    restores them to the canonical “Agency” label.
4.  “station_name_corrections.csv” cross-references the coordinate pairs
    and reassigns canonical station names to those with different names.
    This is particularly important for some groups who regularly switch
    spelling or capitalization of their stations. If changes are made by
    PIs to the station names in the FACT database, FACT, for whatever
    reason, seems to treat those data as “new” and includes them in the
    next data dump. Additionally, it seems to be common for other groups
    to rename the station when redeploying a receiver or deploying a new
    receiver in place of the existing receiver.
5.  “Master_RECEIVERS.csv” cross-references station names with their
    coordinate pairs, and returns any discrepancy to the canonical
    coordinates. Variations in coordinate pairs often present themselves
    as differences in the number of decimal points or slight changes to
    the exact coordinate positions upon redeployment.

Once the new FACT data is cleaned, standardized, and combined, the
function loads in the existing `FACT_detections` dataset currently
housed within the project. Before merging the newly imported data with
the existing data, a copy of the existing `FACT_detections` is saved
within the “Data/” subdirectory as `old_FACT_detections`. This offers a
fallback in the event something occurs when merging the two data sets
and ensures there is always an available “non-updated” copy of the FACT
data. It is also useful as a post-processing cross-reference tool. To
load and view the non-updated data following the completion of
`process_fact_workflow()`, run the console commands:

``` r
utils::data("old_FACT_detections")
View(old_FACT_detections)
```

Once the new and existing data are merged, the function removes
duplicate entries and conducts a validation check. In this validation
check, the function identifies stations with the same coordinates but
different names and/or assigned agency labels as well as stations with
the same name but different coordinates and/or agency labels. A
diagnostic table is automatically printed with function argument
`verbose = TRUE` (which is the default value).

Following the validation check, the function then saves the newly merged
`FACT_detections` in the “Data/” subfolder as
“UPDATED_FACT_detections.RData”. It also identifies all new
(non-duplicate) data from the imported FACT `.csv` files, places them in
a data object named `new_data`, and saves that object in the “Data/”
subdirectory under the file “new_FACT_detections.RData”. To view all
newly added data run the console commands:

``` r
utils::data("new_FACT_detections")
View(new_data)
```

Additionally, `process_fact_workflow()` updates the reference file
“MASTER_RECEIVERS.csv” with a new list of receiver names and their
corresponding `Latitude`, `Longitude`, and `Agency`. To view this file
in R run the console command
`View(data.table::fread("Reference Files/MASTER_RECEIVERS.csv"))`, or
open and view in excel. This allows easy identification of any new PIs
(the `Agency` listed will appear as the PI’s full name, followed by an
email address). New PIs for which the detections are erroneous can then
be added to the “unwanted_agencies.csv” reference file, and any new PIs
deemed to have legitimate detections can be added to the
“agency_look-up.csv” reference file and given a standardized `Agency`
label for future reference.

### Post Processing Quality Control Checks

`process_fact_workflow()` will not be able to identify new data quality
issues not previously defined in the reference files. As such, the
output dataset should be manually inspected for erroneous detections or
other errors/mistakes. Below are several helpful manual checks.

View the ranges of the `Latitude` and `Longitude` columns. This will
help identify false detections from obviously illogical locations (e.g.,
Canadian waters).

``` r

FACT_detections[, .(
  lat_range = range(Latitude),
  lon_range = range(Longitude)
)]
```

Check for new PIs that are not already in the reference files. FACT
formats the PI field as a single or list of individuals with their names
and contact emails. The easiest way to search through the data is to
identify data with the at symbol (@) within it’s ‘Agency’ text string.

``` r

# Check Agency column for new PIs
FACT_detections[grepl("@", Agency), Agency] |> unique()
```

Following any updates to the reference files, the new database can be
re-run through `process_fact_workflow()` to correct for any changes.

``` r

FACT_detections <- VEMCODataMgmt::process_fact_workflow(
  reference_dir = reference_dir,
  existing_rdata = output_rdata
)
```

In the above code, only two fields are required: `reference_dir` and
`existing_rdata`. When the `csv_dir` argument is missing, the function
only evaluates the dataset located by the `existing_rdata` file path.
Because the newly updated data was automatically saved to the specified
`output_rdata` path, the `existing_rdata` path in this run should be the
same as `output_rdata` from the previous run (or wherever the `.RData`
file of interest is stored), in this example it is
`"Data/UPDATED_FACT_detections.RData"`. The remaining function arguments
are not called when `csv_dir` is missing and can therefore be left out.
Unlike the output when merging new data as detailed above, when only
cleaning an existing database, the output is a single `data.table`
object as opposed to a list of objects.

### Additional details

`process_fact_workflow()` is a wrapper function which links together a
series of functions housed within `VEMCODataMgmt`, each having their own
help documentation which can be called with the console command
`?VEMCODataMgmt::name_of_function()`:

1.  `VEMCODataMgmt::import_fact_csv()`
2.  `VEMCODataMgmt::process_fact_agencies()`
3.  `VEMCODataMgmt::apply_fact_corrections()`
4.  `VEMCODataMgmt::merge_fact_databases()`
5.  `VEMCODataMgmt::validate_fact_database()`

These functions were build using the `data.table` R package. As such,
the resulting dataset outputs are formatted as `data.table` objects. To
save the `process_fact_workflow()` output data object as a `.csv` file,
use the function
`data.table::fwrite(result$fact_detections, file = "your_file_path.csv")`.

***All code and associated help files can be found in the GitHub
repository
[Brian-J-Moe/VEMCODataMgmt](https://github.com/Brian-J-Moe/VEMCODataMgmt)***

## Clean and Standardize new FWC Charlotte Harbor data

The following code is used to clean and standardize the new Charlotte
Harbor data. This step is far less arduous than cleaning FACT data and
is largely for ensuring formatting of the data is consistent with that
of the existing database. This produces three primary datasets:

1.  A “raw” database of ALL accumulated FWC Charlotte Harbor owned data
    (our tags, orphan tags, spatial reference and synchronizing tags,
    etc.).
2.  A cleaned and standardized database of all detections associated
    with our tagged animals (regardless of species)
3.  A database of detections associated with known tags deployed by
    FSU/NOAA

### Importing new data

Before importing and cleaning the new data, we’ll want to organize the
receiver logs into folders based on the date of download. This allows
for efficient cataloging and handling of data and the ability to easily
find download logs from specific dates (very useful for finding VPS
files needed by Innovasea). First, define the file paths to specific
directories and data locations. For this example, all folders with
necessary files have been moved into the “README Tutorial” folder to
more efficiently define the directory paths. If the folder of interest
in housed within the project root directory, only the folder name is
required. When calling folders outside the root directory, the full file
path is required.

``` r
##| eval: true

# Directory containing new VRL/CSV files 
vrl_dir <- "Receiver Logs"

# Directory for which date specific folders should be placed
out_dir <- "Logs by Date"

# If a folder has not yet been created, the following code creates it within the rood directory of the project or R session:
#dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
```

Given that receiver logs for a given year are all housed within a single
folder, we can define a date (`after_date`) for which the sorting
function only evaluates logs downloaded after that date.

``` r
##| eval: true

# Optional: Filter downloads after this date (NULL = process all files)
# Format: "YYYY-MM-DD"
after_date = after_date <- "2026-03-01"
```

Once both directories have been defined and an `after_date` set, use the
functions `sort_VRL_logs()` and `load_detections_by_dates()` to sort
newly downloaded logs into their appropriate folders and import the new
detections.

``` r
##| eval: true

# Sort newly downloaded receiver logs
sort_result <- sort_VRL_logs(vrl_dir, out_dir, after_date = after_date)

# Import new data
new_det_full <- load_detections_by_dates(out_dir, after_date = after_date, verbose = FALSE)
```

These data represent ALL transmitters detected including orphan tags,
time syncronization tags, and spatial reference tags. Using a list of
deployed tags, extract only those data.

``` r
##| eval: true


tagged_fish_file <- "Data/Acoustic Tagged Fish.csv"

tagging_data <- fread(tagged_fish_file)

# Standardize transmitter names
tagging_data[, Transmitter := as.character(Transmitter)] # ensures all IDs are character vectors
tagging_data[!is.na(Transmitter), Transmitter := trimws(Transmitter)] # Removes accidental whitespace (e.g., trailing blank spaces)


# Remove known deceased fish if desired
tags_to_remove <- c("A69-9001-54949", "A69-9001-60330", "A69-9001-54939")
tagging_data <- tagging_data[!Transmitter %in% tags_to_remove]


# Extract FWC transmitter data
new_det <- new_det_full[Transmitter %in% tagging_data$Transmitter]
```

### Post processing and data summarizations

The following example code demonstrates how to correct for a receiver
initialized with the wrong coordinates (this uses `data.table`
formatting).

``` r

new_det[`Station Name` == "Glover Bight Creek", `:=` (
  Latitude  = 26.54010,
  Longitude = -81.9914
)]
```

It’s also useful to generate summary statistics for all new data,
specific arrays, or individual fish. The following examples walk through
those processes when working with a `data.table`.

``` r

# Summarize all new detections for each fish detected
overall_summary <- new_det[, .(
  n_detections = .N,                          # total number of detections
  n_stations = uniqueN(`Station Name`),       # total number of stations detected on
  first_det = min(`Date and Time (UTC)`),     # first detection
  last_det = max(`Date and Time (UTC)`)       # last detection
), by = .(Transmitter)][order(Transmitter)]   # group by "Transmitter" and set logical order


# Summarize by grouped stations or arrays
glover_bight = c(paste0("GB", 1:19), "Glover Bight", "Glover Bight Creek")
gb_det <- new_det[`Station Name` %in% glover_bight]
gb_summary <- gb_det[, .(
  n_detections = .N,
  n_stations = uniqueN(`Station Name`),
  first_det = min(`Date and Time (UTC)`),
  last_det = max(`Date and Time (UTC)`)
), by = .(Transmitter)][order(Transmitter)]


# Summarize for specific fish of interest
favorite_fish <- list(
  "Pumpkin"       = "A69-9001-57441",
  "Helga"         = "A69-9001-42821",
  "Big Mamma"     = "A69-9001-42829",
  "Small Fry"     = "A69-9001-54931",
  "Lone Wanderer" = "A69-9001-12369"
)

favorite_results <- list()
  for (fish_name in names(favorite_fish)) {
    trans_id <- favorite_fish[[fish_name]]
    
    if (trans_id %in% new_det$Transmitter) {
      fish_summary <- new_det[Transmitter == trans_id, .(
        n_detections = .N,
        first_det = min(`Date and Time (UTC)`),
        last_det = max(`Date and Time (UTC)`)
      ), by = .(`Station Name`)][order(-last_det)]
      
      favorite_results[[fish_name]] <- fish_summary
    } else {
      favorite_results[[fish_name]] <- NULL
    }
  }
```

## Merge new FACT data and new Charlotte Harbor data with existing database

This is the final phase in quality control of new detections. All new
detections from the FACT Network and/or FWC owned receivers are merged
and standardized to the format of the primary database. A final phase of
quality control is conducted followed by a merger with the existing
database. Lastly we do a final check for duplicate detections (largely a
byproduct of iTag users uploading old data to the FACT Network) before
subsetting the data into smaller more workable datasets. In this step,
the following datasets are produced:

1.  A cleaned and standardized dataset containing all detections
    associated with FWC Charlotte Harbor owned tags across FACT, iTag,
    and FWC Charlotte Harbor receivers.
2.  A series of subsetted datasets for individual species

Each of the datasets produced contain columns identifying:

1.  Species and sex
2.  Capture Date
3.  Length and estimated age at time of capture
4.  Estimated length and age at detection time *t*
5.  The project for which the tag was deployed (`FWCSAW` or `IRL`)
6.  The agency who owns the receiver making the detection
7.  The geographical region in which the receiver is located

This allows for easy extraction of data of particular interest and for
the generation of informative summary statistics.

## Examples of summary statistics

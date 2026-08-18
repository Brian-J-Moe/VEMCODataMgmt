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

# Path to existing FACT RData object and where to store the updated version
existing_db <- "Data/current_FACT_detections.RData"
output_db   <- "Data/updated_FACT_data.RData"

# Import tagging data
tagging_data <- data.table::fread("Data/Acoustic Tagged Fish.csv")
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
  output_rdata = output_db,
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
#> Started: 2026-08-18 15:24:10
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
#>   ✅ Saved: example_folder/Data/updated_FACT_data.RData
#> 
#> ========================================
#> WORKFLOW COMPLETE
#> Finished: 2026-08-18 15:24:17
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
load(output_db)

FACT_detections[, .(
  lat_range = range(Latitude),
  lon_range = range(Longitude)
)]
#>    lat_range lon_range
#>        <num>     <num>
#> 1:   24.4252 -87.22030
#> 2:   31.6499 -79.96945
```

Check for new PIs that are not already in the reference files. FACT
formats the PI field as a single or list of individuals with their names
and contact emails. The easiest way to search through the data is to
identify data with the ‘at’ symbol (@) within it’s `Agency` text string.

``` r
# Check Agency column for new PIs
FACT_detections[grepl("@", Agency), Agency] |> unique()
#> [1] "Melissa Soldevilla (melissa.soldevilla@noaa.gov)"
#> [2] "William Patterson (will.patterson@ufl.edu)"
```

Following any updates to the reference files, the new database can be
re-run through `process_fact_workflow()` to correct for any changes.

``` r
# Correct any changes made to the reference files by passing ONLY the new output data file path and the reference directory file path.
revised_FACT_detections <- VEMCODataMgmt::process_fact_workflow(
  csv_dir = NULL,
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
# Optional: Filter downloads after this date (NULL = process all files)
# Format: "YYYY-MM-DD"
after_date = after_date <- "2026-02-01"
```

Once both directories have been defined and an `after_date` set, use the
functions `sort_VRL_logs()` and `load_detections_by_dates()` to sort
newly downloaded logs into their appropriate folders and import the new
detections.

``` r
# Sort newly downloaded receiver logs
sort_result <- VEMCODataMgmt::sort_VRL_logs(vrl_dir, out_dir, after_date = after_date) 
#> ⏱️  Started: 2026-08-18 15:24:18 (32BKD74)
#> 
#> 🔎 Scanning for .vrl and .csv in: example_folder/Receiver Logs
#> 
#> 🗂️  Preparing 5 date folder(s) in: example_folder/Logs by Date
#>   |                                                                              |                                                                      |   0%  |                                                                              |==============                                                        |  20%  |                                                                              |============================                                          |  40%  |                                                                              |==========================================                            |  60%  |                                                                              |========================================================              |  80%  |                                                                              |======================================================================| 100%
#>    ⏱️  Prepare folders: 0.1s
#> 
#> 📦 Copying files into date folders...
#>   • 2026-02-04: 45 file(s) to copy
#>   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   2%  |                                                                              |===                                                                   |   4%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |========                                                              |  11%  |                                                                              |=========                                                             |  13%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  18%  |                                                                              |==============                                                        |  20%  |                                                                              |================                                                      |  22%  |                                                                              |=================                                                     |  24%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  29%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  33%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  38%  |                                                                              |============================                                          |  40%  |                                                                              |==============================                                        |  42%  |                                                                              |===============================                                       |  44%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  49%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  53%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  58%  |                                                                              |==========================================                            |  60%  |                                                                              |============================================                          |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  69%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  73%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  78%  |                                                                              |========================================================              |  80%  |                                                                              |==========================================================            |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  89%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  93%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  98%  |                                                                              |======================================================================| 100%
#>     ✅ 2026-02-04 complete (45/45 copied).
#>   • 2026-02-17: 33 file(s) to copy
#>   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   3%  |                                                                              |====                                                                  |   6%  |                                                                              |======                                                                |   9%  |                                                                              |========                                                              |  12%  |                                                                              |===========                                                           |  15%  |                                                                              |=============                                                         |  18%  |                                                                              |===============                                                       |  21%  |                                                                              |=================                                                     |  24%  |                                                                              |===================                                                   |  27%  |                                                                              |=====================                                                 |  30%  |                                                                              |=======================                                               |  33%  |                                                                              |=========================                                             |  36%  |                                                                              |============================                                          |  39%  |                                                                              |==============================                                        |  42%  |                                                                              |================================                                      |  45%  |                                                                              |==================================                                    |  48%  |                                                                              |====================================                                  |  52%  |                                                                              |======================================                                |  55%  |                                                                              |========================================                              |  58%  |                                                                              |==========================================                            |  61%  |                                                                              |=============================================                         |  64%  |                                                                              |===============================================                       |  67%  |                                                                              |=================================================                     |  70%  |                                                                              |===================================================                   |  73%  |                                                                              |=====================================================                 |  76%  |                                                                              |=======================================================               |  79%  |                                                                              |=========================================================             |  82%  |                                                                              |===========================================================           |  85%  |                                                                              |==============================================================        |  88%  |                                                                              |================================================================      |  91%  |                                                                              |==================================================================    |  94%  |                                                                              |====================================================================  |  97%  |                                                                              |======================================================================| 100%
#>     ✅ 2026-02-17 complete (33/33 copied).
#>   • 2026-02-24: 30 file(s) to copy
#>   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   3%  |                                                                              |=====                                                                 |   7%  |                                                                              |=======                                                               |  10%  |                                                                              |=========                                                             |  13%  |                                                                              |============                                                          |  17%  |                                                                              |==============                                                        |  20%  |                                                                              |================                                                      |  23%  |                                                                              |===================                                                   |  27%  |                                                                              |=====================                                                 |  30%  |                                                                              |=======================                                               |  33%  |                                                                              |==========================                                            |  37%  |                                                                              |============================                                          |  40%  |                                                                              |==============================                                        |  43%  |                                                                              |=================================                                     |  47%  |                                                                              |===================================                                   |  50%  |                                                                              |=====================================                                 |  53%  |                                                                              |========================================                              |  57%  |                                                                              |==========================================                            |  60%  |                                                                              |============================================                          |  63%  |                                                                              |===============================================                       |  67%  |                                                                              |=================================================                     |  70%  |                                                                              |===================================================                   |  73%  |                                                                              |======================================================                |  77%  |                                                                              |========================================================              |  80%  |                                                                              |==========================================================            |  83%  |                                                                              |=============================================================         |  87%  |                                                                              |===============================================================       |  90%  |                                                                              |=================================================================     |  93%  |                                                                              |====================================================================  |  97%  |                                                                              |======================================================================| 100%
#>     ✅ 2026-02-24 complete (30/30 copied).
#>   • 2026-03-13: 54 file(s) to copy
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   2%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |========                                                              |  11%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  15%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |================                                                      |  22%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  28%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  33%  |                                                                              |=========================                                             |  35%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  39%  |                                                                              |=============================                                         |  41%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  52%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  59%  |                                                                              |===========================================                           |  61%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  65%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |===================================================                   |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  78%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  83%  |                                                                              |============================================================          |  85%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  89%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================| 100%
#>     ✅ 2026-03-13 complete (54/54 copied).
#>   • 2026-03-16: 3 file(s) to copy
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
#>     ✅ 2026-03-16 complete (3/3 copied).
#> • Total copied: 165 file(s).
#>    ⏱️  Copy files: 0.3s
#> 
#> 💾 Writing per-date RData files...
#>   |                                                                              |                                                                      |   0%  |                                                                              |==============                                                        |  20%  |                                                                              |============================                                          |  40%  |                                                                              |==========================================                            |  60%  |                                                                              |========================================================              |  80%  |                                                                              |======================================================================| 100%
#>    ⏱️  Per-date RData: 4.3s
#> 
#> 📊 Building combined detections (all dates)...
#> ✅ Combined detections saved: example_folder/Logs by Date/detections_all_2026-08-18.RData (rows: 2,064,983)
#>    ⏱️  Combined RData: 4.1s
#> 
#> ⏱️  Finished: 2026-08-18 15:24:26  •  Elapsed: 8.8s

# Import new data
new_det_full <- VEMCODataMgmt::load_detections_by_date(out_dir, after_date = after_date, verbose = FALSE)


# View new data
new_det_full
#>          Date and Time (UTC)     Receiver    Transmitter Transmitter Name
#>                       <POSc>       <char>         <char>           <lgcl>
#>       1: 2025-11-19 02:02:42 VR2Tx-489235  A69-9001-8527               NA
#>       2: 2025-11-19 02:04:34 VR2Tx-489235  A69-9001-8527               NA
#>       3: 2025-11-20 20:22:21 VR2Tx-489235 A69-1604-25942               NA
#>       4: 2025-11-22 11:29:01 VR2Tx-489235 A69-9001-54956               NA
#>       5: 2025-11-22 11:34:13 VR2Tx-489235 A69-9001-54956               NA
#>      ---                                                                 
#> 2064979: 2026-03-16 17:26:07 VR2Tx-489385 A69-1601-63337               NA
#> 2064980: 2026-03-16 17:35:22 VR2Tx-489385 A69-1601-63337               NA
#> 2064981: 2026-03-16 17:44:49 VR2Tx-489385 A69-1601-63337               NA
#> 2064982: 2026-03-16 17:54:40 VR2Tx-489385 A69-1601-63337               NA
#> 2064983: 2026-03-16 18:05:19 VR2Tx-489385 A69-1601-63337               NA
#>          Transmitter Serial Sensor Value Sensor Unit  Station Name Latitude
#>                      <lgcl>        <int>      <char>        <char>    <num>
#>       1:                 NA           NA        <NA> Picnic Island 26.49260
#>       2:                 NA           NA        <NA> Picnic Island 26.49260
#>       3:                 NA           NA        <NA> Picnic Island 26.49260
#>       4:                 NA           NA        <NA> Picnic Island 26.49260
#>       5:                 NA           NA        <NA> Picnic Island 26.49260
#>      ---                                                                   
#> 2064979:                 NA           NA        <NA>           GB7 26.53831
#> 2064980:                 NA           NA        <NA>           GB7 26.53831
#> 2064981:                 NA           NA        <NA>           GB7 26.53831
#> 2064982:                 NA           NA        <NA>           GB7 26.53831
#> 2064983:                 NA           NA        <NA>           GB7 26.53831
#>          Longitude Transmitter Type Sensor Precision
#>              <num>           <lgcl>           <lgcl>
#>       1: -82.05100               NA               NA
#>       2: -82.05100               NA               NA
#>       3: -82.05100               NA               NA
#>       4: -82.05100               NA               NA
#>       5: -82.05100               NA               NA
#>      ---                                            
#> 2064979: -81.99423               NA               NA
#> 2064980: -81.99423               NA               NA
#> 2064981: -81.99423               NA               NA
#> 2064982: -81.99423               NA               NA
#> 2064983: -81.99423               NA               NA
#>                           source_csv serial dl_date_chr    dl_date date_label
#>                               <char> <char>      <char>     <Date>     <char>
#>       1: VR2Tx_489235_20260204_1.csv 489235    20260204 2026-02-04 2026-02-04
#>       2: VR2Tx_489235_20260204_1.csv 489235    20260204 2026-02-04 2026-02-04
#>       3: VR2Tx_489235_20260204_1.csv 489235    20260204 2026-02-04 2026-02-04
#>       4: VR2Tx_489235_20260204_1.csv 489235    20260204 2026-02-04 2026-02-04
#>       5: VR2Tx_489235_20260204_1.csv 489235    20260204 2026-02-04 2026-02-04
#>      ---                                                                     
#> 2064979: VR2Tx_489385_20260316_1.csv 489385    20260316 2026-03-16 2026-03-16
#> 2064980: VR2Tx_489385_20260316_1.csv 489385    20260316 2026-03-16 2026-03-16
#> 2064981: VR2Tx_489385_20260316_1.csv 489385    20260316 2026-03-16 2026-03-16
#> 2064982: VR2Tx_489385_20260316_1.csv 489385    20260316 2026-03-16 2026-03-16
#> 2064983: VR2Tx_489385_20260316_1.csv 489385    20260316 2026-03-16 2026-03-16
```

These data represent ALL transmitters detected including orphan tags,
time synchronization tags, and spatial reference tags. Using a list of
deployed tags, extract only those data.

``` r

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

print(overall_summary, nrows = 20)
#>        Transmitter n_detections n_stations           first_det
#>             <char>        <int>      <int>              <POSc>
#>  1:  A69-9001-1787         2739         29 2025-12-01 07:16:44
#>  2: A69-9001-42814         2302          6 2025-12-05 22:38:26
#>  3: A69-9001-42817          639          2 2025-12-05 23:45:41
#>  4: A69-9001-42818         1968          7 2025-12-06 02:46:41
#>  5: A69-9001-42819         1695          2 2025-12-06 02:50:56
#> ---                                                           
#> 49: A69-9001-57425           12          2 2025-12-03 04:52:33
#> 50: A69-9001-57437        10793         35 2025-12-01 07:40:17
#> 51: A69-9001-57439           83          5 2026-01-19 04:33:15
#> 52: A69-9001-60301          150         14 2025-11-26 02:30:19
#> 53: A69-9001-60378            4          1 2026-01-17 16:24:16
#>                last_det
#>                  <POSc>
#>  1: 2026-01-29 09:51:11
#>  2: 2026-02-23 23:54:34
#>  3: 2026-02-24 10:04:31
#>  4: 2026-02-24 11:44:26
#>  5: 2026-02-24 06:47:07
#> ---                    
#> 49: 2026-01-26 07:34:06
#> 50: 2026-03-04 10:35:18
#> 51: 2026-02-09 07:51:28
#> 52: 2026-01-16 02:12:08
#> 53: 2026-02-03 14:54:30
```

``` r
# Summarize by grouped stations or arrays
glover_bight = c(paste0("GB", 1:19), "Glover Bight", "Glover Bight Creek")
gb_det <- new_det[`Station Name` %in% glover_bight]
gb_summary <- gb_det[, .(
  n_detections = .N,
  n_stations = uniqueN(`Station Name`),
  first_det = min(`Date and Time (UTC)`),
  last_det = max(`Date and Time (UTC)`)
), by = .(Transmitter)][order(Transmitter)]

print(gb_summary, nrows = 20)
#>        Transmitter n_detections n_stations           first_det
#>             <char>        <int>      <int>              <POSc>
#>  1:  A69-9001-1787         1892         13 2025-12-14 02:06:20
#>  2: A69-9001-42838        14996         17 2025-12-16 07:36:57
#>  3: A69-9001-46966        18697         18 2025-12-13 09:46:42
#>  4: A69-9001-46969         3595         18 2026-01-09 03:32:00
#>  5: A69-9001-46996         9931         18 2025-12-12 23:03:51
#> ---                                                           
#> 20: A69-9001-54946        12901         18 2025-12-14 02:38:11
#> 21: A69-9001-54951         9602         18 2025-12-12 22:45:48
#> 22: A69-9001-54956            4          1 2025-12-23 07:59:45
#> 23: A69-9001-54962            1          1 2026-03-04 13:00:47
#> 24: A69-9001-57437        10044         18 2025-12-13 06:21:04
#>                last_det
#>                  <POSc>
#>  1: 2026-01-29 06:51:44
#>  2: 2026-02-08 18:22:47
#>  3: 2026-02-25 04:59:27
#>  4: 2026-02-09 09:16:04
#>  5: 2026-03-12 11:03:21
#> ---                    
#> 20: 2026-03-03 05:45:00
#> 21: 2026-02-25 01:46:31
#> 22: 2025-12-23 08:16:10
#> 23: 2026-03-04 13:00:47
#> 24: 2026-03-04 10:35:18
```

``` r
# Summarize for specific fish of interest
fish_lookup <- data.table(
  fish_name = c("Pumpkin", "Helga", "Big Mamma", "Small Fry", "Lone Wanderer"),
  Transmitter = c("A69-9001-57441", "A69-9001-42821", "A69-9001-42829", "A69-9001-54931", "A69-9001-12369")
)

favorite_results <- new_det[fish_lookup, on = .(Transmitter), nomatch = NULL][
  , .(
    n_detections = .N,
    first_det = min(`Date and Time (UTC)`),
    last_det = max(`Date and Time (UTC)`)
  ), 
  by = .(fish_name, Transmitter, `Station Name`)
][order(-last_det)]
#> Warning in min.default(structure(numeric(0), class = c("POSIXct", "POSIXt": no
#> non-missing arguments to min; returning Inf
#> Warning in max.default(structure(numeric(0), class = c("POSIXct", "POSIXt": no
#> non-missing arguments to max; returning -Inf

favorite_results
#> Empty data.table (0 rows and 6 cols): fish_name,Transmitter,Station Name,n_detections,first_det,last_det
```

*In this case, none of the ‘favorite fish’ were detected in the new
data*

#' Import and consolidate FACT network CSV files
#'
#' @description
#' Reads all CSV files from a specified directory containing FACT network
#' detection data and consolidates them into a single `data.table`.
#' Standardizes column names and removes release records.
#'
#' @param csv_dir Character. Path to directory containing FACT CSV exports.
#' @param pattern Optional character. File pattern to match (default: `"\\.csv$"`).
#' @param datetime_col Character. Name of datetime column in source CSVs.
#'   Default: `"datecollected"`.
#' @param transmitter_col Character. Name of transmitter column in source CSVs.
#'   Default: `"tagname"`.
#' @param station_col Character. Name of station column in source CSVs.
#'   Default: `"station"`.
#' @param receiver_col Character. Name of receiver column in source CSVs.
#'   Default: `"receiver"`.
#' @param lat_col Character. Name of latitude column. Default: `"latitude"`.
#' @param lon_col Character. Name of longitude column. Default: `"longitude"`.
#' @param poc_col Character. Name of point-of-contact column. Default: `"contact_poc"`.
#' @param pi_col Character. Name of principal investigator column. Default: `"contact_pi"`.
#' @param verbose Logical. Print progress messages. Default: `TRUE`.
#'
#' @import data.table
#' @return A `data.table` with standardized columns:
#'   `Date.Time`, `Station.Name`, `Receiver`, `Transmitter`,
#'   `Latitude`, `Longitude`, `Agency`
#'
#' @examples
#' \dontrun{
#' fact_raw <- import_fact_csvs(
#'   csv_dir = "Z:/FACT/2025 FACT Data Exports/"
#' )
#' }
#'
#' @seealso [process_fact_agencies()], [apply_fact_corrections()], [merge_fact_databases()]
#' @export
import_fact_csvs <- function(
    csv_dir,
    pattern = "\\.csv$",
    datetime_col = "dateCollectedUTC",
    transmitter_col = "tagName",
    station_col = "station",
    receiver_col = "receiver",
    lat_col = "decimalLatitude",
    lon_col = "decimalLongitude",
    poc_col = "contactPOC",
    pi_col = "contactPI",
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  if (!dir.exists(csv_dir))
    stop("Directory not found: ", csv_dir)

  say <- function(...) if (isTRUE(verbose)) message(...)

  # List all CSV files
  csv_files <- list.files(csv_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

  if (!length(csv_files))
    stop("No CSV files found in: ", csv_dir)

  say("Found ", length(csv_files), " CSV file(s)")
  say("Importing and consolidating...")

  # Import all files
  fact_list <- lapply(csv_files, function(f) {
    tryCatch(
      data.table::fread(f, fill = TRUE),
      error = function(e) {
        warning("Failed to read ", basename(f), ": ", e$message)
        data.table::data.table()
      }
    )
  })

  # Combine
  fact_all <- data.table::rbindlist(fact_list, fill = TRUE)

  if (!nrow(fact_all))
    stop("No data successfully imported from CSV files.")

  say("Imported ", format(nrow(fact_all), big.mark = ","), " total records")

  # Standardize column names
  required_cols <- c(datetime_col, transmitter_col, station_col, receiver_col,
                     lat_col, lon_col)
  missing_cols <- setdiff(required_cols, names(fact_all))

  if (length(missing_cols))
    stop("Required columns missing from CSV files: ", paste(missing_cols, collapse = ", "))

  # Rename to standard names
  data.table::setnames(
    fact_all,
    old = c(datetime_col, transmitter_col, station_col, receiver_col,
            lat_col, lon_col, pi_col, poc_col),
    new = c("Date.Time", "Transmitter", "Station.Name", "Receiver",
            "Latitude", "Longitude", "Agency", "POC"),
    skip_absent = TRUE
  )

  # Remove release records
  n_before <- nrow(fact_all)
  fact_all <- fact_all[tolower(Receiver) != "release"]
  n_removed <- n_before - nrow(fact_all)

  if (n_removed > 0)
    say("Removed ", n_removed, " release record(s)")

  # Fill missing Agency with POC
  if ("POC" %in% names(fact_all)) {
    fact_all[is.na(Agency), Agency := POC]
    fact_all[, POC := NULL]
  }

  # Select and order columns
  keep_cols <- c("Date.Time", "Station.Name", "Receiver", "Transmitter",
                 "Latitude", "Longitude", "Agency")
  fact_all <- fact_all[, ..keep_cols]

  say("✅ Import complete: ", format(nrow(fact_all), big.mark = ","), " records")

  fact_all
}


#' Process FACT network agency assignments
#'
#' @description
#' Filters unwanted agencies and standardizes agency/PI names according
#' to reference lookup tables. Operates in two passes:
#' 1. Remove detections from specified unwanted agencies
#' 2. Reassign agency names using pattern matching
#'
#' @param fact_data A `data.table` of FACT detections (from [import_fact_csvs()]).
#' @param agency_col Character. Name of agency column. Default: `"Agency"`.
#' @param unwanted_agencies Optional character vector of agency/PI names to remove.
#'   If `NULL`, reads from `unwanted_agencies_file`.
#' @param unwanted_agencies_file Optional path to CSV file with column `"Agency"`
#'   listing agencies to remove. Ignored if `unwanted_agencies` provided.
#' @param agency_lookup Optional `data.table` or `data.frame` with columns
#'   `pattern` (regex pattern to match) and `standardized` (replacement name).
#'   If `NULL`, reads from `agency_lookup_file`.
#' @param agency_lookup_file Optional path to CSV file with `pattern` and
#'   `standardized` columns. Ignored if `agency_lookup` provided.
#' @param verbose Logical. Print progress. Default: `TRUE`.
#'
#' @import data.table
#' @return A `data.table` with filtered and standardized agency assignments.
#'
#' @details
#' The `agency_lookup` table should have:
#' - `pattern`: String or regex pattern to search for (e.g., "Barbieri", "Reyier|Reiyer")
#' - `standardized`: Replacement agency code (e.g., "FWC-Barbieri", "NASA-Reyier")
#'
#' Patterns are matched in order, so more specific patterns should come first.
#'
#' @examples
#' \dontrun{
#' # Using reference files
#' fact_filtered <- process_fact_agencies(
#'   fact_raw,
#'   unwanted_agencies_file = "Reference Files/unwanted_agencies.csv",
#'   agency_lookup_file = "Reference Files/agency_lookup.csv"
#' )
#'
#' # Using vectors directly
#' fact_filtered <- process_fact_agencies(
#'   fact_raw,
#'   unwanted_agencies = c("Allen Curry", "Marc Trudel"),
#'   agency_lookup = data.table(
#'     pattern = c("Barbieri", "Reyier"),
#'     standardized = c("FWC-Barbieri", "NASA-Reyier")
#'   )
#' )
#' }
#'
#' @seealso [import_fact_csvs()], [apply_fact_corrections()]
#' @export
process_fact_agencies <- function(
    fact_data,
    agency_col = "Agency",
    unwanted_agencies = NULL,
    unwanted_agencies_file = NULL,
    agency_lookup = NULL,
    agency_lookup_file = NULL,
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  if (!inherits(fact_data, "data.frame"))
    stop("`fact_data` must be a data.frame or data.table.")

  fact_dt <- data.table::as.data.table(fact_data)

  if (!agency_col %in% names(fact_dt))
    stop("Column '", agency_col, "' not found in data.")

  say <- function(...) if (isTRUE(verbose)) message(...)

  # Standardize column name temporarily
  if (agency_col != "Agency")
    data.table::setnames(fact_dt, old = agency_col, new = "Agency")

  n_start <- nrow(fact_dt)

  # ---- Step 1: Remove unwanted agencies ----
  if (is.null(unwanted_agencies) && !is.null(unwanted_agencies_file)) {
    if (!file.exists(unwanted_agencies_file))
      stop("Unwanted agencies file not found: ", unwanted_agencies_file)

    unwanted_dt <- data.table::fread(unwanted_agencies_file)
    if (!"Agency" %in% names(unwanted_dt))
      stop("Unwanted agencies file must have column 'Agency'")

    unwanted_agencies <- unwanted_dt$Agency
  }

  if (!is.null(unwanted_agencies) && length(unwanted_agencies) > 0) {
    say("Filtering ", length(unwanted_agencies), " unwanted agencies...")

    if (isTRUE(verbose)) {
      pb <- utils::txtProgressBar(min = 0, max = length(unwanted_agencies), style = 3)
    }

    for (i in seq_along(unwanted_agencies)) {
      pattern <- unwanted_agencies[i]
      fact_dt <- fact_dt[!grepl(pattern, Agency, ignore.case = FALSE)]
      if (isTRUE(verbose)) utils::setTxtProgressBar(pb, i)
    }

    if (isTRUE(verbose)) close(pb)

    n_removed <- n_start - nrow(fact_dt)
    say("  Removed ", format(n_removed, big.mark = ","), " records from unwanted agencies")
  }

  # ---- Step 2: Standardize agency names ----
  if (is.null(agency_lookup) && !is.null(agency_lookup_file)) {
    if (!file.exists(agency_lookup_file))
      stop("Agency lookup file not found: ", agency_lookup_file)

    agency_lookup <- data.table::fread(agency_lookup_file)
  }

  if (!is.null(agency_lookup)) {
    agency_lookup <- data.table::as.data.table(agency_lookup)

    if (!all(c("pattern", "standardized") %in% names(agency_lookup)))
      stop("Agency lookup must have columns 'pattern' and 'standardized'")

    say("Standardizing agency names (", nrow(agency_lookup), " patterns)...")

    if (isTRUE(verbose)) {
      pb <- utils::txtProgressBar(min = 0, max = nrow(agency_lookup), style = 3)
    }

    for (i in seq_len(nrow(agency_lookup))) {
      pattern <- agency_lookup$pattern[i]
      replacement <- agency_lookup$standardized[i]

      fact_dt[grepl(pattern, Agency, ignore.case = FALSE),
              Agency := replacement]

      if (isTRUE(verbose)) utils::setTxtProgressBar(pb, i)
    }

    if (isTRUE(verbose)) close(pb)

    say("  ✅ Agency standardization complete")
  }

  # Restore original column name if needed
  if (agency_col != "Agency")
    data.table::setnames(fact_dt, old = "Agency", new = agency_col)

  fact_dt
}


#' Apply station-specific corrections to FACT data
#'
#' @description
#' Applies station name corrections, coordinate fixes, and agency reassignments
#' based on reference lookup tables. Corrections are applied in sequence:
#' 1. Station-specific agency reassignments
#' 2. Station name corrections
#' 3. Coordinate updates from master receiver list
#'
#' @param fact_data A `data.table` of FACT detections.
#' @param station_col Character. Name of station column. Default: `"Station.Name"`.
#' @param agency_col Character. Name of agency column. Default: `"Agency"`.
#' @param lat_col Character. Name of latitude column. Default: `"Latitude"`.
#' @param lon_col Character. Name of longitude column. Default: `"Longitude"`.
#' @param station_agency_reassign Optional `data.table` with columns
#'   `Station.Name` and `Agency` for station-specific agency reassignments.
#'   If `NULL`, reads from `station_agency_file`.
#' @param station_agency_file Optional path to CSV file for station-specific
#'   agency reassignments.
#' @param station_name_corrections Optional `data.table` with columns
#'   `old_name` and `new_name` (and optionally `Agency` for agency-specific
#'   corrections). If `NULL`, reads from `station_name_file`.
#' @param station_name_file Optional path to CSV file with station name corrections.
#' @param master_receivers Optional `data.table` with columns
#'   `Station.Name`, `Agency`, `Latitude`, `Longitude` for coordinate
#'   standardization. If `NULL`, reads from `master_receivers_file`.
#' @param master_receivers_file Optional path to master receiver list CSV.
#' @param agency_specific_corrections Optional named list of agency-specific
#'   correction rules (e.g., `list(NASA = list(pattern = "CS", suffix = "-NASA"))`).
#' @param verbose Logical. Print progress. Default: `TRUE`.
#'
#' @import data.table
#' @return A `data.table` with corrected station names, agencies, and coordinates.
#'
#' @details
#' **Station-Agency Reassignment**: Useful for receivers that changed hands.
#' Format: `Station.Name`, `Agency` (new agency assignment).
#'
#' **Station Name Corrections**: Can be global or agency-specific.
#' Format: `old_name`, `new_name`, optionally `Agency` (if correction only
#' applies to specific agency).
#'
#' **Master Receiver List**: Authoritative source for station coordinates.
#' Format: `Station.Name`, `Agency`, `Latitude`, `Longitude`.
#'
#' **Agency-Specific Corrections**: For pattern-based modifications like
#' adding suffixes. Example:
#' ```
#' list(
#'   NASA = list(pattern = "CS", suffix = "-NASA"),
#'   FWC = list(pattern = "FPCC", replacement = "FPLR")
#' )
#' ```
#'
#' @examples
#' \dontrun{
#' fact_corrected <- apply_fact_corrections(
#'   fact_data = fact_filtered,
#'   station_agency_file = "Reference Files/station_agency_reassign.csv",
#'   station_name_file = "Reference Files/station_name_corrections.csv",
#'   master_receivers_file = "Reference Files/MASTER_RECEIVERS.csv"
#' )
#' }
#'
#' @seealso [process_fact_agencies()], [merge_fact_databases()]
#' @export
apply_fact_corrections <- function(
    fact_data,
    station_col = "Station.Name",
    agency_col = "Agency",
    lat_col = "Latitude",
    lon_col = "Longitude",
    station_agency_reassign = NULL,
    station_agency_file = NULL,
    station_name_corrections = NULL,
    station_name_file = NULL,
    master_receivers = NULL,
    master_receivers_file = NULL,
    agency_specific_corrections = NULL,
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  if (!inherits(fact_data, "data.frame"))
    stop("`fact_data` must be a data.frame or data.table.")

  fact_dt <- data.table::as.data.table(fact_data)

  # Validate columns
  required_cols <- c(station_col, agency_col, lat_col, lon_col)
  missing_cols <- setdiff(required_cols, names(fact_dt))
  if (length(missing_cols))
    stop("Required columns missing: ", paste(missing_cols, collapse = ", "))

  say <- function(...) if (isTRUE(verbose)) message(...)

  # Standardize column names temporarily
  col_map <- c(station_col, agency_col, lat_col, lon_col)
  names(col_map) <- c("Station.Name", "Agency", "Latitude", "Longitude")

  for (std_name in names(col_map)) {
    orig_name <- col_map[std_name]
    if (orig_name != std_name && orig_name %in% names(fact_dt)) {
      data.table::setnames(fact_dt, old = orig_name, new = std_name)
    }
  }

  # ---- Step 1: Agency-specific pattern corrections ----
  if (!is.null(agency_specific_corrections)) {
    say("Applying agency-specific corrections...")

    for (agency_name in names(agency_specific_corrections)) {
      rule <- agency_specific_corrections[[agency_name]]

      if (!is.null(rule$pattern) && !is.null(rule$suffix)) {
        # Add suffix to matching stations
        fact_dt[grepl(agency_name, Agency) &
                  grepl(rule$pattern, Station.Name) &
                  !grepl(rule$suffix, Station.Name),
                Station.Name := paste0(Station.Name, rule$suffix)]

      } else if (!is.null(rule$pattern) && !is.null(rule$replacement)) {
        # Replace station name
        fact_dt[grepl(agency_name, Agency) &
                  Station.Name == rule$pattern,
                Station.Name := rule$replacement]
      }
    }

    say("  ✅ Agency-specific corrections applied")
  }

  # ---- Step 2: Station-specific agency reassignments ----
  if (is.null(station_agency_reassign) && !is.null(station_agency_file)) {
    if (!file.exists(station_agency_file))
      stop("Station agency file not found: ", station_agency_file)

    station_agency_reassign <- data.table::fread(station_agency_file)
  }

  if (!is.null(station_agency_reassign)) {
    station_agency_reassign <- data.table::as.data.table(station_agency_reassign)

    if (!all(c("Station.Name", "Agency") %in% names(station_agency_reassign)))
      stop("Station agency file must have 'Station.Name' and 'Agency' columns")

    say("Applying station-specific agency reassignments (",
        nrow(station_agency_reassign), " stations)...")

    if (isTRUE(verbose)) {
      pb <- utils::txtProgressBar(min = 0, max = nrow(station_agency_reassign), style = 3)
    }

    for (i in seq_len(nrow(station_agency_reassign))) {
      stn <- station_agency_reassign$Station.Name[i]
      new_agency <- station_agency_reassign$Agency[i]

      fact_dt[Station.Name == stn, Agency := new_agency]

      if (isTRUE(verbose)) utils::setTxtProgressBar(pb, i)
    }

    if (isTRUE(verbose)) close(pb)
    say("  ✅ Agency reassignments complete")
  }

  # ---- Step 3: Station name corrections ----
  if (is.null(station_name_corrections) && !is.null(station_name_file)) {
    if (!file.exists(station_name_file))
      stop("Station name file not found: ", station_name_file)

    station_name_corrections <- data.table::fread(station_name_file)
  }

  if (!is.null(station_name_corrections)) {
    station_name_corrections <- data.table::as.data.table(station_name_corrections)

    if (!all(c("old_name", "new_name") %in% names(station_name_corrections)))
      stop("Station name file must have 'old_name' and 'new_name' columns")

    say("Applying station name corrections (",
        nrow(station_name_corrections), " corrections)...")

    if (isTRUE(verbose)) {
      pb <- utils::txtProgressBar(min = 0, max = nrow(station_name_corrections), style = 3)
    }

    for (i in seq_len(nrow(station_name_corrections))) {
      old_name <- station_name_corrections$old_name[i]
      new_name <- station_name_corrections$new_name[i]

      # Check if agency-specific
      if ("Agency" %in% names(station_name_corrections) &&
          !is.na(station_name_corrections$Agency[i])) {
        agency_filter <- station_name_corrections$Agency[i]
        fact_dt[Station.Name == old_name & Agency == agency_filter,
                Station.Name := new_name]
      } else {
        fact_dt[Station.Name == old_name, Station.Name := new_name]
      }

      if (isTRUE(verbose)) utils::setTxtProgressBar(pb, i)
    }

    if (isTRUE(verbose)) close(pb)
    say("  ✅ Station name corrections complete")
  }

  # ---- Step 4: Coordinate updates from master receiver list ----
  if (is.null(master_receivers) && !is.null(master_receivers_file)) {
    if (!file.exists(master_receivers_file))
      stop("Master receivers file not found: ", master_receivers_file)

    master_receivers <- data.table::fread(master_receivers_file)
  }

  if (!is.null(master_receivers)) {
    master_receivers <- data.table::as.data.table(master_receivers)

    required_master_cols <- c("Station.Name", "Latitude", "Longitude")
    if ("Owner" %in% names(master_receivers)) {
      data.table::setnames(master_receivers, "Owner", "Agency")
    }
    required_master_cols <- c(required_master_cols, "Agency")

    missing_master <- setdiff(required_master_cols, names(master_receivers))
    if (length(missing_master))
      stop("Master receivers file missing columns: ", paste(missing_master, collapse = ", "))

    say("Updating coordinates from master receiver list (",
        nrow(master_receivers), " stations)...")

    if (isTRUE(verbose)) {
      pb <- utils::txtProgressBar(min = 0, max = nrow(master_receivers), style = 3)
    }

    for (i in seq_len(nrow(master_receivers))) {
      stn <- master_receivers$Station.Name[i]
      agn <- master_receivers$Agency[i]
      lat <- master_receivers$Latitude[i]
      lon <- master_receivers$Longitude[i]

      fact_dt[Station.Name == stn & Agency == agn,
              `:=`(Latitude = lat, Longitude = lon)]

      if (isTRUE(verbose)) utils::setTxtProgressBar(pb, i)
    }

    if (isTRUE(verbose)) close(pb)
    say("  ✅ Coordinate updates complete")
  }

  # Restore original column names if needed
  for (std_name in names(col_map)) {
    orig_name <- col_map[std_name]
    if (orig_name != std_name && std_name %in% names(fact_dt)) {
      data.table::setnames(fact_dt, old = std_name, new = orig_name)
    }
  }

  fact_dt
}


#' Merge FACT databases with duplicate detection
#'
#' @description
#' Merges new FACT detections with an existing database, handling timestamp
#' precision mismatches and identifying duplicates. Returns duplicated
#' combined dataset.
#'
#' @param new_data A `data.table` of new FACT detections.
#' @param existing_data Optional `data.table` of existing detections to merge with.
#'   If `NULL`, reads from `existing_rdata_file`.
#' @param existing_rdata_file Optional path to `.RData` file containing
#'   existing data. Object must be named `FACT_detections`.
#' @param datetime_col Character. Name of datetime column. Default: `"Date.Time"`.
#' @param handle_precision Logical. If `TRUE`, handles timestamp precision
#'   mismatches by rounding to minute before duplicate checking. Default: `TRUE`.
#' @param dup_cols Character vector of columns to use for duplicate detection.
#'   Default: `c("Date.Time", "Station.Name", "Transmitter", "Latitude", "Longitude", "Agency")`.
#' @param verbose Logical. Print progress. Default: `TRUE`.
#'
#' @import data.table
#' @return A list with components:
#' \describe{
#'   \item{combined_data}{`data.table` of duplicated combined detections}
#'   \item{n_new}{Number of new records added}
#'   \item{n_duplicates}{Number of duplicate records found}
#'   \item{n_total}{Total records in combined dataset}
#' }
#'
#' @details
#' When `handle_precision = TRUE`, the function creates a temporary rounded
#' timestamp column (minute precision) for duplicate detection, but preserves
#' the original full-precision timestamps in the output.
#'
#' @examples
#' \dontrun{
#' merge_result <- merge_fact_databases(
#'   new_data = fact_corrected,
#'   existing_rdata_file = "UPDATED_FACT_detections.RData"
#' )
#'
#' # Access components
#' combined_detections <- merge_result$combined_data
#' message("Added ", merge_result$n_new, " new records")
#' }
#'
#' @seealso [import_fact_csvs()], [validate_fact_database()]
#' @export
merge_fact_databases <- function(
    new_data,
    existing_data = NULL,
    existing_rdata_file = NULL,
    datetime_col = "Date.Time",
    handle_precision = TRUE,
    dup_cols = c("Date.Time", "Station.Name", "Transmitter",
                   "Latitude", "Longitude", "Agency"),
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  if (!inherits(new_data, "data.frame"))
    stop("`new_data` must be a data.frame or data.table.")

  new_dt <- data.table::as.data.table(new_data)

  if (!datetime_col %in% names(new_dt))
    stop("Datetime column '", datetime_col, "' not found in new_data.")

  say <- function(...) if (isTRUE(verbose)) message(...)

  # Load existing data if needed
  if (is.null(existing_data) && !is.null(existing_rdata_file)) {
    if (!file.exists(existing_rdata_file))
      stop("Existing RData file not found: ", existing_rdata_file)

    say("Loading existing database from: ", basename(existing_rdata_file))
    env <- new.env()
    load(existing_rdata_file, envir = env)

    if (!"FACT_detections" %in% ls(envir = env))
      stop("RData file must contain object named 'FACT_detections'")

    existing_data <- get("FACT_detections", envir = env)
    old_FACT_detections <- existing_data
    save(old_FACT_detections, paste0(dirname(existing_rdata_file), "/old_FACT_detections.RData"))
  }

  if (!is.null(existing_data)) {
    existing_dt <- data.table::as.data.table(existing_data)

    if (!datetime_col %in% names(existing_dt))
      stop("Datetime column '", datetime_col, "' not found in existing_data.")

    say("Existing database: ", format(nrow(existing_dt), big.mark = ","), " records")
  } else {
    existing_dt <- data.table::data.table()
    say("No existing database provided - starting fresh")
  }

  # Ensure datetime columns are POSIXct
  if (!inherits(new_dt[[datetime_col]], "POSIXct")) {
    new_dt[[datetime_col]] <- as.POSIXct(new_dt[[datetime_col]])
  }

  if (nrow(existing_dt) > 0 && !inherits(existing_dt[[datetime_col]], "POSIXct")) {
    existing_dt[[datetime_col]] <- as.POSIXct(existing_dt[[datetime_col]])
  }

  # Handle timestamp precision if requested
  if (isTRUE(handle_precision)) {
    say("Handling timestamp precision...")

    # Create rounded timestamp for duplicate detection
    new_dt[, Date.Time.Rounded := as.POSIXct(
      format(get(datetime_col), "%Y-%m-%d %H:%M:00"),
      tz = attr(get(datetime_col), "tzone") %||% "UTC"
    )]

    if (nrow(existing_dt) > 0) {
      existing_dt[, Date.Time.Rounded := as.POSIXct(
        format(get(datetime_col), "%Y-%m-%d %H:%M:00"),
        tz = attr(get(datetime_col), "tzone") %||% "UTC"
      )]
    }

    # Modify dup_cols to use rounded time
    dup_cols_adj <- gsub(datetime_col, "Date.Time.Rounded", dup_cols)
  } else {
    dup_cols_adj <- dup_cols
  }

  # Combine datasets
  if (nrow(existing_dt) > 0) {
    combined <- data.table::rbindlist(list(existing_dt, new_dt), fill = TRUE)
  } else {
    combined <- new_dt
  }

  n_before_dup <- nrow(combined)

  # Remove duplicates
  say("Checking for duplicates...")

  # Verify dup columns exist
  missing_dup <- setdiff(dup_cols_adj, names(combined))
  if (length(missing_dup))
    stop("Duplication columns missing: ", paste(missing_dup, collapse = ", "))

  combined_x <- combined[, .(N = .N), by = dup_cols_adj]
  combined_x <- combined_x[N > 1, ]
  combined_x[, N := NULL]

  new_x <- data.table::rbindlist(list(combined_x, new_dt), fill = TRUE)
  new_x <- new_x[, .(N = .N), by = dup_cols_adj]
  new_data <- new_x[N == 1, ]
  new_data <- new_data[, N := NULL]

  combined_dt <- unique(combined, by = dup_cols_adj)

  n_duplicates <- n_before_dup - nrow(combined_dt)
  n_new <- nrow(combined_dt) - nrow(existing_dt)

  # Remove temporary rounded column
  if (isTRUE(handle_precision)) {
    new_data[, Date.Time.Rounded := NULL]
    combined_dt[, Date.Time.Rounded := NULL]
  }

  say("✅ Merge complete:")
  say("   • Total records: ", format(nrow(combined_dt), big.mark = ","))
  say("   • New records added: ", format(max(0, n_new), big.mark = ","))
  say("   • Duplicates removed: ", format(n_duplicates, big.mark = ","))

  list(
    combined_data = combined_dt,
    new_data = new_data,
    n_new = max(0, n_new),
    n_duplicates = n_duplicates,
    n_total = nrow(combined_dt)
  )
}


#' Validate FACT database for common issues
#'
#' @description
#' Performs quality control checks on FACT detection data:
#' - Identifies stations with multiple coordinate pairs
#' - Identifies duplicate station names (different agencies/coordinates)
#' - Identifies duplicate coordinate pairs (different station names)
#' - Generates summary statistics by station
#'
#' @param fact_data A `data.table` of FACT detections.
#' @param station_col Character. Name of station column. Default: `"Station.Name"`.
#' @param agency_col Character. Name of agency column. Default: `"Agency"`.
#' @param lat_col Character. Name of latitude column. Default: `"Latitude"`.
#' @param lon_col Character. Name of longitude column. Default: `"Longitude"`.
#' @param datetime_col Character. Name of datetime column. Default: `"Date.Time"`.
#' @param coord_precision Numeric. Number of decimal places for coordinate
#'   comparison. Default: `7`.
#' @param verbose Logical. Print diagnostics. Default: `TRUE`.
#'
#' @import data.table
#' @return A list with validation results:
#' \describe{
#'   \item{station_summary}{`data.table` of stations with detection counts and date ranges}
#'   \item{duplicate_station_names}{Stations with same name but different coordinates/agencies}
#'   \item{duplicate_coordinates}{Coordinates used by multiple stations}
#'   \item{coordinate_drift}{Stations with multiple coordinate pairs}
#'   \item{issues_found}{Logical flag indicating if any issues detected}
#' }
#'
#' @examples
#' \dontrun{
#' validation <- validate_fact_database(combined_detections)
#'
#' # Review issues
#' if (validation$issues_found) {
#'   View(validation$duplicate_station_names)
#'   View(validation$coordinate_drift)
#' }
#'
#' # Save station summary
#' fwrite(validation$station_summary, "station_summary.csv")
#' }
#'
#' @seealso [merge_fact_databases()]
#' @export
validate_fact_database <- function(
    fact_data,
    station_col = "Station.Name",
    agency_col = "Agency",
    lat_col = "Latitude",
    lon_col = "Longitude",
    datetime_col = "Date.Time",
    coord_precision = 7,
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  if (!inherits(fact_data, "data.frame"))
    stop("`fact_data` must be a data.frame or data.table.")

  fact_dt <- data.table::as.data.table(fact_data)

  required_cols <- c(station_col, agency_col, lat_col, lon_col, datetime_col)
  missing_cols <- setdiff(required_cols, names(fact_dt))
  if (length(missing_cols))
    stop("Required columns missing: ", paste(missing_cols, collapse = ", "))

  say <- function(...) if (isTRUE(verbose)) message(...)

  say("Validating FACT database...")

  # Standardize column names
  col_map <- setNames(
    c(station_col, agency_col, lat_col, lon_col, datetime_col),
    c("Station.Name", "Agency", "Latitude", "Longitude", "Date.Time")
  )

  fact_copy <- data.table::copy(fact_dt)
  for (std_name in names(col_map)) {
    orig_name <- col_map[std_name]
    if (orig_name != std_name) {
      data.table::setnames(fact_copy, old = orig_name, new = std_name)
    }
  }

  # Round coordinates for comparison
  fact_copy[, `:=`(
    Lat.Rounded = round(Latitude, coord_precision),
    Lon.Rounded = round(Longitude, coord_precision)
  )]

  # ---- Station summary ----
  station_summary <- fact_copy[, .(
    first_detection = min(Date.Time, na.rm = TRUE),
    last_detection = max(Date.Time, na.rm = TRUE),
    n_detections = .N
  ), by = .(Station.Name, Latitude, Longitude, Agency)]

  # Add rounded coords to summary for joining
  station_summary[, `:=`(
    Lat.Rounded = round(Latitude, coord_precision),
    Lon.Rounded = round(Longitude, coord_precision)
  )]

  data.table::setorder(station_summary, Station.Name, Agency)

  say("  • Total unique stations: ", fact_copy[, uniqueN(Station.Name)])
  say("  • Total station-agency combinations: ", nrow(station_summary))

  # ---- Check for duplicate station names ----
  dup_stations <- station_summary[, .N, by = Station.Name][N > 1]

  if (nrow(dup_stations) > 0) {
    say("  ⚠️  Found ", nrow(dup_stations), " station names with multiple entries")
    dup_station_details <- station_summary[Station.Name %in% dup_stations$Station.Name]
  } else {
    say("  ✅ No duplicate station names found")
    dup_station_details <- data.table::data.table()
  }

  # ---- Check for duplicate coordinates ----
  dup_coords <- station_summary[, .N, by = .(Lat.Rounded, Lon.Rounded)][N > 1]

  if (nrow(dup_coords) > 0) {
    say("  ⚠️  Found ", nrow(dup_coords), " coordinate pairs used by multiple stations")


    dup_coord_details <- station_summary[
      paste(Lat.Rounded, Lon.Rounded) %in%
        dup_coords[, paste(Lat.Rounded, Lon.Rounded)]
    ]

    data.table::setorder(dup_coord_details, Lat.Rounded, Lon.Rounded, Station.Name)
    station_summary[, `:=`(Lat.Rounded = NULL, Lon.Rounded = NULL)]
  } else {
    say("  ✅ No duplicate coordinate pairs found")
    dup_coord_details <- data.table::data.table()
  }

  # ---- Check for coordinate drift within stations ----
  coord_drift <- fact_copy[, .(
    n_coord_pairs = uniqueN(paste(Lat.Rounded, Lon.Rounded))
  ), by = Station.Name][n_coord_pairs > 1]

  if (nrow(coord_drift) > 0) {
    say("  ⚠️  Found ", nrow(coord_drift), " stations with multiple coordinate pairs")
    coord_drift_details <- fact_copy[
      Station.Name %in% coord_drift$Station.Name,
      .(n_detections = .N),
      by = .(Station.Name, Latitude, Longitude)
    ][order(Station.Name, -n_detections)]
  } else {
    say("  ✅ No coordinate drift detected")
    coord_drift_details <- data.table::data.table()
  }

  issues_found <- nrow(dup_station_details) > 0 ||
    nrow(dup_coord_details) > 0 ||
    nrow(coord_drift_details) > 0

  if (issues_found) {
    say("\n⚠️  Validation identified issues - review output components")
  } else {
    say("\n✅ Database validation complete - no issues found")
  }

  list(
    station_summary = station_summary,
    duplicate_station_names = dup_station_details,
    duplicate_coordinates = dup_coord_details,
    coordinate_drift = coord_drift_details,
    issues_found = issues_found
  )
}


#' Complete FACT data processing workflow
#'
#' @description
#' Executes the complete workflow for importing, correcting, and merging
#' FACT network detection data. This is a convenience wrapper around the
#' individual processing functions.
#'
#' @param csv_dir Character. Path to directory containing new FACT CSV exports.
#' @param reference_dir Character. Path to directory containing reference files.
#' @param existing_rdata Optional path to existing FACT database `.RData` file.
#' @param output_rdata Optional path for saving updated database. If `NULL`,
#'   saves to `paste0(dirname(existing_rdata), "/UPDATED_FACT_detections.RData")`.
#' @param datetime_col The column name in the FACT CSV file corresponding to the
#'   date of detection.
#' @param transmitter_col The column name in the FACT CSV file corresponding to
#'   the acoustic transmitter ID.
#' @param station_col The column name in the FACT CSV file corresponding to the
#'   name of the station in which the detection was at.
#' @param receiver_col The column name in the FACT CSV file corresponding to the
#'   acoustic receiver ID.
#' @param lat_col The column name in the FACT CSV file corresponding to the latitude.
#' @param lon_col The column name in the FACT CSV file corresponding to the longitude.
#' @param poc_col The column name in the FACT CSV file corresponding to the person of contact.
#' @param pi_col The column name in the FACT CSV file corresponding to the principle investigator.
#' @param unwanted_agencies_file Filename of unwanted agencies CSV in `reference_dir`.
#'   Default: `"unwanted_agencies.csv"`.
#' @param agency_lookup_file Filename of agency lookup CSV in `reference_dir`.
#'   Default: `"agency_lookup.csv"`.
#' @param station_agency_file Filename of station-agency reassignments CSV.
#'   Default: `"station_agency_reassign.csv"`.
#' @param station_name_file Filename of station name corrections CSV.
#'   Default: `"station_name_corrections.csv"`.
#' @param master_receivers_file Filename of master receiver list CSV.
#'   Default: `"MASTER_RECEIVERS.csv"`.
#' @param validate Logical. Run validation checks after merge? Default: `TRUE`.
#' @param save_validation Logical. Save validation results to CSV? Default: `TRUE`.
#' @param verbose Logical. Print progress. Default: `TRUE`.
#'
#' @import data.table
#' @return A list containing:
#' \describe{
#'   \item{fact_detections}{Final combined and corrected `data.table`}
#'   \item{merge_stats}{Statistics from merge operation}
#'   \item{validation}{Validation results (if `validate = TRUE`)}
#' }
#'
#' @details
#' This function chains together:
#' 1. [import_fact_csvs()]
#' 2. [process_fact_agencies()]
#' 3. [apply_fact_corrections()]
#' 4. [merge_fact_databases()]
#' 5. [validate_fact_database()] (optional)
#'
#' **Reference Files Required** (in `reference_dir`):
#' - `unwanted_agencies.csv`: Single column `"Agency"` with agencies to exclude
#' - `agency_lookup.csv`: Columns `"pattern"` and `"standardized"` for name standardization
#' - `station_agency_reassign.csv`: Columns `"Station.Name"` and `"Agency"` for reassignments
#' - `station_name_corrections.csv`: Columns `"old_name"`, `"new_name"`, optional `"Agency"`
#' - `MASTER_RECEIVERS.csv`: Columns `"Station.Name"`, `"Agency"`, `"Latitude"`, `"Longitude"`
#'
#' @examples
#' \dontrun{
#' # Basic workflow
#' result <- process_fact_workflow(
#'   csv_dir = "Z:/FACT/2025 FACT Data Exports/",
#'   reference_dir = "Reference Files",
#'   existing_rdata = "UPDATED_FACT_detections.RData"
#' )
#'
#' # Access results
#' fact_detections <- result$fact_detections
#' validation <- result$validation
#' }
#'
#' @seealso [import_fact_csvs()], [process_fact_agencies()], [apply_fact_corrections()],
#'   [merge_fact_databases()], [validate_fact_database()]
#' @export
process_fact_workflow <- function(
    csv_dir,
    reference_dir,
    existing_rdata = NULL,
    output_rdata = NULL,
    datetime_col = "dateCollectedUTC",
    transmitter_col = "tagName",
    station_col = "station",
    receiver_col = "receiver",
    lat_col = "decimalLatitude",
    lon_col = "decimalLongitude",
    poc_col = "contactPOC",
    pi_col = "contactPI",
    unwanted_agencies_file = "unwanted_agencies.csv",
    agency_lookup_file = "agency_lookup.csv",
    station_agency_file = "station_agency_reassign.csv",
    station_name_file = "station_name_corrections.csv",
    master_receivers_file = "MASTER_RECEIVERS.csv",
    validate = TRUE,
    save_validation = TRUE,
    verbose = TRUE

) {


  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  say <- function(...) if (isTRUE(verbose)) message(...)

  say("\n========================================")
  say("FACT DATA PROCESSING WORKFLOW")
  say("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  say("========================================\n")

  t_start <- Sys.time()

  # Build reference file paths
  ref_files <- list(
    unwanted_agencies = file.path(reference_dir, unwanted_agencies_file),
    agency_lookup = file.path(reference_dir, agency_lookup_file),
    station_agency = file.path(reference_dir, station_agency_file),
    station_name = file.path(reference_dir, station_name_file),
    master_receivers = file.path(reference_dir, master_receivers_file)
  )

  # Verify reference files exist
  missing_refs <- ref_files[!file.exists(unlist(ref_files))]
  if (length(missing_refs) > 0) {
    warning("Some reference files not found:\n",
            paste("  •", names(missing_refs), ":", unlist(missing_refs), collapse = "\n"),
            "\nProcessing will continue without these files.")
  }

  # ---- Step 1: Import CSVs ----
  say("STEP 1: Importing FACT CSV files...")
  fact_raw <- import_fact_csvs(csv_dir,
                               datetime_col = datetime_col,
                               transmitter_col = transmitter_col,
                               station_col = station_col,
                               receiver_col = receiver_col,
                               lat_col = lat_col,
                               lon_col = lon_col,
                               poc_col = poc_col,
                               pi_col = pi_col,
                               verbose = verbose)
  say()

  # ---- Step 2: Process agencies ----
  say("STEP 2: Processing agency assignments...")
  fact_filtered <- process_fact_agencies(
    fact_raw,
    unwanted_agencies_file = if (file.exists(ref_files$unwanted_agencies)) ref_files$unwanted_agencies else NULL,
    agency_lookup_file = if (file.exists(ref_files$agency_lookup)) ref_files$agency_lookup else NULL,
    verbose = verbose
  )
  say()

  # ---- Step 3: Apply corrections ----
  say("STEP 3: Applying station corrections...")
  fact_corrected <- apply_fact_corrections(
    fact_filtered,
    station_agency_file = if (file.exists(ref_files$station_agency)) ref_files$station_agency else NULL,
    station_name_file = if (file.exists(ref_files$station_name)) ref_files$station_name else NULL,
    master_receivers_file = if (file.exists(ref_files$master_receivers)) ref_files$master_receivers else NULL,
    verbose = verbose
  )
  say()

  # ---- Step 4: Merge databases ----
  say("STEP 4: Merging with existing database...")
  merge_result <- merge_fact_databases(
    fact_corrected,
    existing_rdata_file = existing_rdata,
    verbose = verbose
  )

  fact_combined <- merge_result$combined_data
  fact_new <- merge_result$new_data
  say()

  # ---- Step 5: Validate (optional) ----
  validation_result <- NULL
  if (isTRUE(validate)) {
    say("STEP 5: Running validation checks...")
    validation_result <- validate_fact_database(fact_combined, verbose = verbose)

    if (isTRUE(save_validation) && !is.null(output_rdata)) {
      out_dir <- dirname(output_rdata)
      date_suffix <- format(Sys.Date(), "%Y-%m-%d")

      if (nrow(validation_result$station_summary) > 0) {
        data.table::fwrite(
          validation_result$station_summary,
          file = file.path(out_dir, paste0("station_summary_", date_suffix, ".csv"))
        )
      }

      if (validation_result$issues_found) {
        if (nrow(validation_result$duplicate_station_names) > 0) {
          data.table::fwrite(
            validation_result$duplicate_station_names,
            file = file.path(out_dir, paste0("duplicate_stations_", date_suffix, ".csv"))
          )
        }

        if (nrow(validation_result$duplicate_coordinates) > 0) {
          data.table::fwrite(
            validation_result$duplicate_coordinates,
            file = file.path(out_dir, paste0("duplicate_coordinates_", date_suffix, ".csv"))
          )
        }

        if (nrow(validation_result$coordinate_drift) > 0) {
          data.table::fwrite(
            validation_result$coordinate_drift,
            file = file.path(out_dir, paste0("coordinate_drift_", date_suffix, ".csv"))
          )
        }

        say("  📊 Validation results saved to: ", out_dir)
      }
    }
    say()
  }

  # ---- Step 6: Save output ----
  if (!is.null(output_rdata)) {
    say("STEP 6: Saving updated database...")
    UPDATED_FACT_detections <- fact_combined
    save(UPDATED_FACT_detections, file = output_rdata)
    say("  ✅ Saved: ", output_rdata)
  } else if (!is.null(existing_rdata)) {
    output_rdata <- file.path(
      dirname(existing_rdata),
      "UPDATED_FACT_detections.RData"
    )
    say("STEP 6: Saving updated database...")
    UPDATED_FACT_detections <- fact_combined
    save(UPDATED_FACT_detections, file = output_rdata)
    say("  ✅ Saved: ", output_rdata)
  }

  t_end <- Sys.time()
  elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))

  say("\n========================================")
  say("WORKFLOW COMPLETE")
  say("Finished: ", format(t_end, "%Y-%m-%d %H:%M:%S"))
  say("Elapsed: ", round(elapsed, 1), " seconds")
  say("========================================\n")

  invisible(list(
    all_detections = fact_combined,
    new_detections = fact_new,
    merge_stats = list(
      n_new = merge_result$n_new,
      n_duplicates = merge_result$n_duplicates,
      n_total = merge_result$n_total
    ),
    validation = validation_result
  ))
}

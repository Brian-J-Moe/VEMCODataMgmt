#' Check for duplicate detections
#'
#' @description
#' Identifies duplicate detection records in acoustic telemetry data.
#' Duplicates can occur when the same VRL file is processed multiple times
#' or when receiver logs overlap across download dates.
#'
#' @details
#' Duplicates are identified based on key columns that uniquely define a detection:
#' - `Transmitter`: The tag ID
#' - `Date and Time (UTC)`: Detection timestamp
#' - `Receiver`: Receiver serial number
#' - `Station Name`: Receiver location
#'
#' Additional columns can be specified via `extra_cols` if your data structure
#' includes other identifying information (e.g., sensor values, signal strength).
#'
#' @param det A `data.table` of detection records (typically from [load_detections_by_date()])
#' @param key_cols Character vector of column names to use for duplicate identification.
#'   Default: `c("Transmitter", "Date and Time (UTC)", "Receiver", "Station Name")`.
#' @param extra_cols Optional character vector of additional columns to include in duplicate check.
#' @param return_type Character. One of:
#'   - `"summary"` (default): Returns a summary table of duplicates by source
#'   - `"duplicates"`: Returns only the duplicate records
#'   - `"unique"`: Returns deduplicated dataset (keeps first occurrence)
#'   - `"all"`: Returns list with summary, duplicates, unique, and duplicate indices
#' @param verbose Logical; print diagnostic messages. Default `TRUE`.
#'
#' @import data.table
#' @return Depends on `return_type`:
#'   - `"summary"`: `data.table` with columns: `source_csv`, `n_duplicates`, `pct_duplicates`
#'   - `"duplicates"`: `data.table` containing only duplicate records
#'   - `"unique"`: `data.table` with duplicates removed
#'   - `"all"`: List containing all of the above plus `duplicate_indices`
#'
#' @examples
#' \dontrun{
#' det_all <- load_detections_by_date("Logs_by_Date")
#'
#' # Check for duplicates
#' dup_summary <- check_duplicate_detections(det_all)
#'
#' # Get the duplicate records themselves
#' dups <- check_duplicate_detections(det_all, return_type = "duplicates")
#'
#' # Get deduplicated data
#' det_clean <- check_duplicate_detections(det_all, return_type = "unique")
#'
#' # Get everything
#' dup_analysis <- check_duplicate_detections(det_all, return_type = "all")
#' }
#'
#' @seealso [sort_VRL_logs()], [load_detections_by_date()], [identify_coverage_gaps()]
#' @export
check_duplicate_detections <- function(
    det,
    key_cols = c("Transmitter", "Date and Time (UTC)", "Receiver", "Station Name"),
    extra_cols = NULL,
    return_type = c("summary", "duplicates", "unique", "all"),
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  if (!inherits(det, "data.frame"))
    stop("`det` must be a data.frame or data.table.")

  det <- data.table::as.data.table(det)
  return_type <- match.arg(return_type)

  say <- function(...) if (isTRUE(verbose)) message(...)

  # Combine key columns
  all_key_cols <- unique(c(key_cols, extra_cols))
  missing_cols <- setdiff(all_key_cols, names(det))
  if (length(missing_cols)) {
    stop("The following key columns are missing from data: ", paste(missing_cols, collapse = ", "))
  }

  # Identify duplicates
  say("Checking for duplicates across ", format(nrow(det), big.mark = ","), " records...")

  det[, .dup_id := .I]
  det[, .dup_n := .N, by = all_key_cols]
  det[, .is_dup := .dup_n > 1]

  n_dup <- sum(det$.is_dup)
  n_unique_sets <- det[.is_dup == TRUE, uniqueN(.SD), .SDcols = all_key_cols]

  if (n_dup == 0) {
    say("✅ No duplicates found.")
    if (return_type == "summary") {
      return(data.table::data.table(
        message = "No duplicates detected",
        n_total = nrow(det),
        n_duplicates = 0L
      ))
    } else if (return_type == "duplicates") {
      return(det[0])
    } else if (return_type == "unique") {
      det[, c(".dup_id", ".dup_n", ".is_dup") := NULL]
      return(det)
    } else {
      det[, c(".dup_id", ".dup_n", ".is_dup") := NULL]
      return(list(
        summary = data.table::data.table(
          message = "No duplicates detected",
          n_total = nrow(det),
          n_duplicates = 0L
        ),
        duplicates = det[0],
        unique = det,
        duplicate_indices = integer(0)
      ))
    }
  }

  say("⚠️  Found ", format(n_dup, big.mark = ","), " duplicate records (",
      sprintf("%.2f%%", 100 * n_dup / nrow(det)), ") across ",
      format(n_unique_sets, big.mark = ","), " unique detection events.")

  # Create summary by source if available
  summary_dt <- NULL
  if ("source_csv" %in% names(det)) {
    summary_dt <- det[, .(
      n_total = .N,
      n_duplicates = sum(.is_dup),
      pct_duplicates = 100 * sum(.is_dup) / .N
    ), by = .(source_csv)][order(-n_duplicates)]

    if (isTRUE(verbose)) {
      say("\nDuplicate summary by source:")
      print(summary_dt[n_duplicates > 0])
    }
  } else {
    summary_dt <- data.table::data.table(
      n_total = nrow(det),
      n_duplicates = n_dup,
      pct_duplicates = 100 * n_dup / nrow(det)
    )
  }

  # Extract duplicates
  dup_records <- det[.is_dup == TRUE][order(Transmitter, `Date and Time (UTC)`)]
  dup_indices <- det[.is_dup == TRUE, .dup_id]

  # Create unique dataset (keep first occurrence)
  unique_det <- det[!duplicated(det, by = all_key_cols)]
  unique_det[, c(".dup_id", ".dup_n", ".is_dup") := NULL]

  say("✅ Deduplicated dataset has ", format(nrow(unique_det), big.mark = ","), " records.")

  # Clean up temporary columns from det
  det[, c(".dup_id", ".dup_n", ".is_dup") := NULL]

  # Return based on type
  if (return_type == "summary") {
    return(summary_dt)
  } else if (return_type == "duplicates") {
    dup_records[, c(".dup_id", ".dup_n", ".is_dup") := NULL]
    return(dup_records)
  } else if (return_type == "unique") {
    return(unique_det)
  } else {
    dup_records[, c(".dup_id", ".dup_n", ".is_dup") := NULL]
    return(list(
      summary = summary_dt,
      duplicates = dup_records,
      unique = unique_det,
      duplicate_indices = dup_indices
    ))
  }
}


#' Identify gaps in receiver coverage
#'
#' @description
#' Analyzes receiver deployment history to identify temporal gaps in coverage
#' that may indicate missing VRL files or incomplete downloads. Useful for
#' quality control and ensuring complete detection histories.
#'
#' @details
#' This function examines detection data across time for each receiver/station
#' combination and identifies periods of silence that exceed a specified threshold.
#' Gaps can indicate:
#' - Missing VRL files from certain download dates
#' - Receiver malfunction or battery failure
#' - Receiver removed from deployment
#' - Incomplete data downloads
#'
#' The function handles timezone conversions and can filter gaps by minimum duration
#' to focus on significant coverage issues rather than expected short-term silence
#' (e.g., low detection periods in natural systems).
#'
#' @param det A `data.table` of detection records with temporal information
#' @param datetime_col Character. Name of the datetime column. Default `"Date and Time (UTC)"`.
#' @param receiver_col Character. Name of the receiver ID column. Default `"Receiver"`.
#' @param station_col Character. Name of the station name column. Default `"Station Name"`.
#' @param min_gap_days Numeric. Minimum gap duration in days to report. Default `7`.
#' @param expected_end Optional. Character or Date. Expected end of monitoring period.
#'   If provided, identifies if coverage ends before this date (e.g., if you know
#'   receivers should be active through "2025-12-31" but data stops in October).
#' @param by_transmitter Logical. If `TRUE`, analyzes gaps separately for each transmitter
#'   at each station (useful for identifying tag-specific detection issues). Default `FALSE`.
#' @param transmitter_col Character. Name of transmitter column. Only used if `by_transmitter = TRUE`.
#'   Default `"Transmitter"`.
#' @param time.zone Character. Timezone for date conversions. Default `"UTC"`.
#' @param verbose Logical. Print diagnostic messages. Default `TRUE`.
#'
#' @import data.table
#' @importFrom lubridate with_tz
#' @return A `data.table` with identified gaps containing:
#' \describe{
#'   \item{Receiver}{Receiver serial number}
#'   \item{Station.Name}{Station name}
#'   \item{Transmitter}{Transmitter ID (only if `by_transmitter = TRUE`)}
#'   \item{gap_start}{Start date/time of gap}
#'   \item{gap_end}{End date/time of gap (or "ongoing" if no subsequent detections)}
#'   \item{gap_days}{Duration of gap in days}
#'   \item{det_before_gap}{Number of detections before the gap}
#'   \item{det_after_gap}{Number of detections after the gap (0 if ongoing)}
#'   \item{flag}{Character flag: "missing_data" or "ongoing" or "normal_gap"}
#' }
#'
#' @examples
#' \dontrun{
#' det_all <- load_detections_by_date("Logs_by_Date")
#'
#' # Find gaps > 7 days
#' gaps <- identify_coverage_gaps(det_all, min_gap_days = 7)
#'
#' # Look for gaps > 30 days, expecting coverage through 2025-12-31
#' gaps_long <- identify_coverage_gaps(
#'   det_all,
#'   min_gap_days = 30,
#'   expected_end = "2025-12-31"
#' )
#'
#' # Analyze gaps by transmitter (identifies if specific tags stop being detected)
#' gaps_by_tag <- identify_coverage_gaps(
#'   det_all,
#'   min_gap_days = 14,
#'   by_transmitter = TRUE
#' )
#' }
#'
#' @seealso [sort_VRL_logs()], [load_detections_by_date()], [check_duplicate_detections()]
#' @export
identify_coverage_gaps <- function(
    det,
    datetime_col = "Date and Time (UTC)",
    receiver_col = "Receiver",
    station_col = "Station Name",
    min_gap_days = 7,
    expected_end = NULL,
    by_transmitter = FALSE,
    transmitter_col = "Transmitter",
    time.zone = "UTC",
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  if (!inherits(det, "data.frame"))
    stop("`det` must be a data.frame or data.table.")

  det <- data.table::as.data.table(det)
  say <- function(...) if (isTRUE(verbose)) message(...)

  # Validate columns
  required_cols <- c(datetime_col, receiver_col, station_col)
  if (by_transmitter) required_cols <- c(required_cols, transmitter_col)
  missing_cols <- setdiff(required_cols, names(det))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure datetime is POSIXct
  if (!inherits(det[[datetime_col]], "POSIXct")) {
    say("Converting datetime column to POSIXct...")
    det[[datetime_col]] <- as.POSIXct(det[[datetime_col]])
  }

  # Convert timezone if needed
  if (time.zone != "UTC") {
    det[, dt_converted := lubridate::with_tz(get(datetime_col), tz = time.zone)]
  } else {
    det[, dt_converted := get(datetime_col)]
  }

  # Parse expected_end if provided
  exp_end_dt <- NULL
  if (!is.null(expected_end)) {
    if (inherits(expected_end, "Date")) {
      exp_end_dt <- as.POSIXct(paste(expected_end, "23:59:59"), tz = time.zone)
    } else {
      exp_end_dt <- as.POSIXct(expected_end, tz = time.zone)
    }
    if (is.na(exp_end_dt)) {
      warning("Could not parse `expected_end`. Ignoring this parameter.")
      exp_end_dt <- NULL
    }
  }

  say("Analyzing coverage gaps (minimum ", min_gap_days, " days)...")

  # Define grouping columns
  by_cols <- c(receiver_col, station_col)
  if (by_transmitter) by_cols <- c(by_cols, transmitter_col)

  # Sort and calculate time between consecutive detections
  data.table::setkeyv(det, c(by_cols, "dt_converted"))

  det[, prev_dt := data.table::shift(dt_converted, n = 1L, type = "lag"), by = by_cols]
  det[, gap_hours := as.numeric(difftime(dt_converted, prev_dt, units = "hours"))]
  det[, gap_days := gap_hours / 24]

  # Identify gaps exceeding threshold
  gaps_raw <- det[gap_days >= min_gap_days]

  if (nrow(gaps_raw) == 0) {
    say("✅ No coverage gaps >= ", min_gap_days, " days found.")
    return(data.table::data.table())
  }

  say("Found ", nrow(gaps_raw), " potential gap(s) >= ", min_gap_days, " days.")

  # Build gap summary
  gap_summary <- gaps_raw[, .(
    gap_start = prev_dt[1],
    gap_end = dt_converted[1],
    gap_days = gap_days[1],
    det_before_gap = .N  # This isn't quite right, will fix below
  ), by = c(by_cols, "prev_dt", "dt_converted")]

  # Get detection counts properly
  # Count detections before each gap start
  for (i in seq_len(nrow(gap_summary))) {
    grp_subset <- det
    for (col in by_cols) {
      grp_subset <- grp_subset[get(col) == gap_summary[i, get(col)]]
    }
    gap_summary[i, det_before_gap := grp_subset[dt_converted < gap_start, .N]]
    gap_summary[i, det_after_gap := grp_subset[dt_converted > gap_end, .N]]
  }

  gap_summary[, prev_dt := NULL]
  gap_summary[, dt_converted := NULL]

  # Flag ongoing gaps (no detections after gap)
  gap_summary[, flag := data.table::fcase(
    det_after_gap == 0, "ongoing",
    gap_days >= min_gap_days * 2, "missing_data",
    default = "normal_gap"
  )]

  # Check against expected_end if provided
  if (!is.null(exp_end_dt)) {
    # For each receiver/station, check if last detection is before expected end
    last_det <- det[, .(last_detection = max(dt_converted)), by = by_cols]
    early_end <- last_det[last_detection < exp_end_dt]

    if (nrow(early_end) > 0) {
      say("⚠️  ", nrow(early_end), " receiver(s) have coverage ending before expected date.")

      # Add these as additional gaps
      early_end[, gap_start := last_detection]
      early_end[, gap_end := exp_end_dt]
      early_end[, gap_days := as.numeric(difftime(gap_end, gap_start, units = "days"))]
      early_end[, flag := "ended_early"]
      early_end[, det_before_gap := NA_integer_]
      early_end[, det_after_gap := 0L]
      early_end[, last_detection := NULL]

      gap_summary <- data.table::rbindlist(list(gap_summary, early_end), fill = TRUE)
    }
  }

  # Clean up and sort
  data.table::setnames(gap_summary,
                       old = c(receiver_col, station_col),
                       new = c("Receiver", "Station.Name"),
                       skip_absent = TRUE)

  if (by_transmitter) {
    data.table::setnames(gap_summary, old = transmitter_col, new = "Transmitter", skip_absent = TRUE)
    data.table::setorder(gap_summary, Receiver, Station.Name, Transmitter, gap_start)
  } else {
    data.table::setorder(gap_summary, Receiver, Station.Name, gap_start)
  }

  say("✅ Coverage gap analysis complete. Found ", nrow(gap_summary), " gap(s).")

  return(gap_summary)
}

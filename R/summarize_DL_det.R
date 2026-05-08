#' Summarize newly downloaded detections by transmitter
#'
#' @description
#' Summarizes newly downloaded detection data which have been sorted into date-specific folders using
#' [sort_VRL_logs()]. You must provide either a data object of capture data (`data.frame`, `tibble`, or `data.table`)
#' or a vector of complete transmitter names.
#'
#' @details
#' - If providing `capture_data`, it ***MUST*** have a column named `"Transmitter"` populated with complete
#'   transmitter names (e.g., "A69-9001-57441") as well as a column named `"Date.Caught"` populated by the date
#'   in which the animal was caught.
#' - The `capture_data` object can have an optional column titled "Spp.Name" populated with the scientific names
#'   of the fish caught (e.g., "Pristis pectinata", "P. pectinata"). If present, the output data summary will be
#'   ordered alphabetically by species. If "pectinata" is present in the name, the order will have P. pectinata
#'   at the top, followed by the remaining species in alphabetical order.
#' - If `capture_data` is provided, all columns in `capture_data` are joined with the output `summary`.
#'
#' @param det_dir A directory path which houses folders of download logs/data named by date (see [sort_VRL_logs()]).
#' @param DL_dates An optional vector of download dates of interest. This must match the format of the names
#'   of the date-specific folders in the directory. If not provided, all downloads from the directory will be imported.
#' @param capture_data An optional `data.frame`, `tibble`, or `data.table` containing relevant capture information.
#'   ***MUST*** include a column titled `"Transmitter"` with the full transmitter names and a column titled `"Date.Caught"`.
#'   Note: `Date.Caught` does not need to be pre-formatted as a `Date` or `POSIXct` object.
#' @param time.zone Optional. The time zone of the capture_data. Defaults to "US/Eastern".
#' @param Date_format Optional. The `POSIX` format of the dates of capture. Defaults to `"%m/%d/%Y"`.
#' @param Transmitters An optional vector of full transmitter names. If capture_data is not supplied, this must be included.
#' @param tags.to.remove An optional vector including the full transmitter names of tags wished to be removed from the summary.
#'
#' @import data.table
#' @importFrom lubridate with_tz is.POSIXt
#' @return Returns a listed object containing:
#' \describe{
#'   \item{summary}{A `data.table` with columns `"n_detections"`, `"n_stations"`, `"first_det"`, and `"last_det"`}
#'   \item{detections}{A `data.table` with all detections associated with the specified `Transmitter` names}
#' }
#'
#' @examples
#' \dontrun{
#' setwd("G:/Charlotte Harbor Common/Sawfish/Acoustic Database Backups")
#' root_dir <- "2025 Receiver Logs"
#' det_dir <- "Logs by Date"
#'
#' # Import tag information
#' fish_ids <- data.table::fread("CSV Files/Acoustic Tagged Fish.csv")
#'
#' # Dates of interest
#' dates <- c("2025-02-28", "2025-05-16", "2025-09-26")
#'
#' # Tags correspond to known deceased fish
#' tags.to.remove <- c("A69-9001-54949", "A69-9001-60330", "A69-9001-54939")
#'
#' # Summarize and view data
#' det_summary <- summarize_DL_det(
#'   det_dir = det_dir,
#'   DL_dates = dates,
#'   capture_data = fish_ids,
#'   tags.to.remove = tags.to.remove
#' )
#'
#' # Optional reorder across additional columns
#' data.table::setorder(det_summary$summary, Spp.Name, ProjName, sex, Transmitter)
#'
#' # Save a csv
#' write.csv(det_summary$summary, row.names = FALSE,
#'           file = paste0(root_dir, "/detection_summary_", Sys.Date(), ".csv"))
#' }
#'
#' @seealso [sort_VRL_logs()], [load_detections_by_date()]
#' @export
summarize_DL_det <- function(
    det_dir,
    DL_dates,
    capture_data,
    time.zone,
    Date_format,
    Transmitters,
    tags.to.remove
) {

  # Validate input: must have either capture_data or Transmitters
  if (missing(capture_data) && missing(Transmitters)) {
    stop("You must supply either `capture_data` or a vector of `Transmitters`.")
  }

  # Load detections
  message("Loading detections. This may take a minute.")
  if (!missing(DL_dates)) {
    new_det <- load_detections_by_date(det_dir, dates = DL_dates)
  } else {
    new_det <- load_detections_by_date(det_dir)
  }

  if (missing(time.zone)) time.zone <- "US/Eastern"

  # Add converted time zone column
  new_det[, Date.Time := lubridate::with_tz(`Date and Time (UTC)`, tz = time.zone)]

  # Branch: if capture_data provided
  if (!missing(capture_data)) {
    if (!"Transmitter" %in% names(capture_data)) {
      stop("'capture_data' must have a column of full transmitter names titled 'Transmitter'.")
    }
    if (!"Date.Caught" %in% names(capture_data)) {
      stop("'capture_data' must have a column of dates titled 'Date.Caught'.")
    }

    capture_data <- data.table::as.data.table(capture_data)
    data.table::setcolorder(capture_data, c("Transmitter", setdiff(names(capture_data), "Transmitter")))

    if (missing(Date_format)) Date_format <- "%m/%d/%Y"

    # Clean transmitter names
    capture_data[, Transmitter := as.character(Transmitter)]
    capture_data[!is.na(Transmitter), Transmitter := trimws(Transmitter)]
    capture_data[Transmitter == "", Transmitter := NA_character_]

    # Format Date.Caught
    if (!lubridate::is.POSIXt(capture_data$Date.Caught)) {
      capture_data[, Date.Caught := as.POSIXct(Date.Caught, format = Date_format)]
    }

    # Handle species ordering if present
    if ("Spp.Name" %in% names(capture_data)) {
      if (grepl("pectinata", paste(capture_data[, Spp.Name], collapse = " "))) {
        ppec <- unique(capture_data[grepl("pectinata", Spp.Name), Spp.Name])
        spp.order <- c(ppec, sort(unique(capture_data$Spp.Name))[!sort(unique(capture_data$Spp.Name)) %in% ppec])
        capture_data[, Spp.Name := factor(Spp.Name, levels = spp.order)]
      } else {
        capture_data[, Spp.Name := factor(Spp.Name)]
      }
    }

    # Remove unwanted tags
    if (!missing(tags.to.remove)) {
      capture_data <- capture_data[!Transmitter %in% tags.to.remove]
    }

    trans_names <- sort(capture_data[!is.na(Transmitter), unique(Transmitter)])
    trans_det <- new_det[Transmitter %in% trans_names]

    # Summarize by transmitter
    by_trans <- trans_det[, .(
      n_detections = .N,
      n_stations = data.table::uniqueN(`Station Name`),
      first_det = min(Date.Time),
      last_det = max(Date.Time)
    ), by = .(Transmitter)][order(Transmitter)]

    # Join with capture data
    lk <- unique(capture_data[!is.na(Transmitter)])
    trans_summary <- lk[by_trans, on = "Transmitter"]

    # Reorder columns
    if ("Spp.Name" %in% names(capture_data)) {
      data.table::setcolorder(trans_summary,
                              c("Spp.Name", names(by_trans),
                                setdiff(names(trans_summary), c("Spp.Name", names(by_trans)))))
      data.table::setorder(trans_summary, Spp.Name, Transmitter)
    } else {
      data.table::setcolorder(trans_summary,
                              c(names(by_trans), setdiff(names(trans_summary), names(by_trans))))
    }

    return(list(summary = trans_summary, detections = trans_det))

  } else {
    # Branch: Transmitters vector provided
    if (!missing(tags.to.remove)) {
      Transmitters <- setdiff(Transmitters, tags.to.remove)
    }

    trans_det <- new_det[Transmitter %in% Transmitters]

    by_trans <- trans_det[, .(
      n_detections = .N,
      n_stations = data.table::uniqueN(`Station Name`),
      first_det = min(`Date and Time (UTC)`),
      last_det = max(`Date and Time (UTC)`)
    ), by = .(Transmitter)][order(Transmitter)]

    return(list(summary = by_trans, detections = trans_det))
  }
}

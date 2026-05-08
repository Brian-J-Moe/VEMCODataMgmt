#' Load & combine per-date detection bundles
#'
#' @description
#' Loads one or more per-date `.RData` bundles created by [sort_VRL_logs()]
#' (each date subfolder contains `detections_<date>.RData` with an
#' object named `det`) and returns a single combined `data.table`.
#'
#' @details
#' - If `dates` is `NULL`, the function discovers all subfolders in `dir`
#'   that look like `YYYY-MM-DD` and loads them (optionally filtered by
#'   `after_date` and `exclude_dates`).
#' - If `dates` is provided, it loads that set minus any in `exclude_dates`,
#'   and also applies `after_date` if given.
#' - For each selected date, it opens `dir/<date>/detections_<date>.RData`,
#'   extracts `det`, adds a `date_label` column (the folder name), and row-binds.
#' - Missing bundles are skipped with a message; if none are found, it errors.
#'
#' @param dir Character. Root folder where date subfolders live; e.g.
#'   the output of [sort_VRL_logs()]. Each date subfolder should contain
#'   `detections_<date>.RData`.
#' @param dates Character vector of date labels (e.g., `"2025-02-28"`),
#'   or `NULL` to auto-discover all date folders.
#' @param exclude_dates Character vector of date labels to exclude
#'   (e.g., `c("2025-02-28", "2025-05-16")`). Default `NULL`.
#' @param after_date Optional character `YYYY-MM-DD`. Keep only folders with
#'   dates >= this threshold. Default `NULL` (no lower bound).
#' @param verbose Logical; print progress/messages. Default: `TRUE`.
#'
#' @import data.table
#' @return A `data.table` with combined detections from all found dates.
#'   Adds a `date_label` column indicating the source folder (e.g., `"2025-02-28"`).
#'
#' @examples
#' \dontrun{
#' setwd("G:/Charlotte Harbor Common/Sawfish/Acoustic Database Backups")
#' root_dir <- "2025 Receiver Logs"
#'
#' # 1) Load only these dates:
#' det_some <- load_detections_by_date(
#'   root_dir,
#'   dates = c("2025-02-28", "2025-05-16")
#' )
#'
#' # 2) Load ALL available dates except a few:
#' det_all_but <- load_detections_by_date(
#'   root_dir,
#'   dates = NULL,
#'   exclude_dates = c("2025-02-28", "2025-09-26")
#' )
#'
#' # 3) Load ALL dates on/after 2025-05-01:
#' det_after <- load_detections_by_date(
#'   root_dir,
#'   after_date = "2025-05-01"
#' )
#' }
#'
#' @seealso [sort_VRL_logs()], [check_duplicate_detections()], [identify_coverage_gaps()]
#' @export
load_detections_by_date <- function(
    dir,
    dates = NULL,
    exclude_dates = NULL,
    after_date = NULL,
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Please install 'data.table' (install.packages('data.table'))")

  if (missing(dir) || !nzchar(dir))
    stop("`dir` must be a non-empty path.")
  if (!dir.exists(dir))
    stop("`dir` not found: ", dir)

  say <- function(...) if (isTRUE(verbose)) message(...)

  # Validate after_date if provided (corrected from original)
  thr <- NULL
  if (!is.null(after_date)) {
    if (!is.character(after_date) || length(after_date) != 1L ||
        !grepl("^\\d{4}-\\d{2}-\\d{2}$", after_date)) {
      stop("`after_date` must be a single string 'YYYY-MM-DD', e.g., '2025-05-01'.")
    }
    thr <- as.Date(after_date, "%Y-%m-%d")
    if (is.na(thr)) stop("`after_date` could not be parsed: ", after_date)
  }

  # Helper: identify YYYY-MM-DD folder names
  is_date_dir <- function(x) grepl("^\\d{4}-\\d{2}-\\d{2}$", x)

  # Discover dates if not provided
  if (is.null(dates)) {
    subdirs <- list.files(dir, full.names = FALSE, recursive = FALSE, include.dirs = TRUE)
    dates <- sort(subdirs[is_date_dir(subdirs)])
    if (!length(dates)) stop("No date-like subfolders (YYYY-MM-DD) found in: ", dir)
    say("Found ", length(dates), " date folder(s).")
  } else {
    if (!is.character(dates) || !length(dates))
      stop("`dates` must be a non-empty character vector or NULL.")
    bad <- !is_date_dir(dates)
    if (any(bad)) stop("These `dates` are not in 'YYYY-MM-DD' format: ", paste(dates[bad], collapse = ", "))
    dates <- sort(unique(dates))
  }

  # Apply after_date threshold
  if (!is.null(thr)) {
    dvals <- as.Date(dates, "%Y-%m-%d")
    keep_idx <- !is.na(dvals) & dvals >= thr
    if (!any(keep_idx)) stop("No date folders are >= ", format(thr, "%Y-%m-%d"), ".")
    drop <- dates[!keep_idx]
    dates <- dates[keep_idx]
    if (length(drop)) say("Excluding older than ", format(thr, "%Y-%m-%d"), ": ", paste(drop, collapse = ", "))
  }

  # Apply explicit exclusions
  if (!is.null(exclude_dates) && length(exclude_dates)) {
    bad_ex <- !is_date_dir(exclude_dates)
    if (any(bad_ex)) stop("These `exclude_dates` are not 'YYYY-MM-DD': ", paste(exclude_dates[bad_ex], collapse = ", "))
    keep <- setdiff(dates, exclude_dates)
    if (!length(keep)) stop("All requested dates were excluded; nothing to load.")
    if (length(keep) < length(dates)) {
      say("Excluding date(s): ", paste(setdiff(dates, keep), collapse = ", "))
    }
    dates <- keep
  }

  # Build expected file paths
  paths <- file.path(dir, dates, paste0("detections_", dates, ".RData"))
  exists_idx <- file.exists(paths)

  if (!any(exists_idx)) {
    stop(
      "None of the selected per-date RData files were found.\n",
      "Looked for:\n", paste(paths, collapse = "\n")
    )
  }

  if (any(!exists_idx)) {
    say("⚠️  Missing bundles for: ", paste(dates[!exists_idx], collapse = ", "))
  }

  det_list <- list()
  for (i in which(exists_idx)) {
    f <- paths[i]
    env <- new.env(parent = emptyenv())
    loaded <- tryCatch(load(f, envir = env), error = function(e) {
      say("⚠️  Error loading ", f, ": ", e$message)
      character()
    })
    if (!"det" %in% loaded) {
      say("⚠️  No 'det' object in ", f, " — skipping.")
      next
    }
    dt <- get("det", envir = env)
    if (!inherits(dt, "data.frame")) {
      say("⚠️  'det' in ", f, " isn't a data.frame — skipping.")
      next
    }
    dt <- data.table::as.data.table(dt)
    if (!"date_label" %in% names(dt)) dt[, date_label := dates[i]]
    det_list[[length(det_list) + 1L]] <- dt
  }

  if (!length(det_list)) {
    stop("No valid 'det' objects were found in the selected files.")
  }

  det_all <- data.table::rbindlist(det_list, fill = TRUE, use.names = TRUE)

  # Put date_label as last column if present
  if ("date_label" %in% names(det_all)) {
    data.table::setcolorder(det_all, c(setdiff(names(det_all), "date_label"), "date_label"))
  }

  say("✅ Combined rows: ", format(nrow(det_all), big.mark = ","))

  det_all
}

#' Parse VRL/CSV filename metadata
#'
#' @description
#' Internal helper to extract serial number and download date from
#' standardized Fathom VRL/CSV filenames.
#'
#' @param filename Character vector of filenames
#' @param strict_serial_digits Minimum digits required for serial number (default 6)
#'
#' @return A `data.table` with columns: serial, dl_date_chr, dl_date, valid_serial, valid_date
#' @noRd
parse_vrl_filename <- function(filename, strict_serial_digits = 6) {
  serial_pattern <- sprintf("_(\\d{%d,})_\\d{8}", strict_serial_digits)
  date_pattern <- "_(\\d{8})"

  serial <- sub(sprintf("^.*?_(\\d{%d,})_\\d{8}.*$", strict_serial_digits),
                "\\1", filename, perl = TRUE)
  date_chr <- sub("^.*?_(\\d{8}).*$", "\\1", filename, perl = TRUE)

  valid_serial <- grepl(serial_pattern, filename)
  valid_date <- grepl(date_pattern, filename)

  data.table::data.table(
    serial = ifelse(valid_serial, serial, NA_character_),
    dl_date_chr = ifelse(valid_date, date_chr, NA_character_),
    dl_date = suppressWarnings(as.Date(ifelse(valid_date, date_chr, NA_character_), "%Y%m%d")),
    valid_serial = valid_serial,
    valid_date = valid_date
  )
}


#' Sort existing VRL & CSV files by download date
#'
#' @description
#' Reads a folder with VEMCO/Innovasea receiver log files (both `.vrl` and `.csv`).
#' Files are sorted into date-specific subfolders under `out_dir`
#' based on the `YYYYMMDD` date of download in the filename (e.g. log VR2Tx_480535_***20250228***_1.vrl
#' was downloaded on 02/28/2025).
#' In each subfolder, all CSVs are combined and saved to `detections_<date>.RData`.
#' Finally, all detections across all dates are combined and saved to
#' `detections_all_YYYY-MM-DD.RData` (by default) in `out_dir`.
#'
#' @param vrl_dir Directory containing `.vrl` and/or `.csv` files.
#' @param out_dir Destination directory that will receive date-named subfolders and combined RData.
#' @param target_serials Optional character vector of serials to include (e.g., `c("110730", "480535")`). `NULL` = all.
#' @param include_dates Optional character vector of `YYYY-MM-DD` strings to include. `NULL` = all.
#' @param after_date Optional `YYYY-MM-DD`; keep files with date >= this. `NULL` = no lower bound.
#' @param strict_serial_digits Minimal digits for serial in filename (default 6; use 5 if needed).
#' @param overwrite_date_folders If `TRUE`, delete any existing date subfolder in `out_dir` before writing.
#' @param date_label_format Format for subfolder labels. Default is `%Y-%m-%d` (e.g., 2025-02-28).
#' @param combined_rdata_name Filename for the combined bundle in `out_dir`.
#'   Default `paste0("detections_all_", Sys.Date(), ".RData")`.
#' @param verbose Logical; print progress messages and bars (default `TRUE`).
#' @param show_timing Logical; print per-phase and total elapsed times (default `TRUE`).
#'
#' @import data.table
#'
#' @return A list with components:
#' \describe{
#'   \item{meta_all}{`data.table` of all discovered files with parsed dates/serials and keep/drop flags.}
#'   \item{meta_kept}{Subset of `meta_all` after filters (serials/dates) applied.}
#'   \item{out_dir}{Output directory path used.}
#'   \item{dates}{Character vector of date labels (subfolders) created.}
#'   \item{n_detections}{Total number of detection records across all dates.}
#'   \item{failed_files}{Character vector of files that failed to read (if any).}
#' }
#'
#' @examples
#' \dontrun{
#' setwd("G:/Charlotte Harbor Common/Sawfish/Acoustic Database Backups/2025 Receiver Logs")
#'
#' result <- sort_VRL_logs(
#'   vrl_dir = "ReceiverLogs_2025_29Sept2025",
#'   out_dir = "Logs_by_Date",
#'   target_serials = c("110730", "480535"),
#'   after_date = "2025-01-01",
#'   strict_serial_digits = 6,
#'   overwrite_date_folders = FALSE
#' )
#' }
#'
#' @seealso [load_detections_by_date()], [check_duplicate_detections()], [identify_coverage_gaps()]
#' @export
sort_VRL_logs <- function(
    vrl_dir,
    out_dir,
    target_serials = NULL,
    include_dates = NULL,
    after_date = NULL,
    strict_serial_digits = 6,
    overwrite_date_folders = TRUE,
    date_label_format = "%Y-%m-%d",
    combined_rdata_name = sprintf("detections_all_%s.RData", format(Sys.Date(), "%Y-%m-%d")),
    verbose = TRUE,
    show_timing = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required. install.packages('data.table')")

  if (!dir.exists(vrl_dir)) stop("vrl_dir not found: ", vrl_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  say <- function(...) if (isTRUE(verbose)) message(...)
  bar <- function(n) if (isTRUE(verbose)) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
  tick <- function(pb, i) if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  done <- function(pb) if (!is.null(pb)) close(pb)

  fmt_dur <- function(sec) {
    sec <- as.numeric(sec)
    h <- sec %/% 3600; m <- (sec %% 3600) %/% 60; s <- round(sec %% 60, 1)
    parts <- c(if (h > 0) sprintf("%dh", h), if (m > 0) sprintf("%dm", m), sprintf("%0.1fs", s))
    paste(parts[!is.na(parts)], collapse = " ")
  }

  phase_time <- function(expr, label) {
    t0 <- Sys.time()
    val <- force(expr)
    t1 <- Sys.time()
    if (isTRUE(show_timing)) say(sprintf("   ⏱️  %s: %s", label, fmt_dur(as.numeric(difftime(t1, t0, units = "secs")))))
    invisible(val)
  }

  t_start <- Sys.time()
  if (isTRUE(show_timing)) say("⏱️  Started: ", format(t_start), " (", Sys.info()[["nodename"]], ")")

  # Track failed files
  failed_files <- character(0)

  # ---- 1) Discover files: VRL + CSV ----
  say("\n🔎 Scanning for .vrl and .csv in: ", vrl_dir)
  files <- list.files(vrl_dir, pattern = "(?i)\\.(vrl|csv)$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No .vrl or .csv files found in: ", vrl_dir)

  b <- basename(files)
  ext <- tolower(tools::file_ext(b))

  # Use centralized parsing
  parsed <- parse_vrl_filename(b, strict_serial_digits)

  meta_all <- data.table::data.table(
    src_path = files,
    file_name = b,
    ext = ext
  )
  meta_all <- cbind(meta_all, parsed)

  # ---- 2) Filters (apply to BOTH VRL & CSV) ----
  keep <- rep(TRUE, nrow(meta_all))

  if (!is.null(target_serials)) {
    keep <- keep & !is.na(meta_all$serial) & meta_all$serial %in% target_serials
  }

  if (!is.null(include_dates)) {
    include_dates_clean <- gsub("-", "", include_dates)
    keep <- keep & !is.na(meta_all$dl_date_chr) & meta_all$dl_date_chr %in% include_dates_clean
  }

  if (!is.null(after_date)) {
    if (!is.character(after_date) || length(after_date) != 1)
      stop("`after_date` must be a single string 'YYYY-MM-DD'.")
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", after_date))
      stop("`after_date` must be 'YYYY-MM-DD' format, e.g., '2025-01-01'.")
    thr <- as.Date(after_date, "%Y-%m-%d")
    if (is.na(thr)) stop("`after_date` could not be parsed: ", after_date)
    keep <- keep & !is.na(meta_all$dl_date) & meta_all$dl_date >= thr
  }

  meta_kept <- meta_all[keep]
  if (!nrow(meta_kept)) stop("No files matched filters in: ", vrl_dir)

  # ---- 3) Date labels & prepare subfolders ----
  meta_kept[, date_label := ifelse(is.na(dl_date), "unknown_date", format(dl_date, date_label_format))]
  labels <- unique(meta_kept[order(is.na(dl_date), dl_date, date_label), date_label])

  phase_time({
    say("\n🗂️  Preparing ", length(labels), " date folder(s) in: ", out_dir)
    pb <- bar(length(labels)); i <- 0L
    for (lab in labels) {
      subdir <- file.path(out_dir, lab)
      if (isTRUE(overwrite_date_folders) && dir.exists(subdir)) {
        unlink(subdir, recursive = TRUE, force = TRUE)
      }
      dir.create(subdir, showWarnings = FALSE, recursive = TRUE)
      i <- i + 1L; tick(pb, i)
    }
    done(pb)
  }, "Prepare folders")

  # ---- 4) Copy files into date folders ----
  phase_time({
    say("\n📦 Copying files into date folders...")
    total_copied <- 0L
    for (lab in labels) {
      subdir <- file.path(out_dir, lab)
      rows <- meta_kept[date_label == lab][order(dl_date, file_name)]
      n <- nrow(rows)
      say(sprintf("  • %s: %d file(s) to copy", lab, n))
      if (n > 0) {
        pb <- bar(n); j <- 0L
        ok <- logical(n)
        for (k in seq_len(n)) {
          ok[k] <- suppressWarnings(
            file.copy(rows$src_path[k], to = subdir,
                      overwrite = TRUE, copy.mode = FALSE, copy.date = FALSE))
          j <- j + 1L; tick(pb, j)
        }
        done(pb)
        copied_n <- sum(ok)
        total_copied <- total_copied + copied_n
        say(sprintf("    ✅ %s complete (%d/%d copied).\n", lab, copied_n, n))
      } else {
        say(sprintf("    ⚠️  %s has no files to copy.", lab))
      }
    }
    say("• Total copied: ", total_copied, " file(s).")
  }, "Copy files")

  # Helper to find detection CSVs in a folder
  find_csvs <- function(path) {
    pat <- "(?i)^[A-Za-z0-9]+_\\d{5,}_\\d{8}_\\d+\\.csv$"
    list.files(path, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  }

  # ---- 5) For each date folder: combine CSVs -> detections_<date>.RData ----
  phase_time({
    say("\n💾 Writing per-date RData files...\n")
    pb <- bar(length(labels)); i <- 0L
    for (lab in labels) {
      subdir <- file.path(out_dir, lab)
      csvs <- find_csvs(subdir)
      if (length(csvs)) {
        det_list <- list()
        for (f in csvs) {
          dt <- tryCatch(
            data.table::fread(f, fill = TRUE),
            error = function(e) {
              if (isTRUE(verbose)) message("    ⚠️  Failed to read ", basename(f), ": ", e$message)
              failed_files <<- c(failed_files, f)
              data.table::data.table()
            }
          )
          if (!nrow(dt)) next

          bb <- basename(f)
          vrl_base <- sub("(?i)\\.csv$", "", bb)
          parsed_meta <- parse_vrl_filename(vrl_base, strict_serial_digits)

          dt[, `:=`(
            source_csv = bb,
            serial = parsed_meta$serial,
            dl_date_chr = parsed_meta$dl_date_chr,
            dl_date = parsed_meta$dl_date
          )]
          det_list[[length(det_list) + 1L]] <- dt
        }

        if (length(det_list)) {
          det_d <- data.table::rbindlist(det_list, fill = TRUE)
          per_date_path <- file.path(subdir, sprintf("detections_%s.RData", lab))
          det <- det_d
          save(det, csvs, file = per_date_path)
        }
      }
      i <- i + 1L; tick(pb, i)
    }
    done(pb)
  }, "Per-date RData")

  # ---- 6) Build combined detections across ALL dates ----
  phase_time({
    say("\n📊 Building combined detections (all dates)...\n")
    all_csvs <- unlist(lapply(file.path(out_dir, labels), find_csvs), use.names = FALSE)
    det_all <- NULL

    if (length(all_csvs)) {
      det_list_all <- list()
      for (f in all_csvs) {
        dt <- tryCatch(
          data.table::fread(f, fill = TRUE),
          error = function(e) {
            if (isTRUE(verbose)) message("    ⚠️  Failed to read ", basename(f), ": ", e$message)
            failed_files <<- c(failed_files, f)
            data.table::data.table()
          }
        )
        if (!nrow(dt)) next

        bb <- basename(f)
        vrl_base <- sub("(?i)\\.csv$", "", bb)
        parsed_meta <- parse_vrl_filename(vrl_base, strict_serial_digits)

        dt[, `:=`(
          source_csv = bb,
          serial = parsed_meta$serial,
          dl_date_chr = parsed_meta$dl_date_chr,
          dl_date = parsed_meta$dl_date
        )]
        det_list_all[[length(det_list_all) + 1L]] <- dt
      }

      if (length(det_list_all)) {
        det_all <- data.table::rbindlist(det_list_all, fill = TRUE)
        save(det_all, all_csvs, file = file.path(out_dir, combined_rdata_name))
        say("✅ Combined detections saved: ", file.path(out_dir, combined_rdata_name),
            " (rows: ", format(nrow(det_all), big.mark = ","), ")")
      }
    } else {
      say("⚠️  No CSVs found to combine.")
    }
  }, "Combined RData")

  if (length(failed_files) > 0) {
    say("\n⚠️  ", length(failed_files), " file(s) failed to read. See $failed_files in output.")
  }

  if (isTRUE(show_timing)) {
    t_end <- Sys.time()
    total_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))
    say("\n⏱️  Finished: ", format(t_end), "  •  Elapsed: ", fmt_dur(total_sec))
  }

  invisible(list(
    meta_all = meta_all,
    meta_kept = meta_kept,
    out_dir = out_dir,
    dates = labels,
    n_detections = if (exists("det_all") && !is.null(det_all)) nrow(det_all) else 0L,
    failed_files = if (length(failed_files)) failed_files else NULL
  ))
}

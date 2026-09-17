#!/usr/bin/env Rscript

# ============================================================
# Download Mindat localities and locality-geomaterial links
#
# This script downloads locality records from the Mindat API v1
# using paginated requests with expand = "~all". For each locality,
# it retains the locality ID, locality name, associated geomaterial
# IDs, and the number of associated geomaterials.
#
# The script supports retries, per-page caching, logging, and resume
# after interruption. After downloading, all cached pages are merged
# and a long-format locality-geomaterial link table is generated.
#
# Main final outputs:
#   localities_all_YYYYMMDD.rds / .csv.gz
#   locality_geomaterial_links_YYYYMMDD.rds / .csv.gz
#
# Intermediate page_*.rds and page_*.csv.gz files are caches used
# for restart/resume and do not need to be distributed with the
# final dataset.
#
# Requirements: R packages httr2 and jsonlite; a valid Mindat API
# token stored in the environment variable MINDAT_API_TOKEN.
# ============================================================


suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

# =========================
# 1. User settings
# =========================
token <- Sys.getenv("MINDAT_API_TOKEN") #Copy you Mindat API key here!
if (!nzchar(token)) stop("MINDAT_API_TOKEN is not set. Please store your Mindat API token in this environment variable before running the script.")
# Date stamp used in output directory and final filenames.
today <- format(Sys.Date(), "%Y%m%d")
out_dir <- file.path("data_raw",
                     paste0("locality_pages_", today))

log_dir <- file.path("data_raw",
                     paste0("logs_", today))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)


# Number of locality records requested per API page.
page_size <- 500
start_page <- 1
end_page <- NULL
# Approximate total number of localities used only for progress/ETA estimation.
# The download itself stops according to the API response (res[["next"]]).
total_localities <- 418996

max_retries <- 5
retry_sleep <- 5
page_pause <- 1

success_log <- file.path(log_dir, "downloaded_pages.txt")
failed_log  <- file.path(log_dir, "failed_pages.txt")
run_log     <- file.path(log_dir, "run_log.txt")

# =========================
# 2. Helper functions
# =========================
log_message <- function(...) {
  msg <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = run_log, append = TRUE)
}

append_line <- function(path, x) {
  cat(x, "\n", file = path, append = TRUE)
}

read_logged_pages <- function(path) {
  if (!file.exists(path)) return(integer(0))
  x <- readLines(path, warn = FALSE)
  x <- trimws(x)
  x <- x[nzchar(x)]
  suppressWarnings(as.integer(x))
}

format_seconds <- function(sec) {
  if (is.na(sec) || !is.finite(sec)) return("NA")
  sec <- round(sec)
  h <- sec %/% 3600
  m <- (sec %% 3600) %/% 60
  s <- sec %% 60
  sprintf("%02dh:%02dm:%02ds", h, m, s)
}

estimate_total_pages <- function(total_localities, page_size) {
  ceiling(total_localities / page_size)
}

# Convert one API page into a compact locality table.
# geomaterial_ids are stored as semicolon-separated Mindat geomaterial IDs.
extract_page_df <- function(results) {
  if (is.null(results) || length(results) == 0) {
    return(data.frame(
      locality_id = integer(0),
      locality_name = character(0),
      geomaterial_ids = character(0),
      n_geomaterials = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  
  rows <- lapply(results, function(z) {
    ids <- z$geomaterials
    
    if (is.null(ids)) {
      ids_chr <- ""
      n_geom <- 0L
    } else {
      ids_vec <- sort(unique(as.integer(unlist(ids, use.names = FALSE))))
      ids_vec <- ids_vec[!is.na(ids_vec)]
      ids_chr <- paste(ids_vec, collapse = ";")
      n_geom <- length(ids_vec)
    }
    
    data.frame(
      locality_id = as.integer(z$id),
      locality_name = as.character(z$txt),
      geomaterial_ids = ids_chr,
      n_geomaterials = as.integer(n_geom),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows)
}

# Request one page from the Mindat localities endpoint.
# expand = "~all" is required here to expose the geomaterials field.
fetch_one_page <- function(page, page_size, token, max_retries = 5, retry_sleep = 5) {
  base_url <- "https://api.mindat.org/v1/localities/"
  
  for (attempt in seq_len(max_retries)) {
    res <- try({
      req <- request(base_url) |>
        req_url_query(
          expand = "~all",
          page = page,
          `page-size` = page_size,
          format = "json"
        ) |>
        req_headers(
          Authorization = paste("Token", token)
        ) |>
        req_timeout(300)
      
      resp <- req_perform(req)
      txt <- resp_body_string(resp)
      fromJSON(txt, simplifyVector = FALSE)
    }, silent = TRUE)
    
    if (!inherits(res, "try-error")) {
      return(res)
    }
    
    log_message("Page ", page, " attempt ", attempt, " failed.")
    if (attempt < max_retries) Sys.sleep(retry_sleep * attempt)
  }
  
  return(NULL)
}

# Save each page immediately in both RDS and compressed CSV formats.
# These files also serve as restart/resume checkpoints.
save_page <- function(df, page, out_dir) {
  rds_file <- file.path(out_dir, sprintf("page_%06d.rds", page))
  csv_file <- file.path(out_dir, sprintf("page_%06d.csv.gz", page))
  
  saveRDS(df, rds_file)
  write.csv(df, gzfile(csv_file), row.names = FALSE)
}

# =========================
# 3. Initialization
# =========================
# Previously completed pages are read from the success log so an interrupted
# download can resume without downloading those pages again.
downloaded_pages <- read_logged_pages(success_log)
estimated_total_pages <- estimate_total_pages(total_localities, page_size)

page_times <- numeric(0)
overall_start <- Sys.time()

log_message("==============================================")
log_message("Start download locality -> geomaterials")
log_message("page_size = ", page_size)
log_message("estimated total localities = ", total_localities)
log_message("estimated total pages = ", estimated_total_pages)
log_message("start_page = ", start_page)
log_message("end_page = ", ifelse(is.null(end_page), "NULL", end_page))
log_message("already downloaded pages = ", length(downloaded_pages))
log_message("==============================================")

# =========================
# 4. Main download loop
# =========================
page <- start_page

while (TRUE) {
  if (!is.null(end_page) && page > end_page) {
    log_message("Reached end_page = ", end_page, ". Stop.")
    break
  }
  
  if (page %in% downloaded_pages) {
    log_message("Page ", page, " already downloaded. Skip.")
    page <- page + 1
    next
  }
  
  page_start <- Sys.time()
  log_message("Fetching page ", page, " ...")
  
  res <- fetch_one_page(page, page_size, token, max_retries, retry_sleep)
  page_fetch_end <- Sys.time()
  
  if (is.null(res)) {
    log_message("Page ", page, " failed after all retries.")
    append_line(failed_log, page)
    page <- page + 1
    next
  }
  
  df <- extract_page_df(res$results)
  
  if (nrow(df) == 0) {
    log_message("Page ", page, " returned 0 rows. Stop.")
    break
  }
  
  save_page(df, page, out_dir)
  append_line(success_log, page)
  
  page_end <- Sys.time()
  elapsed_page <- as.numeric(difftime(page_end, page_start, units = "secs"))
  fetch_time   <- as.numeric(difftime(page_fetch_end, page_start, units = "secs"))
  total_time   <- as.numeric(difftime(page_end, overall_start, units = "secs"))
  
  page_times <- c(page_times, elapsed_page)
  avg_time_per_page <- mean(page_times)
  remaining_pages <- max(estimated_total_pages - page, 0)
  eta_seconds <- remaining_pages * avg_time_per_page
  
  log_message(
    "Saved page ", page,
    " | rows = ", nrow(df),
    " | locality_id range = [", df$locality_id[1], ", ", df$locality_id[nrow(df)], "]",
    " | fetch_time = ", round(fetch_time, 2), " s",
    " | page_time = ", round(elapsed_page, 2), " s"
  )
  
  log_message(
    "Progress: page ", page, "/", estimated_total_pages,
    " | avg_page_time = ", round(avg_time_per_page, 2), " s",
    " | elapsed_total = ", format_seconds(total_time),
    " | ETA = ", format_seconds(eta_seconds)
  )
  
  next_url <- res[["next"]]
  if (is.null(next_url) || !nzchar(next_url)) {
    log_message("No next page. Finished.")
    break
  }
  
  if (page_pause > 0) Sys.sleep(page_pause)
  page <- page + 1
}

overall_elapsed <- as.numeric(difftime(Sys.time(), overall_start, units = "secs"))
log_message("==============================================")
log_message("Run finished.")
log_message("Start merging all pages...")

# Merge all per-page RDS cache files into one final locality table.
rds_files <- list.files(
  out_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

rds_files <- sort(rds_files)

all_localities <- do.call(
  rbind,
  lapply(rds_files, readRDS)
)

merge_file <- file.path(
  out_dir,
  paste0("localities_all_", today, ".rds")
)

saveRDS(all_localities, merge_file)

csv_file <- file.path(
  out_dir,
  paste0("localities_all_", today, ".csv.gz")
)

write.csv(
  all_localities,
  gzfile(csv_file),
  row.names = FALSE
)

log_message(
  "Merged ",
  nrow(all_localities),
  " localities."
)
log_message("Building locality-geomaterial table...")

# Expand the semicolon-separated geomaterial IDs into a long-format
# locality_id <-> geomaterial_id association table.
split_ids <- strsplit(
  all_localities$geomaterial_ids,
  ";",
  fixed = TRUE
)

link_table <- data.frame(
  locality_id = rep(
    all_localities$locality_id,
    lengths(split_ids)
  ),
  geomaterial_id = as.integer(unlist(split_ids)),
  stringsAsFactors = FALSE
)

link_file <- file.path(
  out_dir,
  paste0("locality_geomaterial_links_", today, ".rds")
)

saveRDS(link_table, link_file)

write.csv(
  link_table,
  gzfile(
    file.path(
      out_dir,
      paste0("locality_geomaterial_links_", today, ".csv.gz")
    )
  ),
  row.names = FALSE
)

log_message(
  "Built ",
  nrow(link_table),
  " locality-geomaterial links."
)
log_message("Elapsed total = ", format_seconds(overall_elapsed))
if (length(page_times) > 0) {
  log_message("Mean page time = ", round(mean(page_times), 2), " s")
  log_message("Median page time = ", round(median(page_times), 2), " s")
}
log_message("==============================================")
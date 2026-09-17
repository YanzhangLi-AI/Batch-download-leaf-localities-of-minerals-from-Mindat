# ============================================================
# Leaf-locality filtering for a mineral list with Mindat IDs
#
# Inputs:
#   1. locality_geomaterial_links_20260725.rds
#      A long table containing locality_id and geomaterial_id.
#   2. A mineral list containing mineral names and Mindat
#      geomaterial IDs (for example, mineral_with_mindat_ID.csv).
#
# Leaf-locality exclusion rules:
#   Rule 1. Exclude localities with non_hierarchical == 1.
#   Rule 2. Among hierarchical localities for the SAME mineral,
#           exclude a locality if its normalized revtxtd is a
#           strict complete-path prefix of another, more specific
#           locality where that mineral is also recorded.
#   Rule 3. Exclude records with missing/empty locality_name
#           or revtxtd.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
  library(httr2)
  library(jsonlite)
})

# ============================================================
# 1. User settings
# ============================================================

run_date <- format(Sys.Date(), "%Y%m%d")

# Final locality-geomaterial link table generated in the
# locality-download step.
locality_geomaterial_link_file <-
  "locality_geomaterial_links_20260725.rds"

# Mineral list containing mineral names and Mindat IDs.
# Change only this file name when applying the workflow to
# another mineral subset.
mineral_name_file <- "mineral_with_mindat_ID.csv"

# Run settings.
resume_from_checkpoints <- TRUE
recompute_corrupt_checkpoint <- TRUE
verbose_locality_progress <- FALSE

# Write a progress message after this many newly computed minerals.
progress_log_every <- 25L

# Mindat API settings.
api_base_url <- "https://api.mindat.org/v1"
token <- "MINDAT_API_TOKEN" #Copy you Mindat API here!

# API retry settings.
max_retries <- 5L
retry_sleep_seconds <- 3
request_pause_seconds <- 0.05

# TRUE: ignore existing metadata cache and request the API again.
# FALSE: reuse cached locality metadata whenever available.
force_refresh_metadata <- FALSE

# Keep the existing metadata cache directory so previously
# downloaded locality metadata can be reused.
metadata_cache_dir <- file.path(
  "data_raw",
  "locality_metadata_cache_20260725"
)

# IMPORTANT:
# Use a new run directory because the filtering rules changed.
# Old checkpoints may contain results from the former locsinclude rule
# and should not be mixed with this three-rule workflow.
run_root_dir <- file.path(
  "leaf_check_results",
  "leaf_localities_three_rules"
)

checkpoint_dir <- file.path(
  run_root_dir,
  "mineral_checkpoints"
)

log_dir <- file.path(
  run_root_dir,
  "logs"
)

output_dir <- file.path(
  run_root_dir,
  "final_outputs"
)

main_log_file <- file.path(
  log_dir,
  "leaf_locality_main.log"
)

error_log_file <- file.path(
  log_dir,
  "leaf_locality_errors.log"
)

status_file <- file.path(
  run_root_dir,
  "mineral_run_status.csv"
)

dir.create(
  metadata_cache_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  log_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 2. Logging and safe-write helper functions
# ============================================================

log_message <- function(
    ...,
    level = "INFO",
    console = TRUE,
    log_file = main_log_file
) {

  text <- paste0(..., collapse = "")

  line <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " [", level, "] ",
    text
  )

  cat(line, "\n", file = log_file, append = TRUE)

  if (isTRUE(console)) {
    cat(line, "\n")
    flush.console()
  }

  invisible(line)
}

log_error <- function(...) {

  text <- paste0(..., collapse = "")

  log_message(
    text,
    level = "ERROR",
    console = TRUE,
    log_file = main_log_file
  )

  log_message(
    text,
    level = "ERROR",
    console = FALSE,
    log_file = error_log_file
  )
}

safe_write_csv <- function(x, path, na = "") {

  temporary_path <- paste0(path, ".tmp")

  write_csv(
    x,
    temporary_path,
    na = na
  )

  if (file.exists(path)) {
    file.remove(path)
  }

  if (!file.rename(temporary_path, path)) {
    stop("Failed to rename temporary file: ", path)
  }

  invisible(path)
}

safe_save_rds <- function(object, path) {

  temporary_path <- paste0(path, ".tmp")

  saveRDS(
    object,
    temporary_path
  )

  if (file.exists(path)) {
    file.remove(path)
  }

  if (!file.rename(temporary_path, path)) {
    stop("Failed to rename temporary file: ", path)
  }

  invisible(path)
}

checkpoint_path_for <- function(geomaterial_id) {

  file.path(
    checkpoint_dir,
    sprintf(
      "geomaterial_%09d.rds",
      as.integer(geomaterial_id)
    )
  )
}

format_elapsed <- function(seconds) {

  if (is.na(seconds) || !is.finite(seconds)) {
    return(NA_character_)
  }

  seconds <- max(0, round(seconds))

  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  secs <- seconds %% 60

  sprintf(
    "%02d:%02d:%02d",
    hours,
    minutes,
    secs
  )
}

# ============================================================
# 3. Check inputs
# ============================================================

if (!file.exists(locality_geomaterial_link_file)) {
  stop(
    "Cannot find locality-geomaterial link table:\n",
    locality_geomaterial_link_file,
    "\nCurrent working directory:\n",
    getwd()
  )
}

if (!file.exists(mineral_name_file)) {
  stop(
    "Cannot find mineral file:\n",
    mineral_name_file,
    "\nCurrent working directory:\n",
    getwd()
  )
}

if (!nzchar(token)) {
  stop(
    "MINDAT_API_TOKEN was not detected.\n\n",
    "Set it before running, for example:\n",
    'Sys.setenv(MINDAT_API_TOKEN = "YOUR_TOKEN")'
  )
}

log_message("=================================================")
log_message("Leaf-locality three-rule run")
log_message(
  "Locality-geomaterial link table: ",
  normalizePath(locality_geomaterial_link_file)
)
log_message(
  "Mineral file: ",
  normalizePath(mineral_name_file)
)
log_message(
  "Metadata cache: ",
  normalizePath(metadata_cache_dir)
)
log_message(
  "Checkpoint directory: ",
  normalizePath(checkpoint_dir)
)
log_message(
  "Output directory: ",
  normalizePath(output_dir)
)
log_message(
  "Resume from checkpoints: ",
  resume_from_checkpoints
)
log_message("=================================================")

capture.output(
  sessionInfo(),
  file = file.path(
    log_dir,
    paste0("sessionInfo_", run_date, ".txt")
  )
)

# ============================================================
# 4. Read the final locality-geomaterial link table directly
# ============================================================

locality_geomaterial_links <- readRDS(
  locality_geomaterial_link_file
)

required_link_columns <- c(
  "locality_id",
  "geomaterial_id"
)

missing_link_columns <- setdiff(
  required_link_columns,
  names(locality_geomaterial_links)
)

if (length(missing_link_columns) > 0) {
  stop(
    "The locality-geomaterial link table is missing required column(s): ",
    paste(missing_link_columns, collapse = ", ")
  )
}

# Keep the same basic cleaning logic as the original workflow.
locality_geomaterial_links <- locality_geomaterial_links %>%
  transmute(
    locality_id = suppressWarnings(
      as.integer(locality_id)
    ),
    geomaterial_id = suppressWarnings(
      as.integer(geomaterial_id)
    )
  ) %>%
  filter(
    !is.na(locality_id),
    !is.na(geomaterial_id)
  ) %>%
  distinct(
    locality_id,
    geomaterial_id
  )

cat(
  "Unique locality-geomaterial links: ",
  format(
    nrow(locality_geomaterial_links),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Unique locality IDs: ",
  format(
    n_distinct(locality_geomaterial_links$locality_id),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Unique geomaterial IDs: ",
  format(
    n_distinct(locality_geomaterial_links$geomaterial_id),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)

# ============================================================
# 5. Read the mineral list with Mindat IDs
# ============================================================

mineral_raw <- read_csv(
  mineral_name_file,
  show_col_types = FALSE
)

cat("Mineral-file columns:\n")
print(names(mineral_raw))

# Automatically recognize common Mindat ID column names.
id_candidates <- intersect(
  c(
    "mindat_ID",
    "mindat_id",
    "Mindat_ID",
    "Mindat ID",
    "Mindat.ID",
    "geomaterial_id",
    "geomaterial.id",
    "mineral_id",
    "id"
  ),
  names(mineral_raw)
)

# Automatically recognize common mineral-name column names.
name_candidates <- intersect(
  c(
    "Mineral",
    "mineral",
    "Mineral Name",
    "Mineral.Name",
    "mineral_name",
    "name"
  ),
  names(mineral_raw)
)

if (length(id_candidates) == 0) {
  stop(
    "Cannot identify a Mindat ID column.\n",
    "Available columns:\n",
    paste(names(mineral_raw), collapse = ", ")
  )
}

if (length(name_candidates) == 0) {
  stop(
    "Cannot identify a mineral-name column.\n",
    "Available columns:\n",
    paste(names(mineral_raw), collapse = ", ")
  )
}

id_column <- id_candidates[1]
name_column <- name_candidates[1]

cat("\nUsing ID column: ", id_column, "\n", sep = "")
cat("Using mineral-name column: ", name_column, "\n\n", sep = "")

mineral_names <- mineral_raw %>%
  transmute(
    geomaterial_id = suppressWarnings(
      as.integer(.data[[id_column]])
    ),
    mineral_name = as.character(
      .data[[name_column]]
    )
  ) %>%
  filter(
    !is.na(geomaterial_id),
    !is.na(mineral_name),
    nzchar(trimws(mineral_name))
  ) %>%
  distinct(
    geomaterial_id,
    .keep_all = TRUE
  )

cat(
  "Minerals with valid Mindat IDs: ",
  nrow(mineral_names),
  "\n\n",
  sep = ""
)

# ============================================================
# 6. Count actual localities for each mineral
# ============================================================

mineral_counts <- locality_geomaterial_links %>%
  count(
    geomaterial_id,
    name = "n_actual_localities"
  )

mineral_counts_all <- mineral_names %>%
  left_join(
    mineral_counts,
    by = "geomaterial_id"
  ) %>%
  mutate(
    n_actual_localities = tidyr::replace_na(
      n_actual_localities,
      0L
    )
  )

# Run only minerals with at least one locality.
selected_minerals <- mineral_counts_all %>%
  filter(
    n_actual_localities > 0
  ) %>%
  select(
    geomaterial_id,
    mineral_name,
    n_actual_localities
  ) %>%
  arrange(
    geomaterial_id
  )

log_message(
  "Minerals selected for leaf-locality filtering: ",
  nrow(selected_minerals)
)

log_message(
  "Total initial mineral-locality links: ",
  format(
    sum(selected_minerals$n_actual_localities),
    big.mark = ","
  )
)

safe_write_csv(
  selected_minerals,
  file.path(
    run_root_dir,
    "selected_minerals.csv"
  )
)

# ============================================================
# 7. API metadata helper functions
# ============================================================

first_non_null <- function(...) {

  values <- list(...)

  for (value in values) {
    if (!is.null(value) && length(value) > 0) {
      return(value)
    }
  }

  NULL
}

parse_binary_flag <- function(x) {

  if (is.null(x) || length(x) == 0) {
    return(NA_integer_)
  }

  if (is.logical(x)) {
    return(as.integer(isTRUE(x[1])))
  }

  x <- tolower(
    trimws(
      as.character(x[1])
    )
  )

  if (x %in% c("1", "true", "yes", "y")) {
    return(1L)
  }

  if (x %in% c("0", "false", "no", "n")) {
    return(0L)
  }

  suppressWarnings(as.integer(x))
}

# ============================================================
# 8. Read and cache locality metadata
# ============================================================

fetch_locality_metadata <- function(
    locality_id,
    force_refresh = FALSE
) {

  cache_file <- file.path(
    metadata_cache_dir,
    sprintf(
      "locality_%09d.rds",
      locality_id
    )
  )

  if (
    file.exists(cache_file) &&
    !force_refresh
  ) {

    # Existing cache files from the older workflow may contain
    # additional columns such as locsinclude_ids. They are harmless:
    # the three-rule workflow below simply does not use them.
    return(readRDS(cache_file))
  }

  endpoint <- paste0(
    api_base_url,
    "/localities/",
    locality_id,
    "/"
  )

  api_result <- NULL
  last_error <- NULL

  for (attempt in seq_len(max_retries)) {

    api_result <- tryCatch(
      {

        response <- request(endpoint) %>%
          req_url_query(
            expand = "~all",
            format = "json"
          ) %>%
          req_headers(
            Authorization = paste(
              "Token",
              token
            )
          ) %>%
          req_timeout(300) %>%
          req_perform()

        response %>%
          resp_body_string() %>%
          fromJSON(
            simplifyVector = FALSE
          )
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(api_result)) {
      break
    }

    if (attempt < max_retries) {
      Sys.sleep(
        retry_sleep_seconds * attempt
      )
    }
  }

  if (is.null(api_result)) {

    warning(
      "API request failed for locality ID = ",
      locality_id,
      "\n",
      last_error
    )

    return(
      tibble(
        locality_id = as.integer(locality_id),
        locality_name = NA_character_,
        revtxtd = NA_character_,
        non_hierarchical = NA_integer_,
        api_success = FALSE
      )
    )
  }

  metadata <- tibble(
    locality_id = as.integer(locality_id),

    locality_name = as.character(
      first_non_null(
        api_result$txt,
        api_result$name,
        NA_character_
      )
    ),

    revtxtd = as.character(
      first_non_null(
        api_result$revtxtd,
        NA_character_
      )
    ),

    non_hierarchical = parse_binary_flag(
      first_non_null(
        api_result$non_hierarchical,
        api_result$nonhierarchical
      )
    ),

    api_success = TRUE
  )

  safe_save_rds(
    metadata,
    cache_file
  )

  if (request_pause_seconds > 0) {
    Sys.sleep(request_pause_seconds)
  }

  metadata
}

# ============================================================
# 9. Normalize revtxtd paths
# ============================================================

normalize_revtxtd <- function(x) {

  x <- as.character(x)

  # Replace non-breaking spaces with normal spaces.
  x <- str_replace_all(
    x,
    fixed("\u00a0"),
    " "
  )

  # Collapse repeated whitespace.
  x <- str_squish(x)

  # Standardize spaces around commas.
  x <- str_replace_all(
    x,
    "\\s*,\\s*",
    ", "
  )

  # Remove extra commas/spaces at the beginning and end.
  x <- str_replace(
    x,
    "^[,\\s]+",
    ""
  )

  x <- str_replace(
    x,
    "[,\\s]+$",
    ""
  )

  # Compare locality paths case-insensitively.
  str_to_lower(x)
}

# A parent must be a complete hierarchical-path prefix of a child.
# Partial locality-name string matches are not accepted.
is_strict_path_prefix <- function(
    parent_path,
    child_path
) {

  if (
    is.na(parent_path) ||
    is.na(child_path) ||
    !nzchar(parent_path) ||
    !nzchar(child_path)
  ) {
    return(FALSE)
  }

  # Identical paths do not define a parent-child relationship.
  if (identical(parent_path, child_path)) {
    return(FALSE)
  }

  startsWith(
    child_path,
    paste0(parent_path, ", ")
  )
}

# ============================================================
# 10. Apply the three leaf-locality rules to one mineral
# ============================================================

classify_one_mineral <- function(
    geomaterial_id,
    mineral_name
) {

  # These are the actual locality IDs at which the current
  # mineral is recorded in the downloaded Mindat link table.
  actual_locality_ids <- locality_geomaterial_links %>%
    filter(
      .data$geomaterial_id == !!geomaterial_id
    ) %>%
    pull(locality_id) %>%
    unique() %>%
    sort()

  cat("\n-------------------------------------------------\n")
  cat(
    mineral_name,
    " | geomaterial ID = ",
    geomaterial_id,
    " | actual localities = ",
    length(actual_locality_ids),
    "\n",
    sep = ""
  )

  metadata_list <- vector(
    "list",
    length(actual_locality_ids)
  )

  for (i in seq_along(actual_locality_ids)) {

    locality_id <- actual_locality_ids[i]

    if (isTRUE(verbose_locality_progress)) {
      cat(
        "\rReading metadata ",
        i,
        "/",
        length(actual_locality_ids),
        " | locality ",
        locality_id,
        "          ",
        sep = ""
      )
      flush.console()
    }

    metadata_list[[i]] <- fetch_locality_metadata(
      locality_id = locality_id,
      force_refresh = force_refresh_metadata
    )
  }

  if (isTRUE(verbose_locality_progress)) {
    cat("\n")
  }

  metadata <- bind_rows(
    metadata_list
  ) %>%
    mutate(
      geomaterial_id = as.integer(geomaterial_id),
      mineral_name = as.character(mineral_name),

      revtxtd_normalized = normalize_revtxtd(
        revtxtd
      ),

      # Rule 1:
      # Exclude localities explicitly flagged as non-hierarchical.
      non_hierarchical_removed =
        !is.na(non_hierarchical) &
        non_hierarchical == 1L
    )

  # ----------------------------------------------------------
  # Rule 2: remove hierarchical parent localities
  #
  # Comparison is performed only among hierarchical localities
  # where the SAME mineral is actually recorded.
  #
  # A locality with non_hierarchical == 1 is neither retained
  # as a final leaf nor allowed to act as a more-specific child
  # in the revtxtd path comparison.
  # ----------------------------------------------------------

  hierarchical_metadata <- metadata %>%
    filter(
      !non_hierarchical_removed,
      !is.na(revtxtd_normalized),
      nzchar(revtxtd_normalized)
    )

  prefix_child_map <- list()

  if (nrow(hierarchical_metadata) >= 2) {

    for (i in seq_len(nrow(hierarchical_metadata))) {

      parent_id <- hierarchical_metadata$locality_id[i]

      parent_path <-
        hierarchical_metadata$revtxtd_normalized[i]

      child_rows <- hierarchical_metadata %>%
        filter(
          locality_id != parent_id
        ) %>%
        filter(
          map_lgl(
            revtxtd_normalized,
            function(child_path) {
              is_strict_path_prefix(
                parent_path,
                child_path
              )
            }
          )
        )

      if (nrow(child_rows) > 0) {
        prefix_child_map[[
          as.character(parent_id)
        ]] <- child_rows$locality_id
      }
    }
  }

  metadata <- metadata %>%
    mutate(
      prefix_child_ids = map(
        locality_id,
        function(id) {

          result <- prefix_child_map[[
            as.character(id)
          ]]

          if (is.null(result)) {
            integer(0)
          } else {
            sort(
              unique(
                as.integer(result)
              )
            )
          }
        }
      ),

      prefix_parent_removed =
        lengths(prefix_child_ids) > 0
    )

  # ----------------------------------------------------------
  # Rule 3: remove records with insufficient locality-path data
  # ----------------------------------------------------------

  metadata <- metadata %>%
    mutate(
      missing_locality_path =
        is.na(locality_name) |
        !nzchar(str_squish(locality_name)) |
        is.na(revtxtd) |
        !nzchar(str_squish(revtxtd)),

      # Final classification uses ONLY the three rules above.
      final_keep =
        !missing_locality_path &
        !non_hierarchical_removed &
        !prefix_parent_removed
    )

  # Convert the list column to text for CSV output and generate
  # a human-readable removal reason for each locality.
  metadata <- metadata %>%
    mutate(
      prefix_child_ids_text = map_chr(
        prefix_child_ids,
        ~ paste(.x, collapse = ";")
      ),

      removal_reason = pmap_chr(
        list(
          final_keep,
          missing_locality_path,
          non_hierarchical_removed,
          prefix_parent_removed,
          prefix_child_ids_text
        ),
        function(
            keep,
            missing_path,
            nonhier,
            prefix_parent,
            prefix_children
        ) {

          if (isTRUE(keep)) {
            return("KEEP")
          }

          reasons <- character(0)

          if (isTRUE(missing_path)) {
            reasons <- c(
              reasons,
              "missing locality_name or revtxtd"
            )
          }

          if (isTRUE(nonhier)) {
            reasons <- c(
              reasons,
              "non_hierarchical = 1"
            )
          }

          if (isTRUE(prefix_parent)) {
            reasons <- c(
              reasons,
              paste0(
                "revtxtd parent of locality ID(s): ",
                prefix_children
              )
            )
          }

          paste(
            reasons,
            collapse = " | "
          )
        }
      )
    ) %>%
    select(
      geomaterial_id,
      mineral_name,
      locality_id,
      locality_name,
      revtxtd,
      revtxtd_normalized,
      non_hierarchical,
      api_success,
      non_hierarchical_removed,
      prefix_parent_removed,
      prefix_child_ids_text,
      missing_locality_path,
      final_keep,
      removal_reason
    ) %>%
    arrange(
      desc(final_keep),
      locality_id
    )

  metadata
}

# ============================================================
# 11. Run all selected minerals with checkpoints
# ============================================================

run_started_at <- Sys.time()

initialize_status_table <- function(selected_minerals) {

  selected_minerals %>%
    transmute(
      geomaterial_id,
      mineral_name,
      n_actual_localities,
      status = "pending",
      checkpoint_file = map_chr(
        geomaterial_id,
        checkpoint_path_for
      ),
      started_at = NA_character_,
      finished_at = NA_character_,
      elapsed_seconds = NA_real_,
      n_final_leaf = NA_integer_,
      n_api_failed = NA_integer_,
      error_message = NA_character_
    )
}

# Reuse the status file only within this new three-rule run directory.
if (
  resume_from_checkpoints &&
  file.exists(status_file)
) {

  old_status <- read_csv(
    status_file,
    show_col_types = FALSE
  )

  run_status <- initialize_status_table(
    selected_minerals
  )

  common_ids <- intersect(
    run_status$geomaterial_id,
    old_status$geomaterial_id
  )

  for (id in common_ids) {

    new_index <- match(
      id,
      run_status$geomaterial_id
    )

    old_index <- match(
      id,
      old_status$geomaterial_id
    )

    copy_columns <- intersect(
      c(
        "status",
        "started_at",
        "finished_at",
        "elapsed_seconds",
        "n_final_leaf",
        "n_api_failed",
        "error_message"
      ),
      names(old_status)
    )

    for (column in copy_columns) {
      run_status[[column]][new_index] <-
        old_status[[column]][old_index]
    }
  }

} else {

  run_status <- initialize_status_table(
    selected_minerals
  )
}

# A checkpoint file is the primary evidence that a mineral was
# successfully completed. Correct the status table accordingly.
for (i in seq_len(nrow(run_status))) {

  checkpoint_file <- run_status$checkpoint_file[i]

  if (
    resume_from_checkpoints &&
    file.exists(checkpoint_file)
  ) {

    checkpoint_ok <- tryCatch(
      {
        x <- readRDS(checkpoint_file)

        all(
          c(
            "geomaterial_id",
            "locality_id",
            "final_keep"
          ) %in% names(x)
        )
      },
      error = function(e) {
        FALSE
      }
    )

    if (isTRUE(checkpoint_ok)) {
      run_status$status[i] <- "completed"
    } else if (isTRUE(recompute_corrupt_checkpoint)) {
      file.remove(checkpoint_file)
      run_status$status[i] <- "pending"
    } else {
      stop(
        "Corrupt checkpoint detected: ",
        checkpoint_file
      )
    }
  }
}

safe_write_csv(
  run_status,
  status_file
)

n_total <- nrow(selected_minerals)
n_resumed <- 0L
n_newly_computed <- 0L
n_failed <- 0L

progress_bar <- txtProgressBar(
  min = 0,
  max = max(1L, n_total),
  style = 3
)

if (n_total > 0) {

  for (i in seq_len(n_total)) {

    geomaterial_id <-
      selected_minerals$geomaterial_id[i]

    mineral_name <-
      selected_minerals$mineral_name[i]

    status_index <- match(
      geomaterial_id,
      run_status$geomaterial_id
    )

    checkpoint_file <- checkpoint_path_for(
      geomaterial_id
    )

    # --------------------------------------------------------
    # Resume from an existing valid checkpoint.
    # --------------------------------------------------------

    if (
      resume_from_checkpoints &&
      file.exists(checkpoint_file)
    ) {

      checkpoint_result <- tryCatch(
        readRDS(checkpoint_file),
        error = function(e) NULL
      )

      checkpoint_ok <-
        !is.null(checkpoint_result) &&
        all(
          c(
            "geomaterial_id",
            "locality_id",
            "final_keep"
          ) %in% names(checkpoint_result)
        )

      if (isTRUE(checkpoint_ok)) {

        n_resumed <- n_resumed + 1L

        run_status$status[status_index] <- "completed"
        run_status$n_final_leaf[status_index] <- sum(
          checkpoint_result$final_keep,
          na.rm = TRUE
        )

        if ("api_success" %in% names(checkpoint_result)) {
          run_status$n_api_failed[status_index] <- sum(
            !checkpoint_result$api_success,
            na.rm = TRUE
          )
        }

        setTxtProgressBar(
          progress_bar,
          i
        )

        rm(checkpoint_result)
        next
      }

      log_error(
        "Corrupt checkpoint detected for geomaterial ID = ",
        geomaterial_id,
        " | file = ",
        checkpoint_file
      )

      if (!isTRUE(recompute_corrupt_checkpoint)) {
        close(progress_bar)
        stop("Run stopped because a corrupt checkpoint was detected.")
      }

      file.remove(checkpoint_file)
    }

    # --------------------------------------------------------
    # New computation.
    # --------------------------------------------------------

    mineral_started_at <- Sys.time()

    run_status$status[status_index] <- "running"
    run_status$started_at[status_index] <- format(
      mineral_started_at,
      "%Y-%m-%d %H:%M:%S"
    )
    run_status$error_message[status_index] <- NA_character_

    safe_write_csv(
      run_status,
      status_file
    )

    log_message(
      "Starting [",
      i,
      "/",
      n_total,
      "] ",
      mineral_name,
      " | geomaterial ID = ",
      geomaterial_id,
      " | localities = ",
      selected_minerals$n_actual_localities[i]
    )

    mineral_result <- tryCatch(
      classify_one_mineral(
        geomaterial_id = geomaterial_id,
        mineral_name = mineral_name
      ),
      error = function(e) {
        structure(
          conditionMessage(e),
          class = c(
            "leaf_mineral_error",
            "character"
          )
        )
      }
    )

    elapsed_seconds <- as.numeric(
      difftime(
        Sys.time(),
        mineral_started_at,
        units = "secs"
      )
    )

    if (inherits(
      mineral_result,
      "leaf_mineral_error"
    )) {

      n_failed <- n_failed + 1L

      run_status$status[status_index] <- "failed"
      run_status$finished_at[status_index] <- format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      )
      run_status$elapsed_seconds[status_index] <-
        elapsed_seconds
      run_status$error_message[status_index] <-
        as.character(mineral_result)

      log_error(
        "Mineral failed: ",
        mineral_name,
        " | geomaterial ID = ",
        geomaterial_id,
        " | elapsed = ",
        format_elapsed(elapsed_seconds),
        " | error = ",
        as.character(mineral_result)
      )

    } else {

      # Save the checkpoint before marking the mineral completed.
      safe_save_rds(
        mineral_result,
        checkpoint_file
      )

      n_final_leaf <- sum(
        mineral_result$final_keep,
        na.rm = TRUE
      )

      n_api_failed <- sum(
        !mineral_result$api_success,
        na.rm = TRUE
      )

      run_status$status[status_index] <- "completed"
      run_status$finished_at[status_index] <- format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      )
      run_status$elapsed_seconds[status_index] <-
        elapsed_seconds
      run_status$n_final_leaf[status_index] <-
        n_final_leaf
      run_status$n_api_failed[status_index] <-
        n_api_failed
      run_status$error_message[status_index] <-
        NA_character_

      n_newly_computed <- n_newly_computed + 1L

      log_message(
        "Completed: ",
        mineral_name,
        " | geomaterial ID = ",
        geomaterial_id,
        " | initial = ",
        nrow(mineral_result),
        " | final leaf = ",
        n_final_leaf,
        " | API failed rows = ",
        n_api_failed,
        " | elapsed = ",
        format_elapsed(elapsed_seconds)
      )
    }

    safe_write_csv(
      run_status,
      status_file
    )

    setTxtProgressBar(
      progress_bar,
      i
    )

    if (
      n_newly_computed > 0 &&
      n_newly_computed %% progress_log_every == 0
    ) {

      total_elapsed <- as.numeric(
        difftime(
          Sys.time(),
          run_started_at,
          units = "secs"
        )
      )

      log_message(
        "Progress: completed = ",
        sum(run_status$status == "completed"),
        "/",
        n_total,
        " | failed = ",
        sum(run_status$status == "failed"),
        " | resumed = ",
        n_resumed,
        " | newly computed = ",
        n_newly_computed,
        " | elapsed = ",
        format_elapsed(total_elapsed)
      )
    }

    rm(mineral_result)
    invisible(gc(verbose = FALSE))
  }
}

close(progress_bar)

# ============================================================
# 12. Merge all successful checkpoints
# ============================================================

completed_status <- run_status %>%
  filter(
    status == "completed",
    file.exists(checkpoint_file)
  ) %>%
  arrange(
    geomaterial_id
  )

log_message(
  "Merging completed checkpoints: ",
  nrow(completed_status)
)

detail_result_list <- vector(
  "list",
  nrow(completed_status)
)

merge_bar <- txtProgressBar(
  min = 0,
  max = max(1L, nrow(completed_status)),
  style = 3
)

if (nrow(completed_status) > 0) {

  for (i in seq_len(nrow(completed_status))) {

    detail_result_list[[i]] <- readRDS(
      completed_status$checkpoint_file[i]
    )

    setTxtProgressBar(
      merge_bar,
      i
    )
  }
}

close(merge_bar)

detail_results <- bind_rows(
  detail_result_list
)

# ============================================================
# 13. Final mineral-locality long tables
# ============================================================

final_mineral_locality_links <- detail_results %>%
  filter(
    final_keep
  ) %>%
  select(
    geomaterial_id,
    mineral_name,
    locality_id,
    locality_name,
    revtxtd
  ) %>%
  distinct(
    geomaterial_id,
    locality_id,
    .keep_all = TRUE
  ) %>%
  arrange(
    geomaterial_id,
    locality_id
  )

# This compact three-column table is convenient for later
# co-occurrence calculations.
final_mineral_locality_ids_long <-
  final_mineral_locality_links %>%
  select(
    geomaterial_id,
    mineral_name,
    locality_id
  )

# The same links sorted by locality first.
final_locality_mineral_links <-
  final_mineral_locality_ids_long %>%
  arrange(
    locality_id,
    geomaterial_id
  )

# ============================================================
# 14. Per-mineral summary
# ============================================================

summary_results <- detail_results %>%
  group_by(
    geomaterial_id,
    mineral_name
  ) %>%
  summarise(
    n_actual_localities = n(),

    n_non_hierarchical_removed = sum(
      non_hierarchical_removed,
      na.rm = TRUE
    ),

    n_prefix_parent_removed = sum(
      prefix_parent_removed,
      na.rm = TRUE
    ),

    n_missing_path_removed = sum(
      missing_locality_path,
      na.rm = TRUE
    ),

    n_final_leaf = sum(
      final_keep,
      na.rm = TRUE
    ),

    final_locality_ids = paste(
      sort(
        unique(
          locality_id[final_keep %in% TRUE]
        )
      ),
      collapse = ";"
    ),

    n_api_failed = sum(
      !api_success,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  arrange(
    geomaterial_id
  )

# ============================================================
# 15. Save final outputs
# ============================================================

log_message("Saving final outputs...")

safe_write_csv(
  detail_results,
  file.path(
    output_dir,
    "leaf_check_detail_all_minerals.csv"
  )
)

safe_save_rds(
  detail_results,
  file.path(
    output_dir,
    "leaf_check_detail_all_minerals.rds"
  )
)

safe_write_csv(
  summary_results,
  file.path(
    output_dir,
    "leaf_check_summary_all_minerals.csv"
  )
)

safe_save_rds(
  summary_results,
  file.path(
    output_dir,
    "leaf_check_summary_all_minerals.rds"
  )
)

safe_write_csv(
  final_mineral_locality_links,
  file.path(
    output_dir,
    "final_mineral_locality_links_with_names.csv"
  )
)

safe_save_rds(
  final_mineral_locality_links,
  file.path(
    output_dir,
    "final_mineral_locality_links_with_names.rds"
  )
)

safe_write_csv(
  final_mineral_locality_ids_long,
  file.path(
    output_dir,
    "final_mineral_locality_ids_long.csv"
  )
)

safe_save_rds(
  final_mineral_locality_ids_long,
  file.path(
    output_dir,
    "final_mineral_locality_ids_long.rds"
  )
)

safe_write_csv(
  final_locality_mineral_links,
  file.path(
    output_dir,
    "final_locality_mineral_links_for_cooccurrence.csv"
  )
)

safe_save_rds(
  final_locality_mineral_links,
  file.path(
    output_dir,
    "final_locality_mineral_links_for_cooccurrence.rds"
  )
)

failed_minerals <- run_status %>%
  filter(
    status == "failed"
  ) %>%
  arrange(
    geomaterial_id
  )

safe_write_csv(
  failed_minerals,
  file.path(
    output_dir,
    "failed_minerals.csv"
  )
)

# ============================================================
# 16. Consistency checks
# ============================================================

summary_final_count <- sum(
  summary_results$n_final_leaf,
  na.rm = TRUE
)

long_table_count <- nrow(
  final_mineral_locality_ids_long
)

if (summary_final_count != long_table_count) {

  log_error(
    "Consistency check failed: summary n_final_leaf total = ",
    summary_final_count,
    "; final long-table rows = ",
    long_table_count
  )

} else {

  log_message(
    "Consistency check passed: final links = ",
    long_table_count
  )
}

duplicate_final_links <-
  final_mineral_locality_ids_long %>%
  count(
    geomaterial_id,
    locality_id
  ) %>%
  filter(
    n > 1
  )

if (nrow(duplicate_final_links) > 0) {

  safe_write_csv(
    duplicate_final_links,
    file.path(
      output_dir,
      "WARNING_duplicate_final_links.csv"
    )
  )

  log_error(
    "Duplicate geomaterial_id-locality_id links detected: ",
    nrow(duplicate_final_links)
  )

} else {

  log_message(
    "Duplicate check passed: no duplicate final links."
  )
}

# ============================================================
# 17. Final run report
# ============================================================

run_finished_at <- Sys.time()

total_elapsed_seconds <- as.numeric(
  difftime(
    run_finished_at,
    run_started_at,
    units = "secs"
  )
)

final_report <- tibble(
  run_started_at = format(
    run_started_at,
    "%Y-%m-%d %H:%M:%S"
  ),
  run_finished_at = format(
    run_finished_at,
    "%Y-%m-%d %H:%M:%S"
  ),
  total_elapsed_seconds =
    total_elapsed_seconds,
  total_elapsed =
    format_elapsed(total_elapsed_seconds),
  n_selected_minerals =
    nrow(selected_minerals),
  n_completed_minerals =
    sum(run_status$status == "completed"),
  n_failed_minerals =
    sum(run_status$status == "failed"),
  n_resumed_from_checkpoint =
    n_resumed,
  n_newly_computed =
    n_newly_computed,
  n_detail_rows =
    nrow(detail_results),
  n_final_mineral_locality_links =
    nrow(final_mineral_locality_ids_long),
  n_unique_final_localities =
    n_distinct(
      final_mineral_locality_ids_long$locality_id
    )
)

safe_write_csv(
  final_report,
  file.path(
    output_dir,
    "run_report.csv"
  )
)

log_message("=================================================")
log_message("Run finished")
log_message(
  "Minerals: ",
  nrow(selected_minerals)
)
log_message(
  "Completed: ",
  sum(run_status$status == "completed")
)
log_message(
  "Failed: ",
  sum(run_status$status == "failed")
)
log_message(
  "Newly computed: ",
  n_newly_computed
)
log_message(
  "Resumed from checkpoints: ",
  n_resumed
)
log_message(
  "Final mineral-locality links: ",
  format(
    nrow(final_mineral_locality_ids_long),
    big.mark = ","
  )
)
log_message(
  "Unique final localities: ",
  format(
    n_distinct(
      final_mineral_locality_ids_long$locality_id
    ),
    big.mark = ","
  )
)
log_message(
  "Total elapsed: ",
  format_elapsed(total_elapsed_seconds)
)
log_message(
  "Output directory: ",
  normalizePath(output_dir)
)
log_message("=================================================")

print(
  final_report,
  width = Inf
)

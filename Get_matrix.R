# ============================================================
# Build mineral co-occurrence matrices from leaf localities
#
# Input:
#   Leaf-locality summary table
#
# Required columns:
#   geomaterial_id
#   mineral_name
#   n_final_leaf
#   final_locality_ids
#
# Outputs:
#   1. Raw mineral co-occurrence matrix
#   2. Scaled-to-100 mineral co-occurrence matrix
#
# Raw co-occurrence:
#   Number of final leaf localities shared by two minerals.
#
# Scaled-to-100:
#   co-occurrence(i,j) / min(N_i, N_j) * 100
#
# where N_i and N_j are the numbers of final leaf localities
# for minerals i and j.
# ============================================================


suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
})


# ============================================================
# 1. User settings
# ============================================================

# Leaf-locality summary file
leaf_file <- "leaf_check_summary_all_minerals.csv"

# Minimum and maximum numbers of final leaf localities
# used to select minerals
min_locality <- 1
max_locality <- 521

# Output directory
out_dir <- "mineral_cooccurrence_matrices"

# Round scaled-to-100 values to integers
round_scaled100 <- TRUE

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Locality-range label used in output filenames
locality_range <- paste0(
  min_locality,
  "_to_",
  max_locality
)


# ============================================================
# 2. Read leaf-locality summary
# ============================================================

leaf_dt <- fread(
  leaf_file,
  encoding = "UTF-8",
  na.strings = c("", "NA")
)

required_columns <- c(
  "geomaterial_id",
  "mineral_name",
  "n_final_leaf",
  "final_locality_ids"
)

missing_columns <- setdiff(
  required_columns,
  names(leaf_dt)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required column(s): ",
    paste(missing_columns, collapse = ", ")
  )
}

cat("Leaf-locality summary loaded.\n")
cat("Total minerals:", nrow(leaf_dt), "\n")


# ============================================================
# 3. Clean and select minerals
# ============================================================

leaf_dt[
  ,
  geomaterial_id := as.integer(geomaterial_id)
]

leaf_dt[
  ,
  n_final_leaf := as.integer(n_final_leaf)
]

leaf_dt[
  ,
  mineral_name := trimws(as.character(mineral_name))
]

# Remove invalid records
leaf_dt <- leaf_dt[
  !is.na(geomaterial_id) &
    !is.na(mineral_name) &
    mineral_name != "" &
    !is.na(n_final_leaf)
]

# Keep one record per geomaterial ID
leaf_dt <- unique(
  leaf_dt,
  by = "geomaterial_id"
)

# Select minerals according to the number of final leaf localities
selected <- leaf_dt[
  n_final_leaf >= min_locality &
    n_final_leaf <= max_locality
]

if (nrow(selected) == 0) {
  stop(
    "No minerals remain within the selected locality range."
  )
}

# Make matrix names unique in case duplicate mineral names exist
selected[
  ,
  matrix_name := make.unique(
    mineral_name,
    sep = "__"
  )
]

cat(
  "Selected locality range:",
  min_locality,
  "to",
  max_locality,
  "\n"
)

cat(
  "Selected minerals:",
  nrow(selected),
  "\n"
)


# ============================================================
# 4. Expand final locality IDs
# ============================================================

# Convert a semicolon-separated locality-ID string
# into a unique integer vector
split_locality_ids <- function(x) {
  
  if (
    is.na(x) ||
    trimws(x) == ""
  ) {
    return(integer(0))
  }
  
  ids <- strsplit(
    x,
    split = ";",
    fixed = TRUE
  )[[1]]
  
  ids <- suppressWarnings(
    as.integer(
      trimws(ids)
    )
  )
  
  sort(
    unique(
      ids[!is.na(ids)]
    )
  )
}

selected[
  ,
  locality_id_list := lapply(
    final_locality_ids,
    split_locality_ids
  )
]

# Recount locality IDs for an internal consistency check
selected[
  ,
  n_ids_from_list := lengths(locality_id_list)
]

count_mismatch <- selected[
  n_ids_from_list != n_final_leaf
]

cat(
  "Minerals with locality-count mismatch:",
  nrow(count_mismatch),
  "\n"
)

if (nrow(count_mismatch) > 0) {
  warning(
    "Some n_final_leaf values do not match the number ",
    "of IDs in final_locality_ids."
  )
}


# ============================================================
# 5. Create mineral-locality records
# ============================================================

records_list <- lapply(
  seq_len(nrow(selected)),
  function(i) {
    
    ids <- selected$locality_id_list[[i]]
    
    if (length(ids) == 0) {
      return(NULL)
    }
    
    data.table(
      geomaterial_id =
        selected$geomaterial_id[i],
      
      mineral_name =
        selected$mineral_name[i],
      
      matrix_name =
        selected$matrix_name[i],
      
      locality_id = ids
    )
  }
)

leaf_records <- rbindlist(
  records_list,
  use.names = TRUE,
  fill = TRUE
)

if (nrow(leaf_records) == 0) {
  stop(
    "No valid mineral-locality records were generated."
  )
}

# Count each mineral-locality pair only once
leaf_records <- unique(
  leaf_records,
  by = c(
    "geomaterial_id",
    "locality_id"
  )
)

setorder(
  leaf_records,
  locality_id,
  geomaterial_id
)

cat(
  "Mineral-locality records:",
  nrow(leaf_records),
  "\n"
)

cat(
  "Unique leaf localities:",
  uniqueN(leaf_records$locality_id),
  "\n"
)


# ============================================================
# 6. Build mineral-locality incidence matrix
# ============================================================

mineral_ids <- selected$geomaterial_id
matrix_names <- selected$matrix_name

locality_ids <- sort(
  unique(
    leaf_records$locality_id
  )
)

mineral_index <- match(
  leaf_records$geomaterial_id,
  mineral_ids
)

locality_index <- match(
  leaf_records$locality_id,
  locality_ids
)

if (
  anyNA(mineral_index) ||
  anyNA(locality_index)
) {
  stop(
    "Unmatched IDs were found while building ",
    "the incidence matrix."
  )
}

incidence_mat <- sparseMatrix(
  i = mineral_index,
  j = locality_index,
  x = 1L,
  dims = c(
    nrow(selected),
    length(locality_ids)
  ),
  dimnames = list(
    matrix_names,
    as.character(locality_ids)
  )
)

cat(
  "Incidence matrix:",
  nrow(incidence_mat),
  "minerals x",
  ncol(incidence_mat),
  "localities\n"
)


# ============================================================
# 7. Calculate raw co-occurrence matrix
# ============================================================

# Matrix multiplication gives the number of final leaf
# localities shared by every pair of minerals.
#
# The diagonal gives the number of final leaf localities
# for each mineral.

co_mat <- as.matrix(
  tcrossprod(incidence_mat)
)

storage.mode(co_mat) <- "integer"

diag_vals <- diag(co_mat)

cat(
  "Raw co-occurrence matrix completed.\n"
)


# ============================================================
# 8. Calculate scaled-to-100 co-occurrence matrix
# ============================================================

# For minerals i and j:
#
#                    co-occurrence(i,j)
# scaled(i,j) = ----------------------------- x 100
#                    min(N_i, N_j)
#
# A value of 100 means that all localities of the
# less-common mineral are shared with the other mineral.

denominator_mat <- outer(
  diag_vals,
  diag_vals,
  FUN = pmin
)

scaled_mat <- matrix(
  NA_real_,
  nrow = nrow(co_mat),
  ncol = ncol(co_mat),
  dimnames = dimnames(co_mat)
)

valid_positions <- denominator_mat > 0

scaled_mat[valid_positions] <-
  co_mat[valid_positions] /
  denominator_mat[valid_positions] *
  100

if (round_scaled100) {
  scaled_mat <- round(
    scaled_mat
  )
}

# Each mineral has complete overlap with itself
diag(scaled_mat) <- 100

cat(
  "Scaled-to-100 matrix completed.\n"
)


# ============================================================
# 9. Create upper-triangle matrices
# ============================================================

raw_upper <- co_mat

raw_upper[
  lower.tri(raw_upper)
] <- NA_integer_

scaled_upper <- scaled_mat

scaled_upper[
  lower.tri(scaled_upper)
] <- NA_real_


# ============================================================
# 10. Check matrix diagonal
# ============================================================

diag_check <- data.table(
  geomaterial_id =
    selected$geomaterial_id,
  
  mineral_name =
    selected$mineral_name,
  
  n_final_leaf =
    selected$n_final_leaf,
  
  matrix_diagonal =
    as.integer(diag_vals)
)

diag_check[
  ,
  difference :=
    matrix_diagonal - n_final_leaf
]

n_diag_mismatch <- diag_check[
  difference != 0,
  .N
]

cat(
  "Raw-matrix diagonal mismatches:",
  n_diag_mismatch,
  "\n"
)

if (n_diag_mismatch > 0) {
  warning(
    "Some raw-matrix diagonal values do not match ",
    "n_final_leaf."
  )
}


# ============================================================
# 11. Save outputs
# ============================================================

cat(
  "Writing output files...\n"
)

# Selected mineral list
selected_output <- selected[
  ,
  .(
    geomaterial_id,
    mineral_name,
    n_final_leaf,
    final_locality_ids
  )
]

fwrite(
  selected_output,
  file.path(
    out_dir,
    paste0(
      "selected_minerals_",
      locality_range,
      ".csv"
    )
  )
)

# Expanded mineral-locality records
fwrite(
  leaf_records[
    ,
    .(
      geomaterial_id,
      mineral_name,
      locality_id
    )
  ],
  file.path(
    out_dir,
    paste0(
      "mineral_locality_records_",
      locality_range,
      ".csv"
    )
  )
)

# Raw upper-triangle co-occurrence matrix
write.csv(
  raw_upper,
  file.path(
    out_dir,
    paste0(
      "cooccurrence_raw_",
      locality_range,
      ".csv"
    )
  ),
  row.names = TRUE,
  na = ""
)

# Scaled-to-100 upper-triangle co-occurrence matrix
write.csv(
  scaled_upper,
  file.path(
    out_dir,
    paste0(
      "cooccurrence_scaled100_",
      locality_range,
      ".csv"
    )
  ),
  row.names = TRUE,
  na = ""
)


# Diagonal consistency check
fwrite(
  diag_check,
  file.path(
    out_dir,
    paste0(
      "diagonal_check_",
      locality_range,
      ".csv"
    )
  )
)


# ============================================================
# 12. Final summary
# ============================================================

cat("\n")
cat("========================================\n")
cat("DONE\n")
cat("========================================\n")

cat(
  "Locality range:",
  min_locality,
  "to",
  max_locality,
  "\n"
)

cat(
  "Selected minerals:",
  nrow(selected),
  "\n"
)

cat(
  "Unique leaf localities:",
  length(locality_ids),
  "\n"
)

cat(
  "Mineral-locality records:",
  nrow(leaf_records),
  "\n"
)

cat(
  "Matrix size:",
  nrow(co_mat),
  "x",
  ncol(co_mat),
  "\n"
)

cat(
  "Raw-matrix diagonal mismatches:",
  n_diag_mismatch,
  "\n"
)

cat(
  "Output directory:",
  normalizePath(out_dir),
  "\n"
)
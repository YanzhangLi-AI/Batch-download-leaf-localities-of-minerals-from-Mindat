# ============================================================
# Map IMA Mineral Names to Mindat Geomaterial IDs
#
# This script assigns Mindat geomaterial IDs to any list of
# IMA mineral names using a curated IMA-Mindat mapping table.
#
# The mapping table must contain:
#   - mineral_IMA
#   - geomaterial_id
#
# The input mineral list can contain any additional columns.
# Only the mineral-name column needs to be specified below.
# ============================================================

library(dplyr)
library(readr)
library(stringr)


# ------------------------------------------------------------
# 1. User settings
# ------------------------------------------------------------

# Input file containing a list of IMA minerals, you can revise it
input_file <- "mineral_list.csv"

# Name of the mineral-name column in the input file, you can revise it
mineral_column <- "Mineral"

# Curated IMA-Mindat mapping table
mapping_file <- "geomaterial_id_assignment.csv"

# Output files
output_file <- "mineral_with_mindat_ID.csv"
unmatched_file <- "unmatched_minerals.csv"


# ------------------------------------------------------------
# 2. Read data
# ------------------------------------------------------------

minerals <- read_csv(
  input_file,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

mapping <- read_csv(
  mapping_file,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 3. Check required columns
# ------------------------------------------------------------

if (!mineral_column %in% names(minerals)) {
  stop(
    paste0(
      'Column "', mineral_column,
      '" was not found in the input mineral list.'
    )
  )
}

if (!all(c("mineral_IMA", "geomaterial_id") %in% names(mapping))) {
  stop(
    'The mapping table must contain "mineral_IMA" ',
    'and "geomaterial_id".'
  )
}


# ------------------------------------------------------------
# 4. Normalize mineral names
#
# Normalization minimizes mismatches caused by:
#   - capitalization
#   - leading/trailing spaces
#   - repeated spaces
#   - non-breaking spaces
#   - different Unicode hyphen/dash characters
#
# Original mineral names are not modified.
# ------------------------------------------------------------

normalize_name <- function(x) {
  
  x %>%
    as.character() %>%
    str_replace_all("\u00A0", " ") %>%
    str_replace_all("[‐-‒–—−]", "-") %>%
    str_squish() %>%
    str_to_lower()
}


# ------------------------------------------------------------
# 5. Create normalized matching keys
# ------------------------------------------------------------

minerals <- minerals %>%
  mutate(
    .name_key = normalize_name(.data[[mineral_column]])
  )

mapping <- mapping %>%
  mutate(
    .name_key = normalize_name(mineral_IMA)
  )


# ------------------------------------------------------------
# 6. Check whether one IMA mineral name maps to multiple
#    Mindat geomaterial IDs
# ------------------------------------------------------------

ambiguous <- mapping %>%
  filter(
    !is.na(.name_key),
    !is.na(geomaterial_id)
  ) %>%
  distinct(.name_key, geomaterial_id) %>%
  count(.name_key, name = "n_geomaterial_ids") %>%
  filter(n_geomaterial_ids > 1)

if (nrow(ambiguous) > 0) {
  
  warning(
    nrow(ambiguous),
    " mineral name(s) map to multiple Mindat geomaterial IDs."
  )
  
  print(ambiguous)
}


# ------------------------------------------------------------
# 7. Create the lookup table
# ------------------------------------------------------------

lookup <- mapping %>%
  filter(
    !is.na(.name_key),
    !is.na(geomaterial_id)
  ) %>%
  select(
    .name_key,
    geomaterial_id
  ) %>%
  distinct()


# ------------------------------------------------------------
# 8. Assign Mindat geomaterial IDs
#
# All original rows and columns are retained.
# ------------------------------------------------------------

result <- minerals %>%
  left_join(
    lookup,
    by = ".name_key"
  )


# ------------------------------------------------------------
# 9. Identify unmatched mineral names
# ------------------------------------------------------------

unmatched <- result %>%
  filter(is.na(geomaterial_id)) %>%
  distinct(
    mineral_name = .data[[mineral_column]]
  ) %>%
  arrange(mineral_name)


# ------------------------------------------------------------
# 10. Report matching statistics
# ------------------------------------------------------------

n_total <- n_distinct(minerals$.name_key, na.rm = TRUE)

n_unmatched <- nrow(unmatched)

n_matched <- n_total - n_unmatched

cat("\n")
cat("========================================\n")
cat("IMA-Mindat matching summary\n")
cat("========================================\n")

cat("Unique input minerals :", n_total, "\n")
cat("Matched minerals      :", n_matched, "\n")
cat("Unmatched minerals    :", n_unmatched, "\n")

if (n_total > 0) {
  cat(
    "Matching rate        :",
    round(n_matched / n_total * 100, 2),
    "%\n"
  )
}


# ------------------------------------------------------------
# 11. Remove the temporary matching key
# ------------------------------------------------------------

result <- result %>%
  select(-.name_key)


# ------------------------------------------------------------
# 12. Save results
# ------------------------------------------------------------

write_csv(
  result,
  output_file
)

write_csv(
  unmatched,
  unmatched_file
)


# ------------------------------------------------------------
# 13. Print unmatched minerals
# ------------------------------------------------------------

if (nrow(unmatched) == 0) {
  
  cat("\nAll mineral names were successfully matched.\n")
  
} else {
  
  cat("\nUnmatched mineral names:\n")
  print(unmatched, n = Inf)
}
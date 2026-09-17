# Batch-download-leaf-localities-of-minerals-from-Mindat
These codes and files, which came from my own exploration in the last five months, help you to batch-download leaf-localities of minerals from Mindat. That will further assist you to do mineral association analysis based on real co-occurrence information.

# Mineral Locality and Co-occurrence Workflow

This repository provides an R-based workflow for retrieving mineral-locality
data from Mindat, identifying leaf localities, and constructing mineral
co-occurrence matrices.

The workflow consists of four main procedures.

## 1. Build a local locality dataset

**R script:** `Locality_page.R`

This script retrieves locality records from Mindat and builds a local
locality–geomaterial relationship dataset.

This procedure requires a valid **Mindat API token**. Because downloading the
complete locality dataset may take considerable time, a pre-generated file,
`locality_geomaterial_links_20260725.rds`, is provided in this repository.
Users may therefore skip this procedure and directly use the provided RDS file.

## 2. Assign Mindat geomaterial IDs to minerals

**R script:** `Get_ID.R`

This script assigns Mindat geomaterial IDs to a user-provided mineral list
based on mineral names. The provided `geomaterial_id_assignment.csv` file is
used for the name-to-ID assignment.

This procedure does not require API access.

## 3. Identify leaf localities for minerals

**R script:** `Get_leaf_locality.R`

This script uses the mineral list with assigned Mindat geomaterial IDs to
retrieve and evaluate associated locality records and identify the final leaf
localities for each mineral.

This procedure requires a valid **Mindat API token**. Retrieved locality
metadata are cached locally to avoid repeated API requests. Mineral-level
checkpoints are also used so that an interrupted run can be resumed without
reprocessing completed minerals.

The main summary output includes the number of final leaf localities
(`n_final_leaf`) and their Mindat locality IDs (`final_locality_ids`) for
each mineral.

## 4. Build mineral co-occurrence matrices

**R script:** `Get_matrix.R`

This script uses the leaf-locality results to construct mineral co-occurrence
matrices. Minerals can be selected according to a user-defined minimum and
maximum number of leaf localities (`min_locality` and `max_locality`).

Two matrices are generated:

- **Raw co-occurrence matrix:** the number of leaf localities shared by each
  pair of minerals.
- **Scaled-to-100 co-occurrence matrix:** the shared-locality count divided by
  the smaller locality count of the two minerals and multiplied by 100.

This procedure is performed locally and does not require API access.

## Mindat API access

Procedures 1 and 3 require access to the Mindat API and a valid API token.
API tokens are not included in this repository. Users should obtain and use
their own Mindat API token before running the corresponding scripts.

Procedures 2 and 4 do not require API access.

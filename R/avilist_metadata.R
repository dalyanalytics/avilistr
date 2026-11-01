
#' AviList Field Metadata
#'
#' Metadata describing all fields in the AviList datasets, including
#' field descriptions, data types, sources, source versions, and availability
#' in different dataset versions.
#'
#' @format A tibble with metadata for all AviList fields:
#' \describe{
#'   \item{field_name}{Name of the field in the dataset}
#'   \item{description}{Description of the field content, from the "How to use" sheet of the AviList file}
#'   \item{data_type}{Data type (character, numeric, etc.)}
#'   \item{source}{Original source of the data (AviList, Clements, etc.)}
#'   \item{source_version}{Version of the source the field is aligned with (e.g. v2026 for Clements, v10 for BirdLife)}
#'   \item{in_full_version}{Logical, whether field is in the full dataset}
#'   \item{in_short_version}{Logical, whether field is in the official short version, returned by `avilist(short = TRUE)`}
#' }
#'
#' @source Generated from the "How to use" sheet and column headers of the
#'   AviList extended file
#'
#' @examples
#' # View all field descriptions
#' data(avilist_metadata)
#'
#' # Fields in short version only
#' avilist_metadata[avilist_metadata$in_short_version, ]
#'
#' # Fields from specific sources
#' avilist_metadata[avilist_metadata$source == "Clements", ]
#'
"avilist_metadata"


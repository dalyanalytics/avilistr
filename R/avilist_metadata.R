
#' AviList Field Metadata
#'
#' Metadata describing all fields in the AviList datasets, including
#' field descriptions, data types, sources, and availability in different
#' dataset versions.
#'
#' @format A tibble with metadata for all AviList fields:
#' \describe{
#'   \item{field_name}{Name of the field in the dataset}
#'   \item{description}{Human-readable description of the field content}
#'   \item{data_type}{Data type (character, numeric, etc.)}
#'   \item{source}{Original source of the data (AviList, Clements, etc.)}
#'   \item{in_full_version}{Logical, whether field is in the full dataset}
#'   \item{in_short_version}{Logical, whether field is in the short dataset}
#' }
#'
#' @source Generated from AviList field analysis
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


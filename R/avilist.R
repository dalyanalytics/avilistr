#' Get the AviList checklist
#'
#' Returns the AviList checklist shipped with the package, with either all
#' fields (the extended version) or only the fields of the official AviList
#' short version.
#'
#' @param short Logical. If `TRUE`, return only the fields of the official
#'   short version (those with `in_short_version` in [avilist_metadata]).
#'   Defaults to `FALSE`, which returns all fields.
#'
#' @return A data frame with one row per taxon (order, family, genus, species
#'   and subspecies). See [avilist_metadata] for field descriptions.
#'
#' @seealso [avilist_metadata]
#'
#' @examples
#' # All fields
#' full <- avilist()
#' dim(full)
#'
#' # Fields of the official short version only
#' short <- avilist(short = TRUE)
#' names(short)
#'
#' @export
avilist <- function(short = FALSE) {
  if (!is.logical(short) || length(short) != 1 || is.na(short)) {
    stop("`short` must be TRUE or FALSE.", call. = FALSE)
  }

  # The checklist is shipped under its versioned name (e.g. avilist_2026)
  lazydata <- getNamespaceInfo("avilistr", "lazydata")
  name <- grep("^avilist_[0-9]{4}$", ls(lazydata), value = TRUE)
  data <- get(name, envir = lazydata)

  if (short) {
    metadata <- get("avilist_metadata", envir = lazydata)
    short_fields <- metadata$field_name[metadata$in_short_version]
    data <- data[, names(data) %in% short_fields]
  }

  data
}

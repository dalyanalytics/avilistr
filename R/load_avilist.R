#' Load AviList Data
#'
#' Load the AviList global bird checklist data with optional filtering.
#' This is the main function to access the unified global bird taxonomy
#' created by the AviList Core Team.
#'
#' @param version Character. Which version to load: "full" (default) includes all
#'   fields from the extended AviList dataset, "short" includes only essential
#'   taxonomic fields from the official short version.
#' @param filter_rank Character vector. Filter by taxonomic ranks. Options include
#'   "species", "subspecies", "genus", "family", "order". Default NULL returns all ranks.
#' @param families Character vector. Filter by specific family names (e.g., "Turdidae").
#'   Default NULL returns all families.
#' @param orders Character vector. Filter by specific order names (e.g., "Passeriformes").
#'   Default NULL returns all orders.
#' @param region Character. Filter by biogeographic region based on type locality
#'   (if available). Default NULL returns all regions.
#'
#' @return A tibble containing the requested AviList data with additional attributes:
#'   \itemize{
#'     \item version: Which dataset version was loaded
#'     \item loaded_at: Timestamp of when data was loaded
#'     \item filters: List of applied filters
#'     \item summary: Basic statistics about the loaded data
#'   }
#'
#' @details
#' The AviList dataset represents the first unified global checklist of bird species,
#' harmonizing previous differences between IOC, Clements, and BirdLife taxonomies.
#'
#' **Version differences:**
#' \itemize{
#'   \item **full**: All 26 fields including nomenclatural details, URLs, and bibliographic info
#'   \item **short**: Essential taxonomic fields only, faster loading for basic operations
#' }
#'
#' **Performance tips:**
#' \itemize{
#'   \item Use \code{version = "short"} for faster loading when you don't need all fields
#'   \item Filter at load time rather than after loading for better memory efficiency
#'   \item Use \code{filter_rank = "species"} to exclude subspecies for most analyses
#' }
#'
#' @examples
#' # Load full dataset
#' avilist_full <- load_avilist()
#'
#' # Load only species (no subspecies) from short version
#' species_only <- load_avilist(version = "short", filter_rank = "species")
#'
#' # Load specific bird families
#' raptors <- load_avilist(
#'   families = c("Accipitridae", "Falconidae", "Strigidae"),
#'   filter_rank = "species"
#' )
#'
#' # Load all passerines (songbirds)
#' songbirds <- load_avilist(
#'   orders = "Passeriformes",
#'   version = "short"
#' )
#'
#' # Examine the loaded data
#' str(species_only)
#' attr(species_only, "summary")
#'
#' @seealso
#' \code{\link{avilist_2025}} for the full dataset object,
#' \code{\link{avilist_2025_short}} for the short dataset object,
#' \code{\link{avilist_stats}} for summary statistics
#'
#' @source AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025.
#'   \url{https://doi.org/10.2173/avilist.v2025}
#'
#' @export
load_avilist <- function(version = "full",
                         filter_rank = NULL,
                         families = NULL,
                         orders = NULL,
                         region = NULL) {

  # =============================================================================
  # INPUT VALIDATION
  # =============================================================================

  # Validate version parameter
  version <- match.arg(version, c("full", "short"))

  # Validate filter_rank if provided
  if (!is.null(filter_rank)) {
    valid_ranks <- c("species", "subspecies", "genus", "family", "order")
    if (!all(filter_rank %in% valid_ranks)) {
      invalid_ranks <- setdiff(filter_rank, valid_ranks)
      stop("Invalid taxonomic rank(s): ", paste(invalid_ranks, collapse = ", "),
           "\nValid options are: ", paste(valid_ranks, collapse = ", "))
    }
  }

  # =============================================================================
  # LOAD APPROPRIATE DATASET
  # =============================================================================

  # Load the requested version
  if (version == "full") {
    if (!exists("avilist_2025")) {
      # Try to load the data if not already available
      tryCatch({
        utils::data("avilist_2025", package = "avilistr", envir = environment())
      }, error = function(e) {
        stop("Could not load avilist_2025 dataset. Make sure the avilistr package is properly installed.")
      })
    }
    data <- avilist_2025

  } else { # version == "short"
    if (!exists("avilist_2025_short")) {
      tryCatch({
        utils::data("avilist_2025_short", package = "avilistr", envir = environment())
      }, error = function(e) {
        stop("Could not load avilist_2025_short dataset. Make sure the avilistr package is properly installed.")
      })
    }
    data <- avilist_2025_short
  }

  # Store original data size for summary
  original_rows <- nrow(data)

  # =============================================================================
  # APPLY FILTERS
  # =============================================================================

  # Apply taxonomic rank filter
  if (!is.null(filter_rank)) {
    if ("Taxon_rank" %in% names(data)) {
      data <- dplyr::filter(data, Taxon_rank %in% filter_rank)
    } else {
      warning("Taxon_rank column not found. Rank filtering skipped.")
    }
  }

  # Apply family filter
  if (!is.null(families)) {
    if ("Family" %in% names(data)) {
      # Handle case-insensitive matching
      available_families <- unique(data$Family)
      matched_families <- families[families %in% available_families]

      if (length(matched_families) == 0) {
        warning("No matching families found. Available families include: ",
                paste(head(available_families, 5), collapse = ", "),
                ifelse(length(available_families) > 5, "...", ""))
        return(data[0, ]) # Return empty tibble with correct structure
      }

      if (length(matched_families) < length(families)) {
        missing_families <- setdiff(families, matched_families)
        warning("Some families not found: ", paste(missing_families, collapse = ", "))
      }

      data <- dplyr::filter(data, Family %in% matched_families)
    } else {
      warning("Family column not found. Family filtering skipped.")
    }
  }

  # Apply order filter
  if (!is.null(orders)) {
    if ("Order" %in% names(data)) {
      available_orders <- unique(data$Order)
      matched_orders <- orders[orders %in% available_orders]

      if (length(matched_orders) == 0) {
        warning("No matching orders found. Available orders include: ",
                paste(head(available_orders, 5), collapse = ", "),
                ifelse(length(available_orders) > 5, "...", ""))
        return(data[0, ]) # Return empty tibble with correct structure
      }

      if (length(matched_orders) < length(orders)) {
        missing_orders <- setdiff(orders, matched_orders)
        warning("Some orders not found: ", paste(missing_orders, collapse = ", "))
      }

      data <- dplyr::filter(data, Order %in% matched_orders)
    } else {
      warning("Order column not found. Order filtering skipped.")
    }
  }

  # Apply region filter (if Type_locality field exists)
  if (!is.null(region)) {
    if ("Type_locality" %in% names(data)) {
      # Simple pattern matching for region in type locality
      region_pattern <- paste(region, collapse = "|")
      data <- dplyr::filter(data, stringr::str_detect(Type_locality,
                                                      stringr::regex(region_pattern, ignore_case = TRUE)))

      if (nrow(data) == 0) {
        warning("No records found for region(s): ", paste(region, collapse = ", "))
      }
    } else {
      warning("Type_locality column not found. Region filtering skipped.")
    }
  }

  # =============================================================================
  # CREATE SUMMARY STATISTICS
  # =============================================================================

  summary_stats <- list(
    version = version,
    original_records = original_rows,
    filtered_records = nrow(data),
    records_filtered_out = original_rows - nrow(data),
    filter_efficiency = round((1 - nrow(data)/original_rows) * 100, 1),

    # Taxonomic breakdown
    species_count = sum(data$Taxon_rank == "species", na.rm = TRUE),
    subspecies_count = sum(data$Taxon_rank == "subspecies", na.rm = TRUE),
    families_count = length(unique(data$Family)),
    orders_count = length(unique(data$Order)),

    # Applied filters
    filters_applied = list(
      rank = filter_rank,
      families = families,
      orders = orders,
      region = region
    )
  )

  # =============================================================================
  # ADD ATTRIBUTES AND RETURN
  # =============================================================================

  # Add useful attributes to the returned data
  attr(data, "version") <- version
  attr(data, "loaded_at") <- Sys.time()
  attr(data, "filters") <- list(
    rank = filter_rank,
    families = families,
    orders = orders,
    region = region
  )
  attr(data, "summary") <- summary_stats
  attr(data, "citation") <- "AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025. https://doi.org/10.2173/avilist.v2025"

  # Add a custom class for potential S3 methods
  class(data) <- c("avilist_data", class(data))

  return(data)
}

#' Get AviList Summary Statistics
#'
#' Display summary statistics for AviList data, either from a loaded dataset
#' or by loading fresh data.
#'
#' @param data Optional tibble. AviList data to summarize. If NULL, loads the full dataset.
#' @param version Character. If data is NULL, which version to load for summary.
#'
#' @return Named list with summary statistics including record counts, taxonomic breakdown,
#'   version information, and citation details.
#'
#' @examples
#' # Get statistics for full dataset
#' stats <- avilist_stats()
#' print(stats)
#'
#' # Get statistics for loaded data
#' my_data <- load_avilist(filter_rank = "species")
#' stats <- avilist_stats(my_data)
#'
#' # Get statistics for short version
#' stats_short <- avilist_stats(version = "short")
#'
#' @export
avilist_stats <- function(data = NULL, version = "full") {

  if (is.null(data)) {
    message("Loading AviList data for statistics...")
    data <- load_avilist(version = version)
  }

  # Extract summary from attributes if available
  if (!is.null(attr(data, "summary"))) {
    return(attr(data, "summary"))
  }

  # Calculate fresh statistics
  stats <- list(
    total_records = nrow(data),
    species_count = sum(data$Taxon_rank == "species", na.rm = TRUE),
    subspecies_count = sum(data$Taxon_rank == "subspecies", na.rm = TRUE),
    genera_count = length(unique(stringr::str_extract(data$Scientific_name[data$Taxon_rank == "species"], "^[A-Z][a-z]+"))),
    families_count = length(unique(data$Family)),
    orders_count = length(unique(data$Order)),
    fields_count = ncol(data),
    version = attr(data, "version") %||% version,
    loaded_at = attr(data, "loaded_at") %||% "unknown",
    last_updated = "2025-06-11", # AviList v2025 release date
    citation = "AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025. https://doi.org/10.2173/avilist.v2025"
  )

  return(stats)
}

#' Print method for avilist_data objects
#'
#' @param x An avilist_data object
#' @param ... Additional arguments passed to print
#' @export
print.avilist_data <- function(x, ...) {
  # Print the tibble normally
  NextMethod()

  # Add summary information
  summary <- attr(x, "summary")
  if (!is.null(summary)) {
    cat("\n")
    cat("AviList Summary:\n")
    cat("  Version:", summary$version, "\n")
    cat("  Records:", summary$filtered_records,
        ifelse(summary$original_records != summary$filtered_records,
               paste0(" (", summary$filter_efficiency, "% filtered)"), ""), "\n")
    cat("  Species:", summary$species_count, "\n")
    cat("  Families:", summary$families_count, "\n")
    cat("  Orders:", summary$orders_count, "\n")

    # Show applied filters
    filters <- summary$filters_applied
    active_filters <- filters[!sapply(filters, is.null)]
    if (length(active_filters) > 0) {
      cat("  Filters:", paste(names(active_filters), collapse = ", "), "\n")
    }
  }

  invisible(x)
}

# Utility function for null-default operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

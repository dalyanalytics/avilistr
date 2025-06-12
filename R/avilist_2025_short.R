
#' AviList Global Avian Checklist v2025 (Short Version)
#'
#' The essential fields from the AviList dataset containing core taxonomic
#' information for all bird species and subspecies as of June 2025. This is
#' the official short version provided by the AviList team, optimized for
#' faster loading and basic taxonomic operations.
#'
#' @format A tibble with 33684 rows and 14 columns:
#' \describe{{
#'   item{Authority}{Author and year of original description}
#'   item{AvibaseID}{Avibase database identifier}
#'   item{Bibliographic_details}{Bibliographic details of original description}
#'   item{Decision_summary}{Data field: Decision summary}
#'   item{English_name_AviList}{English common name (AviList)}
#'   item{Extinct_or_possibly_extinct}{Data field: Extinct or possibly extinct}
#'   item{Family}{Taxonomic family}
#'   item{Family_English_name}{English name of the family}
#'   item{IUCN_Red_List_Category}{Data field: IUCN Red List Category}
#'   item{Order}{Taxonomic order}
#'   item{Range}{Data field: Range}
#'   item{Scientific_name}{Scientific binomial name}
#'   item{Sequence}{Sequential numbering for taxonomic order}
#'   item{Taxon_rank}{Taxonomic rank (species, subspecies, etc.)}
#' }}
#'
#' @source AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025.
#'   \url{{https://doi.org/10.2173/avilist.v2025}}
#'
#' @seealso \code{{\link{{avilist_2025}}}} for the full version with all available fields
#'
#' @examples
#' # Load the short dataset (faster loading)
#' data(avilist_2025_short)
#'
#' # View summary
#' str(avilist_2025_short)
#'
#' # Count species by family
#' avilist_2025_short %>%
#'   filter(Taxon_rank == "species") %>%
#'   count(Family, sort = TRUE) %>%
#'   head(10)
#'
#' # Search for species
#' avilist_2025_short %>%
#'   filter(str_detect(Scientific_name, "Turdus"))
#'
"avilist_2025_short"



#' AviList Global Avian Checklist v2025 (Full Version)
#'
#' The complete AviList dataset containing all bird species, subspecies, and
#' taxonomic information as of June 2025. This is the extended version with
#' all available fields including nomenclatural details, bibliographic
#' information, and external database links.
#'
#' @format A tibble with 33684 rows and 26 columns:
#' \describe{{
#'   item{Sequence}{Sequential numbering for taxonomic order}
#'   item{Taxon_rank}{Taxonomic rank (species, subspecies, etc.)}
#'   item{Order}{Taxonomic order}
#'   item{Family}{Taxonomic family}
#'   item{Family_English_name}{English name of the family}
#'   item{Scientific_name}{Scientific binomial name}
#'   item{Authority}{Author and year of original description}
#'   item{English_name_AviList}{English common name (AviList)}
#'   item{English_name_Clements_v2024}{English common name (Clements 2024)}
#'   item{Species_code_Cornell_Lab}{Cornell Lab species code}
#'   item{AvibaseID}{Avibase database identifier}
#'   item{BirdLife_DataZone_URL}{BirdLife DataZone species page URL}
#'   item{Birds_of_the_World_URL}{Birds of the World species account URL}
#'   item{Original_description_URL}{URL to original species description}
#' }}
#'
#' @source AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025.
#'   \url{{https://doi.org/10.2173/avilist.v2025}}
#'
#' @seealso \code{{\link{{avilist_2025_short}}}} for the short version with essential fields only
#'
#' @examples
#' # Load the full dataset
#' data(avilist_2025)
#'
#' # View summary
#' str(avilist_2025)
#'
#' # Count species by order
#' avilist_2025 %>%
#'   filter(Taxon_rank == "species") %>%
#'   count(Order, sort = TRUE)
#'
#' # Access external database links
#' avilist_2025 %>%
#'   filter(!is.na(BirdLife_DataZone_URL)) %>%
#'   select(Scientific_name, BirdLife_DataZone_URL) %>%
#'   head()
#'
"avilist_2025"


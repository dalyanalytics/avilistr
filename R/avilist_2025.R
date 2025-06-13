
#' AviList Global Avian Checklist v2025 (Full Version)
#'
#' The complete AviList dataset containing all bird species, subspecies, and
#' taxonomic information as of June 2025. This is the extended version with
#' all available fields including nomenclatural details, bibliographic
#' information, and external database links.
#'
#' @format A tibble with 33684 rows and 26 columns:
#' \describe{{
#'   \item{Authority}{Author and year of original description}
#'   \item{AvibaseID}{Avibase database identifier}
#'   \item{Bibliographic_details}{Bibliographic details of original description}
#'   \item{BirdLife_DataZone_URL}{BirdLife DataZone species page URL}
#'   \item{Birds_of_the_World_URL}{Birds of the World species account URL}
#'   \item{Decision_summary}{Data field: Decision summary}
#'   \item{English_name_AviList}{English common name (AviList)}
#'   \item{English_name_BirdLife_v9}{Name field: English_name_BirdLife_v9}
#'   \item{English_name_Clements_v2024}{English common name (Clements 2024)}
#'   \item{Extinct_or_possibly_extinct}{Data field: Extinct or possibly extinct}
#'   \item{Family}{Taxonomic family}
#'   \item{Family_English_name}{English name of the family}
#'   \item{Gender_of_genus}{Grammatical gender of the genus name}
#'   \item{IUCN_Red_List_Category}{Data field: IUCN Red List Category}
#'   \item{Order}{Taxonomic order}
#'   \item{Original_description_URL}{URL to original species description}
#'   \item{Proposal_number}{Data field: Proposal number}
#'   \item{Protonym}{Original name as first published}
#'   \item{Range}{Data field: Range}
#'   \item{Scientific_name}{Scientific binomial name}
#'   \item{Sequence}{Sequential numbering for taxonomic order}
#'   \item{Species_code_Cornell_Lab}{Cornell Lab species code}
#'   \item{Taxon_rank}{Taxonomic rank (species, subspecies, etc.)}
#'   \item{Title_of_original_description}{Title of the original species description}
#'   \item{Type_locality}{Type locality where species was first collected}
#'   \item{Type_species_of_genus}{Type species of the genus}
#' }}
#'
#' @source AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025.
#'   \url{https://doi.org/10.2173/avilist.v2025}
#'
#' @examples
#' # Load the full dataset
#' data(avilist_2025)
#'
#' # View summary
#' str(avilist_2025)

"avilist_2025"


# =============================================================================
# avilistr Package - Data Preparation Script
# data-raw/prepare_data.R
#
# This script processes the raw AviList Excel file and creates clean R data objects
# for inclusion in the avilistr package.
# =============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tibble)
library(usethis)
library(tidyr)

# =============================================================================
# 1. LOAD AND CLEAN RAW DATA
# =============================================================================

# Read both AviList Excel files
message("Reading AviList Excel files...")

# Full/extended version
raw_avilist_full <- read_excel(
  "data-raw/AviList-v2025-11Jun-extended.xlsx",
  sheet = "AviList v2025 extended",
  col_types = "text"  # Read everything as text initially
)

# Official short version
raw_avilist_short <- read_excel(
  "data-raw/AviList-v2025-11Jun-short.xlsx",
  col_types = "text"
)

message(paste("Loaded full version:", nrow(raw_avilist_full), "records with", ncol(raw_avilist_full), "columns"))
message(paste("Loaded short version:", nrow(raw_avilist_short), "records with", ncol(raw_avilist_short), "columns"))

# Display column names for verification
message("Full version column names:")
print(names(raw_avilist_full))
message("\nShort version column names:")
print(names(raw_avilist_short))

# =============================================================================
# 2. DATA CLEANING AND TYPE CONVERSION
# =============================================================================

message("Cleaning and processing full dataset...")

avilist_2025 <- raw_avilist_full %>%
  # Clean column names (remove spaces, standardize)
  rename_with(~ str_replace_all(.x, "\\s+", "_")) %>%
  rename_with(~ str_replace_all(.x, "[^A-Za-z0-9_]", "_")) %>%
  rename_with(~ str_remove_all(.x, "_+$")) %>%  # Remove trailing underscores

  # Convert appropriate columns to numeric
  mutate(
    Sequence = as.numeric(Sequence),

    # Clean and standardize text fields
    Scientific_name = str_trim(Scientific_name),
    English_name_AviList = str_trim(English_name_AviList),
    English_name_Clements_v2024 = str_trim(English_name_Clements_v2024),

    # Standardize taxonomic ranks
    Taxon_rank = str_to_lower(str_trim(Taxon_rank)),

    # Clean family and order names
    Order = str_trim(Order),
    Family = str_trim(Family),
    Family_English_name = str_trim(Family_English_name),

    # Clean authority information
    Authority = str_trim(Authority),

    # Process URLs - ensure they're valid or NA
    BirdLife_DataZone_URL = case_when(
      str_detect(BirdLife_DataZone_URL, "^https?://") ~ BirdLife_DataZone_URL,
      BirdLife_DataZone_URL == "" ~ NA_character_,
      TRUE ~ NA_character_
    ),

    Birds_of_the_World_URL = case_when(
      str_detect(Birds_of_the_World_URL, "^https?://") ~ Birds_of_the_World_URL,
      Birds_of_the_World_URL == "" ~ NA_character_,
      TRUE ~ NA_character_
    ),

    Original_description_URL = case_when(
      str_detect(Original_description_URL, "^https?://") ~ Original_description_URL,
      Original_description_URL == "" ~ NA_character_,
      TRUE ~ NA_character_
    ),

    # Clean species codes
    Species_code_Cornell_Lab = str_trim(Species_code_Cornell_Lab),
    AvibaseID = str_trim(AvibaseID) #,

    # Handle empty strings as NA
    # across(everything(), ~ case_when(
    #   .x == "" ~ NA_character_,
    #   .x == "NA" ~ NA_character_,
    #   TRUE ~ .x
    # ))
  ) %>%

  # Arrange by taxonomic sequence
  arrange(Sequence)

# =============================================================================
# 3. PROCESS OFFICIAL SHORT VERSION
# =============================================================================

message("Processing official short version...")

avilist_2025_short <- raw_avilist_short %>%
  # Apply same cleaning as full version
  rename_with(~ str_replace_all(.x, "\\s+", "_")) %>%
  rename_with(~ str_replace_all(.x, "[^A-Za-z0-9_]", "_")) %>%
  rename_with(~ str_remove_all(.x, "_+$")) %>%

  # Convert appropriate columns to numeric
  mutate(
    Sequence = as.numeric(Sequence),

    # Clean and standardize text fields (adjust field names as needed)
    Scientific_name = str_trim(Scientific_name),
    across(contains("English_name"), ~ str_trim(.x)),

    # Standardize taxonomic ranks
    Taxon_rank = str_to_lower(str_trim(Taxon_rank)),

    # Clean family and order names
    across(c(Order, Family), ~ str_trim(.x)),

    # Clean authority information
    Authority = str_trim(Authority) #,

    # Handle empty strings as NA
    # across(everything(), ~ case_when(
    #   .x == "" ~ NA_character_,
    #   .x == "NA" ~ NA_character_,
    #   TRUE ~ .x
    # ))
  ) %>%

  # Arrange by taxonomic sequence
  arrange(Sequence)

# Compare field overlap between versions
common_fields <- intersect(names(avilist_2025), names(avilist_2025_short))
full_only_fields <- setdiff(names(avilist_2025), names(avilist_2025_short))
short_only_fields <- setdiff(names(avilist_2025_short), names(avilist_2025))

message(paste("Common fields:", length(common_fields)))
message(paste("Full-only fields:", length(full_only_fields)))
message(paste("Short-only fields:", length(short_only_fields)))

if (length(short_only_fields) > 0) {
  message("Fields unique to short version:")
  print(short_only_fields)
}

# =============================================================================
# 4. CREATE METADATA AND REFERENCE OBJECTS
# =============================================================================

message("Creating metadata objects...")

# Field descriptions (update based on actual fields found)
avilist_metadata <- tribble(
  ~field_name, ~description, ~data_type, ~source, ~in_short_version,
  "Sequence", "Sequential numbering for taxonomic order", "numeric", "AviList", TRUE,
  "Taxon_rank", "Taxonomic rank (species, subspecies, etc.)", "character", "AviList", TRUE,
  "Order", "Taxonomic order", "character", "AviList", TRUE,
  "Family", "Taxonomic family", "character", "AviList", TRUE,
  "Family_English_name", "English name of the family", "character", "AviList", TRUE,
  "Scientific_name", "Scientific binomial name", "character", "AviList", TRUE,
  "Authority", "Author and year of original description", "character", "AviList", TRUE,
  "English_name_AviList", "English common name (AviList)", "character", "AviList", TRUE,
  "English_name_Clements_v2024", "English common name (Clements 2024)", "character", "Clements", FALSE,
  "Species_code_Cornell_Lab", "Cornell Lab species code", "character", "Cornell Lab", FALSE,
  "AvibaseID", "Avibase database identifier", "character", "Avibase", FALSE,
  "BirdLife_DataZone_URL", "BirdLife DataZone species page URL", "character", "BirdLife", FALSE,
  "Birds_of_the_World_URL", "Birds of the World species account URL", "character", "Cornell Lab", FALSE,
  "Original_description_URL", "URL to original species description", "character", "Various", FALSE
) %>%
  # Update based on actual fields found
  mutate(
    in_short_version = field_name %in% names(avilist_2025_short),
    in_full_version = field_name %in% names(avilist_2025)
  )

# Summary statistics
avilist_stats_2025 <- list(
  version = "2025",
  release_date = "2025-06-11",
  total_records = nrow(avilist_2025),
  species_count = sum(avilist_2025$Taxon_rank == "species", na.rm = TRUE),
  subspecies_count = sum(avilist_2025$Taxon_rank == "subspecies", na.rm = TRUE),
  genera_count = length(unique(str_extract(avilist_2025$Scientific_name[avilist_2025$Taxon_rank == "species"], "^[A-Z][a-z]+"))),
  families_count = length(unique(avilist_2025$Family)),
  orders_count = length(unique(avilist_2025$Order)),
  fields_count = ncol(avilist_2025),
  citation = "AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025. https://doi.org/10.2173/avilist.v2025"
)

# =============================================================================
# 5. DATA VALIDATION AND QUALITY CHECKS
# =============================================================================

message("Performing data quality checks...")

# Check for duplicates
duplicates <- avilist_2025 %>%
  filter(Taxon_rank == "species") %>%
  group_by(Scientific_name) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicates) > 0) {
  warning(paste("Found", nrow(duplicates), "duplicate species records"))
  print(duplicates %>% select(Scientific_name, Order, Family))
}

# Check for missing essential data
missing_sci_names <- sum(is.na(avilist_2025$Scientific_name))
missing_families <- sum(is.na(avilist_2025$Family))
missing_orders <- sum(is.na(avilist_2025$Order))

message(paste("Missing scientific names:", missing_sci_names))
message(paste("Missing families:", missing_families))
message(paste("Missing orders:", missing_orders))

# Validate taxonomic hierarchy
invalid_ranks <- avilist_2025 %>%
  filter(!Taxon_rank %in% c("species", "subspecies", "genus", "family", "order")) %>%
  distinct(Taxon_rank)

if (nrow(invalid_ranks) > 0) {
  warning("Found unexpected taxonomic ranks:")
  print(invalid_ranks)
}

# =============================================================================
# 6. CREATE INTERNAL REFERENCE DATA
# =============================================================================

message("Creating internal reference objects...")

# Common name variations for fuzzy matching
name_variations <- avilist_2025 %>%
  filter(Taxon_rank == "species") %>%
  select(Scientific_name, English_name_AviList, English_name_Clements_v2024) %>%
  pivot_longer(
    cols = c(English_name_AviList, English_name_Clements_v2024),
    names_to = "source",
    values_to = "common_name",
    values_drop_na = TRUE
  ) %>%
  distinct() %>%
  arrange(Scientific_name)

# Authority codes and abbreviations
authority_patterns <- avilist_2025 %>%
  filter(!is.na(Authority), Taxon_rank == "species") %>%
  count(Authority, sort = TRUE) %>%
  slice_head(n = 100)  # Top 100 most common authorities

# Order information with statistics
order_info <- avilist_2025 %>%
  filter(Taxon_rank == "species") %>%
  group_by(Order) %>%
  summarise(
    species_count = n(),
    families_count = n_distinct(Family),
    example_families = paste(head(unique(Family), 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(species_count))

# Family information with statistics
family_info <- avilist_2025 %>%
  filter(Taxon_rank == "species") %>%
  group_by(Order, Family, Family_English_name) %>%
  summarise(
    species_count = n(),
    example_species = paste(head(Scientific_name, 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(Order, desc(species_count))

# =============================================================================
# 7. SAVE DATA OBJECTS
# =============================================================================

message("Saving data objects...")

# Save main datasets
usethis::use_data(avilist_2025, overwrite = TRUE)
usethis::use_data(avilist_2025_short, overwrite = TRUE)
usethis::use_data(avilist_metadata, overwrite = TRUE)

# Save internal data (not exported to users)
usethis::use_data(
  name_variations,
  authority_patterns,
  order_info,
  family_info,
  avilist_stats_2025,
  internal = TRUE,
  overwrite = TRUE
)

# =============================================================================
# 8. CREATE DOCUMENTATION STUBS
# =============================================================================

message("Creating documentation templates...")

# Create data documentation template
data_doc_template <- '
#\' AviList Global Avian Checklist v2025
#\'
#\' The complete AviList dataset containing all bird species, subspecies, and
#\' taxonomic information as of June 2025.
#\'
#\' @format A tibble with {nrow} rows and {ncol} columns:
#\' \\describe{{
{field_descriptions}
#\' }}
#\'
#\' @source AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025.
#\'   \\url{{https://doi.org/10.2173/avilist.v2025}}
#\'
#\' @examples
#\' # Load the full dataset
#\' data(avilist_2025)
#\'
#\' # View summary
#\' str(avilist_2025)
#\'
#\' # Count species by order
#\' avilist_2025 %>%
#\'   filter(Taxon_rank == "species") %>%
#\'   count(Order, sort = TRUE)
#\'
"avilist_2025"
'

# Generate field descriptions for documentation
field_descriptions <- avilist_metadata %>%
  mutate(desc_line = paste0("#\'   \\item{", field_name, "}{", description, "}")) %>%
  pull(desc_line) %>%
  paste(collapse = "\n")

# Fill in template
complete_doc <- str_replace_all(data_doc_template,
                                c("\\{nrow\\}" = as.character(nrow(avilist_2025)),
                                  "\\{ncol\\}" = as.character(ncol(avilist_2025)),
                                  "\\{field_descriptions\\}" = field_descriptions))

# Write documentation file
writeLines(complete_doc, "R/avilist_2025.R")

# =============================================================================
# 9. FINAL SUMMARY
# =============================================================================

message("\n" %>% paste(rep("=", 60), collapse = ""))
message("DATA PREPARATION COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(paste("Total records processed:", nrow(avilist_2025)))
message(paste("Species count:", avilist_stats_2025$species_count))
message(paste("Subspecies count:", avilist_stats_2025$subspecies_count))
message(paste("Families count:", avilist_stats_2025$families_count))
message(paste("Orders count:", avilist_stats_2025$orders_count))
message(paste("Fields in full dataset:", ncol(avilist_2025)))
message(paste("Fields in short dataset:", ncol(avilist_2025_short)))
message("\nFiles created:")
message("- data/avilist_2025.rda")
message("- data/avilist_2025_short.rda")
message("- data/avilist_metadata.rda")
message("- data/sysdata.rda (internal)")
message("- R/data.R (documentation)")
message("\nReady for package development!")

# Display top families by species count
message("\nTop 10 families by species count:")
print(family_info %>%
        slice_head(n = 10) %>%
        select(Family, Family_English_name, species_count))

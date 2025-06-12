# =============================================================================
# avilistr Package - Data Preparation Script
# data-raw/data_prep.R
#
# This script processes the raw AviList Excel file and creates clean R data objects
# for inclusion in the avilistr package.
# =============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(usethis)

# =============================================================================
# 1. LOAD AND CLEAN RAW DATA
# =============================================================================

# Read both AviList Excel files
message("Reading AviList Excel files...")

# Full/extended version
raw_avilist_full <- read_excel(
  "data-raw/AviList-v2025-11Jun-extended.xlsx",
  sheet = "AviList v2025 extended", # the default sheet is the first one
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
    AvibaseID = str_trim(AvibaseID)
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
    Authority = str_trim(Authority)
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

# Create comprehensive field descriptions from actual data
message("Generating field descriptions from actual data...")

# Get all fields from both datasets
full_fields <- names(avilist_2025)
short_fields <- names(avilist_2025_short)
all_fields <- unique(c(full_fields, short_fields))

message(paste("Full dataset has", length(full_fields), "fields"))
message(paste("Short dataset has", length(short_fields), "fields"))
message(paste("Total unique fields:", length(all_fields)))

# Print all field names for verification
message("All field names found:")
print(all_fields)

# Create comprehensive metadata table based on actual fields
avilist_metadata <- tibble(
  field_name = all_fields
) %>%
  mutate(
    in_full_version = field_name %in% full_fields,
    in_short_version = field_name %in% short_fields,

    # Generate descriptions based on field names (comprehensive patterns)
    description = case_when(
      field_name == "Sequence" ~ "Sequential numbering for taxonomic order",
      field_name == "Taxon_rank" ~ "Taxonomic rank (species, subspecies, etc.)",
      field_name == "Order" ~ "Taxonomic order",
      field_name == "Family" ~ "Taxonomic family",
      str_detect(tolower(field_name), "family.*english") ~ "English name of the family",
      field_name == "Scientific_name" ~ "Scientific binomial name",
      field_name == "Authority" ~ "Author and year of original description",
      str_detect(tolower(field_name), "english.*name.*avilist") ~ "English common name (AviList)",
      str_detect(tolower(field_name), "english.*name.*clements") ~ "English common name (Clements 2024)",
      str_detect(tolower(field_name), "bibliographic") ~ "Bibliographic details of original description",
      str_detect(tolower(field_name), "species.*code.*cornell") ~ "Cornell Lab species code",
      field_name == "AvibaseID" ~ "Avibase database identifier",
      str_detect(tolower(field_name), "birdlife.*url") ~ "BirdLife DataZone species page URL",
      str_detect(tolower(field_name), "birds.*world.*url") ~ "Birds of the World species account URL",
      str_detect(tolower(field_name), "original.*description.*url") ~ "URL to original species description",
      str_detect(tolower(field_name), "gender.*genus") ~ "Grammatical gender of the genus name",
      str_detect(tolower(field_name), "type.*species") ~ "Type species of the genus",
      str_detect(tolower(field_name), "type.*locality") ~ "Type locality where species was first collected",
      str_detect(tolower(field_name), "title.*original") ~ "Title of the original species description",
      field_name == "Protonym" ~ "Original name as first published",

      # Additional common patterns
      str_detect(tolower(field_name), "url$") ~ paste("URL link for", str_replace(field_name, "_URL$", "")),
      str_detect(tolower(field_name), "code") ~ paste("Species code from", str_extract(field_name, "\\w+(?=_|$)")),
      str_detect(tolower(field_name), "id$") ~ paste("Database identifier for", str_replace(field_name, "ID$", "")),
      str_detect(tolower(field_name), "name") ~ paste("Name field:", field_name),

      # Fallback - just use the field name
      TRUE ~ paste("Data field:", str_replace_all(field_name, "_", " "))
    ),

    # Determine data type
    data_type = case_when(
      field_name == "Sequence" ~ "numeric",
      str_detect(tolower(field_name), "url") ~ "character (URL)",
      TRUE ~ "character"
    ),

    # Determine likely source
    source = case_when(
      str_detect(tolower(field_name), "clements") ~ "Clements",
      str_detect(tolower(field_name), "cornell") ~ "Cornell Lab",
      str_detect(tolower(field_name), "birdlife") ~ "BirdLife",
      str_detect(tolower(field_name), "avibase") ~ "Avibase",
      str_detect(tolower(field_name), "birds.*world") ~ "Cornell Lab",
      TRUE ~ "AviList"
    )
  ) %>%
  arrange(field_name)

# Print summary of field coverage
message("\nField description summary:")
message(paste("Fields in full version:", sum(avilist_metadata$in_full_version)))
message(paste("Fields in short version:", sum(avilist_metadata$in_short_version)))

# Show any fields that might have generic descriptions
generic_fields <- avilist_metadata %>%
  filter(str_detect(description, "^Data field:")) %>%
  pull(field_name)

if (length(generic_fields) > 0) {
  message("\nFields with generic descriptions (may need manual review):")
  print(generic_fields)
}

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

# Ensure we're in the right directory and data/ folder exists
if (!dir.exists("data")) {
  dir.create("data")
  message("Created data/ directory")
}

# Save main datasets
usethis::use_data(avilist_2025, overwrite = TRUE)
usethis::use_data(avilist_2025_short, overwrite = TRUE)
usethis::use_data(avilist_metadata, overwrite = TRUE)

# Save internal data (should go to data/sysdata.rda)
message("Saving internal data to data/sysdata.rda...")
usethis::use_data(
  name_variations,
  authority_patterns,
  order_info,
  family_info,
  avilist_stats_2025,
  internal = TRUE,
  overwrite = TRUE
)

# Verify sysdata.rda location
if (file.exists("data/sysdata.rda")) {
  message("✓ Internal data saved to data/sysdata.rda")
} else if (file.exists("R/sysdata.rda")) {
  message("⚠ Internal data was saved to R/sysdata.rda - moving to data/")
  file.rename("R/sysdata.rda", "data/sysdata.rda")
} else {
  warning("Could not locate sysdata.rda file")
}

# =============================================================================
# 8. CREATE DOCUMENTATION STUBS
# =============================================================================

message("Creating documentation templates...")

# Create data documentation template for FULL dataset
data_doc_template_full <- '
#\' AviList Global Avian Checklist v2025 (Full Version)
#\'
#\' The complete AviList dataset containing all bird species, subspecies, and
#\' taxonomic information as of June 2025. This is the extended version with
#\' all available fields including nomenclatural details, bibliographic
#\' information, and external database links.
#\'
#\' @format A tibble with {nrow} rows and {ncol} columns:
#\' \\describe{{
{field_descriptions}
#\' }}
#\'
#\' @source AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025.
#\'   \\url{{https://doi.org/10.2173/avilist.v2025}}
#\'
#\' @seealso \\code{{\\link{{avilist_2025_short}}}} for the short version with essential fields only
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
#\' # Access external database links
#\' avilist_2025 %>%
#\'   filter(!is.na(BirdLife_DataZone_URL)) %>%
#\'   select(Scientific_name, BirdLife_DataZone_URL) %>%
#\'   head()
#\'
"avilist_2025"
'

# Create data documentation template for SHORT dataset
data_doc_template_short <- '
#\' AviList Global Avian Checklist v2025 (Short Version)
#\'
#\' The essential fields from the AviList dataset containing core taxonomic
#\' information for all bird species and subspecies as of June 2025. This is
#\' the official short version provided by the AviList team, optimized for
#\' faster loading and basic taxonomic operations.
#\'
#\' @format A tibble with {nrow} rows and {ncol} columns:
#\' \\describe{{
{field_descriptions}
#\' }}
#\'
#\' @source AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025.
#\'   \\url{{https://doi.org/10.2173/avilist.v2025}}
#\'
#\' @seealso \\code{{\\link{{avilist_2025}}}} for the full version with all available fields
#\'
#\' @examples
#\' # Load the short dataset (faster loading)
#\' data(avilist_2025_short)
#\'
#\' # View summary
#\' str(avilist_2025_short)
#\'
#\' # Count species by family
#\' avilist_2025_short %>%
#\'   filter(Taxon_rank == "species") %>%
#\'   count(Family, sort = TRUE) %>%
#\'   head(10)
#\'
#\' # Search for species
#\' avilist_2025_short %>%
#\'   filter(str_detect(Scientific_name, "Turdus"))
#\'
"avilist_2025_short"
'

# Generate field descriptions for FULL dataset
field_descriptions_full <- avilist_metadata %>%
  filter(in_full_version) %>%
  mutate(desc_line = paste0("#\'   \\item{", field_name, "}{", description, "}")) %>%
  pull(desc_line) %>%
  paste(collapse = "\n")

# Generate field descriptions for SHORT dataset
field_descriptions_short <- avilist_metadata %>%
  filter(in_short_version) %>%
  mutate(desc_line = paste0("#\'   \\item{", field_name, "}{", description, "}")) %>%
  pull(desc_line) %>%
  paste(collapse = "\n")

# Fill in templates
complete_doc_full <- str_replace_all(data_doc_template_full,
                                     c("\\{nrow\\}" = as.character(nrow(avilist_2025)),
                                       "\\{ncol\\}" = as.character(ncol(avilist_2025)),
                                       "\\{field_descriptions\\}" = field_descriptions_full))

complete_doc_short <- str_replace_all(data_doc_template_short,
                                      c("\\{nrow\\}" = as.character(nrow(avilist_2025_short)),
                                        "\\{ncol\\}" = as.character(ncol(avilist_2025_short)),
                                        "\\{field_descriptions\\}" = field_descriptions_short))

# Write documentation files
writeLines(complete_doc_full, "R/avilist_2025.R")
writeLines(complete_doc_short, "R/avilist_2025_short.R")

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
message("- R/avilist_2025.R (full dataset documentation)")
message("- R/avilist_2025_short.R (short dataset documentation)")
message("\nReady for package development!")

# Display top families by species count
message("\nTop 10 families by species count:")
print(family_info %>%
        slice_head(n = 10) %>%
        select(Family, Family_English_name, species_count))

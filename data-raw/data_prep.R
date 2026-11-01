# =============================================================================
# avilistr Package - Data Preparation Script
# data-raw/data_prep.R
#
# This script processes the raw AviList Excel files and creates clean R data objects
# for inclusion in the avilistr package.
#
# =============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(usethis)

# Clean AviList column headers: snake_case, without source versions such as
# "v2026" (e.g. "ENGLISH NAME: CLEMENTS v2026" -> "english_name_clements")
clean_field_names <- function(x) {
  x <- str_remove_all(x, "\\bv\\d+\\b")
  x <- str_to_lower(str_replace_all(str_trim(x), "[^A-Za-z0-9]+", "_"))
  case_when(
    x == "_" ~ "sequence",  # "#" column
    x == "rank" ~ "taxon_rank",
    TRUE ~ x
  )
}

# =============================================================================
# 1. LOAD AND CLEAN RAW DATA
# =============================================================================

# Read both AviList Excel files
message("Reading AviList Excel files...")

# Full/extended version
raw_avilist_full <- read_excel(
  "data-raw/AviList-v2026-20Oct2026-extended-version.xlsx",
  sheet = "AviList v2026", # the default sheet is the first one
  col_types = "text"  # Read everything as text initially
)

# Official short version
raw_avilist_short <- read_excel(
  "data-raw/AviList-v2026-20Oct2026-short-version.xlsx",
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

avilist_2026 <- raw_avilist_full %>%
  # Clean column names (snake_case, drop source versions such as "v2026")
  rename_with(clean_field_names) %>%

  # Convert appropriate columns to numeric
  mutate(
    sequence = as.numeric(sequence),

    # Clean and standardize text fields
    scientific_name = str_trim(scientific_name),
    english_name_avilist = str_trim(english_name_avilist),
    english_name_clements = str_trim(english_name_clements),

    # Standardize taxonomic ranks
    taxon_rank = str_to_lower(str_trim(taxon_rank)),

    # Clean family and order names
    order = str_trim(order),
    family = str_trim(family),

    # Clean authority information
    authority = str_trim(authority),

    # Process URLs - ensure they're valid or NA
    birdlife_datazone_url = case_when(
      str_detect(birdlife_datazone_url, "^https?://") ~ birdlife_datazone_url,
      birdlife_datazone_url == "" ~ NA_character_,
      TRUE ~ NA_character_
    ),

    birds_of_the_world_url = case_when(
      str_detect(birds_of_the_world_url, "^https?://") ~ birds_of_the_world_url,
      birds_of_the_world_url == "" ~ NA_character_,
      TRUE ~ NA_character_
    ),

    original_description_url = case_when(
      str_detect(original_description_url, "^https?://") ~ original_description_url,
      original_description_url == "" ~ NA_character_,
      TRUE ~ NA_character_
    ),

    # Clean species codes
    species_code_cornell_lab = str_trim(species_code_cornell_lab),
    avibase_id = str_trim(avibase_id)
  ) %>%

  # Arrange by taxonomic sequence
  arrange(sequence)

# =============================================================================
# 3. PROCESS OFFICIAL SHORT VERSION
# =============================================================================

message("Processing official short version...")

avilist_2026_short <- raw_avilist_short %>%
  # Apply same cleaning as full version
  rename_with(clean_field_names) %>%

  # Convert appropriate columns to numeric
  mutate(
    sequence = as.numeric(sequence),

    # Clean and standardize text fields (adjust field names as needed)
    scientific_name = str_trim(scientific_name),
    across(contains("english_name"), ~ str_trim(.x)),

    # Standardize taxonomic ranks
    taxon_rank = str_to_lower(str_trim(taxon_rank)),

    # Clean family and order names
    across(c(order, family), ~ str_trim(.x)),

    # Clean authority information
    authority = str_trim(authority)
  ) %>%

  # Arrange by taxonomic sequence
  arrange(sequence)

# Compare field overlap between versions
common_fields <- intersect(names(avilist_2026), names(avilist_2026_short))
full_only_fields <- setdiff(names(avilist_2026), names(avilist_2026_short))
short_only_fields <- setdiff(names(avilist_2026_short), names(avilist_2026))

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

# Field descriptions from the "How to use" sheet of the extended version
message("Reading field descriptions from the 'How to use' sheet...")

field_descriptions <- read_excel(
  "data-raw/AviList-v2026-20Oct2026-extended-version.xlsx",
  sheet = "How to use",
  col_types = "text"
) %>%
  transmute(
    field_name = clean_field_names(`Column header`),
    description = str_squish(Description)
  )

# Source versions are given in the column headers (e.g. "CLEMENTS v2026",
# "BIRDLIFE v10")
source_versions <- tibble(
  field_name = clean_field_names(names(raw_avilist_full)),
  header_version = str_extract(names(raw_avilist_full), "\\bv\\d+\\b")
)
clements_version <- source_versions$header_version[source_versions$field_name == "english_name_clements"]
birdlife_version <- source_versions$header_version[source_versions$field_name == "english_name_birdlife"]

# Get all fields from both datasets
full_fields <- names(avilist_2026)
short_fields <- names(avilist_2026_short)
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
  left_join(field_descriptions, by = "field_name") %>%
  left_join(source_versions, by = "field_name") %>%
  mutate(
    in_full_version = field_name %in% full_fields,
    in_short_version = field_name %in% short_fields,

    # Determine data type
    data_type = case_when(
      field_name == "sequence" ~ "numeric",
      str_detect(tolower(field_name), "url") ~ "character (URL)",
      TRUE ~ "character"
    ),

    # Determine likely source
    source = case_when(
      str_detect(tolower(field_name), "clements") ~ "Clements",
      str_detect(tolower(field_name), "cornell") ~ "Cornell Lab",
      str_detect(tolower(field_name), "birdlife|iucn") ~ "BirdLife",
      str_detect(tolower(field_name), "avibase") ~ "Avibase",
      str_detect(tolower(field_name), "birds.*world") ~ "Cornell Lab",
      TRUE ~ "AviList"
    ),

    # Version of the source the field is aligned with: from the column header
    # when given, otherwise the version of the matching English name column
    source_version = coalesce(header_version, case_when(
      source %in% c("Clements", "Cornell Lab") ~ clements_version,
      source == "BirdLife" ~ birdlife_version,
      source == "AviList" ~ "v2026"
    ))
  ) %>%
  select(field_name, description, data_type, source, source_version,
         in_full_version, in_short_version) %>%
  arrange(field_name)

# Print summary of field coverage
message("\nField description summary:")
message(paste("Fields in full version:", sum(avilist_metadata$in_full_version)))
message(paste("Fields in short version:", sum(avilist_metadata$in_short_version)))

# Show any fields missing from the "How to use" sheet
missing_descriptions <- avilist_metadata %>%
  filter(is.na(description)) %>%
  pull(field_name)

if (length(missing_descriptions) > 0) {
  message("\nFields without a description (may need manual review):")
  print(missing_descriptions)
}

# Summary statistics
avilist_stats_2026 <- list(
  version = "2026",
  release_date = "2026-10-20",
  total_records = nrow(avilist_2026),
  species_count = sum(avilist_2026$taxon_rank == "species", na.rm = TRUE),
  subspecies_count = sum(avilist_2026$taxon_rank == "subspecies", na.rm = TRUE),
  genera_count = length(unique(str_extract(avilist_2026$scientific_name[avilist_2026$taxon_rank == "species"], "^[A-Z][a-z]+"))),
  families_count = length(unique(avilist_2026$family)),
  orders_count = length(unique(avilist_2026$order)),
  fields_count = ncol(avilist_2026),
  citation = "AviList Core Team. 2026. AviList: The Global Avian Checklist, v2026. https://doi.org/10.2173/avilist.v2026"
)

# =============================================================================
# 5. DATA VALIDATION AND QUALITY CHECKS
# =============================================================================

message("Performing data quality checks...")

# Check for duplicates
duplicates <- avilist_2026 %>%
  filter(taxon_rank == "species") %>%
  group_by(scientific_name) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicates) > 0) {
  warning(paste("Found", nrow(duplicates), "duplicate species records"))
  print(duplicates %>% select(scientific_name, order, family))
}

# Check for missing essential data
missing_sci_names <- sum(is.na(avilist_2026$scientific_name))
missing_families <- sum(is.na(avilist_2026$family))
missing_orders <- sum(is.na(avilist_2026$order))

message(paste("Missing scientific names:", missing_sci_names))
message(paste("Missing families:", missing_families))
message(paste("Missing orders:", missing_orders))

# Validate taxonomic hierarchy
invalid_ranks <- avilist_2026 %>%
  filter(!taxon_rank %in% c("species", "subspecies", "genus", "family", "order")) %>%
  distinct(taxon_rank)

if (nrow(invalid_ranks) > 0) {
  warning("Found unexpected taxonomic ranks:")
  print(invalid_ranks)
}

# =============================================================================
# 6. CREATE INTERNAL REFERENCE DATA
# =============================================================================

message("Creating internal reference objects...")

# Common name variations for fuzzy matching
name_variations <- avilist_2026 %>%
  filter(taxon_rank == "species") %>%
  select(scientific_name, english_name_avilist, english_name_clements) %>%
  pivot_longer(
    cols = c(english_name_avilist, english_name_clements),
    names_to = "source",
    values_to = "common_name",
    values_drop_na = TRUE
  ) %>%
  distinct() %>%
  arrange(scientific_name)

# Authority codes and abbreviations
authority_patterns <- avilist_2026 %>%
  filter(!is.na(authority), taxon_rank == "species") %>%
  count(authority, sort = TRUE) %>%
  slice_head(n = 100)  # Top 100 most common authorities

# Order information with statistics
order_info <- avilist_2026 %>%
  filter(taxon_rank == "species") %>%
  group_by(order) %>%
  summarise(
    species_count = n(),
    families_count = n_distinct(family),
    example_families = paste(head(unique(family), 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(species_count))

# Family information with statistics
family_info <- avilist_2026 %>%
  filter(taxon_rank == "species") %>%
  group_by(order, family) %>%
  summarise(
    species_count = n(),
    example_species = paste(head(scientific_name, 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(order, desc(species_count))

# =============================================================================
# 7. SAVE DATA OBJECTS
# =============================================================================

message("Saving data objects...")

# Ensure we're in the right directory and data/ folder exists
if (!dir.exists("data")) {
  dir.create("data")
  message("Created data/ directory")
}

# Remove any existing .rda files to start clean
existing_rda <- list.files("data/", pattern = "\\.rda$", full.names = TRUE)
if (length(existing_rda) > 0) {
  file.remove(existing_rda)
  message("Removed existing .rda files")
}

# ONLY save the three main datasets that users should access
usethis::use_data(avilist_2026, overwrite = TRUE)
usethis::use_data(avilist_2026_short, overwrite = TRUE)
usethis::use_data(avilist_metadata, overwrite = TRUE)

message("✓ Saved three main datasets")

# Verify final state
final_files <- list.files("data/", pattern = "\\.rda$")
message("Final data files:", paste(final_files, collapse = ", "))

# Should only be: avilist_2026.rda, avilist_2026_short.rda, avilist_metadata.rda

# =============================================================================
# 8. CREATE DOCUMENTATION STUBS
# =============================================================================

message("Creating documentation templates...")

# Create data documentation template for FULL dataset
data_doc_template_full <- '
#\' AviList Global Avian Checklist v2026 (Full Version)
#\'
#\' The complete AviList dataset containing all bird species, subspecies, and
#\' taxonomic information as of October 2026. This is the extended version with
#\' all available fields including nomenclatural details, bibliographic
#\' information, and external database links.
#\'
#\' @format A data frame with {nrow} rows and {ncol} columns.
#\'   See avilist_metadata for detailed field descriptions.
#\'
#\' @source AviList Core Team. 2026. AviList: The Global Avian Checklist, v2026.
#\'   \\doi{10.2173/avilist.v2026}
#\'
#\' @examples
#\' # Load the full dataset
#\' data(avilist_2026)
#\'
#\' # View summary
#\' str(avilist_2026)

"avilist_2026"
'

# Create data documentation template for SHORT dataset
data_doc_template_short <- '
#\' AviList Global Avian Checklist v2026 (Short Version)
#\'
#\' The essential fields from the AviList dataset containing core taxonomic
#\' information for all bird species and subspecies as of October 2026. This is
#\' the official short version provided by the AviList team, optimized for
#\' faster loading and basic taxonomic operations.
#\'
#\' @format A data frame with {nrow} rows and {ncol} columns.
#\'   See avilist_metadata for detailed field descriptions.
#\'
#\' @source AviList Core Team. 2026. AviList: The Global Avian Checklist, v2026.
#\'   \\doi{10.2173/avilist.v2026}
#\'
#\' @examples
#\' # Load the short dataset (faster loading)
#\' data(avilist_2026_short)
#\'
#\' # View summary
#\' str(avilist_2026_short)
#\'
"avilist_2026_short"
'

# Fill in templates (without detailed field descriptions to avoid LaTeX issues)
complete_doc_full <- str_replace_all(data_doc_template_full,
                                     c("\\{nrow\\}" = as.character(nrow(avilist_2026)),
                                       "\\{ncol\\}" = as.character(ncol(avilist_2026))))

complete_doc_short <- str_replace_all(data_doc_template_short,
                                      c("\\{nrow\\}" = as.character(nrow(avilist_2026_short)),
                                        "\\{ncol\\}" = as.character(ncol(avilist_2026_short))))

# Write documentation files
writeLines(complete_doc_full, "R/avilist_2026.R")
writeLines(complete_doc_short, "R/avilist_2026_short.R")

# Create avilist_metadata documentation
metadata_doc <- '
#\' AviList Field Metadata
#\'
#\' Metadata describing all fields in the AviList datasets, including
#\' field descriptions, data types, sources, source versions, and availability
#\' in different dataset versions.
#\'
#\' @format A tibble with metadata for all AviList fields:
#\' \\describe{
#\'   \\item{field_name}{Name of the field in the dataset}
#\'   \\item{description}{Description of the field content, from the "How to use" sheet of the AviList file}
#\'   \\item{data_type}{Data type (character, numeric, etc.)}
#\'   \\item{source}{Original source of the data (AviList, Clements, etc.)}
#\'   \\item{source_version}{Version of the source the field is aligned with (e.g. v2026 for Clements, v10 for BirdLife)}
#\'   \\item{in_full_version}{Logical, whether field is in the full dataset}
#\'   \\item{in_short_version}{Logical, whether field is in the short dataset}
#\' }
#\'
#\' @source Generated from the "How to use" sheet and column headers of the
#\'   AviList extended file
#\'
#\' @examples
#\' # View all field descriptions
#\' data(avilist_metadata)
#\'
#\' # Fields in short version only
#\' avilist_metadata[avilist_metadata$in_short_version, ]
#\'
#\' # Fields from specific sources
#\' avilist_metadata[avilist_metadata$source == "Clements", ]
#\'
"avilist_metadata"
'

writeLines(metadata_doc, "R/avilist_metadata.R")

# =============================================================================
# 9. FINAL SUMMARY
# =============================================================================

message("\n", strrep("=", 60))
message("DATA PREPARATION COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(paste("Total records processed:", nrow(avilist_2026)))
message(paste("Species count:", avilist_stats_2026$species_count))
message(paste("Subspecies count:", avilist_stats_2026$subspecies_count))
message(paste("Families count:", avilist_stats_2026$families_count))
message(paste("Orders count:", avilist_stats_2026$orders_count))
message(paste("Fields in full dataset:", ncol(avilist_2026)))
message(paste("Fields in short dataset:", ncol(avilist_2026_short)))
message("\nFiles created:")
message("- data/avilist_2026.rda")
message("- data/avilist_2026_short.rda")
message("- data/avilist_metadata.rda")
message("- R/avilist_2026.R (full dataset documentation)")
message("- R/avilist_2026_short.R (short dataset documentation)")
message("- R/avilist_metadata.R (metadata documentation)")
message("\nReady for package development!")

# Display top families by species count (calculated on the fly)
message("\nTop 10 families by species count:")
print(avilist_2026 %>%
        filter(taxon_rank == "species") %>%
        group_by(family) %>%
        summarise(species_count = n(), .groups = "drop") %>%
        arrange(desc(species_count)) %>%
        slice_head(n = 10) %>%
        select(family, species_count))

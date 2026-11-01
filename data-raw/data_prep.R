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

# =============================================================================
# 0. AVILIST VERSION - update these for a new AviList release
# =============================================================================

avilist_version <- "2026"
avilist_release_date <- as.Date("2026-10-20")
file_full <- "data-raw/AviList-v2026-20Oct2026-extended-version.xlsx"
file_short <- "data-raw/AviList-v2026-20Oct2026-short-version.xlsx"

# Names of the exported datasets, e.g. avilist_2026 and avilist_2026_short
name_full <- paste0("avilist_", avilist_version)
name_short <- paste0("avilist_", avilist_version, "_short")

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
  file_full,
  sheet = 1, # the checklist is the first sheet; its name changes each release
  col_types = "text"  # Read everything as text initially
)

# Official short version
raw_avilist_short <- read_excel(
  file_short,
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

avilist <- raw_avilist_full %>%
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

avilist_short <- raw_avilist_short %>%
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
common_fields <- intersect(names(avilist), names(avilist_short))
full_only_fields <- setdiff(names(avilist), names(avilist_short))
short_only_fields <- setdiff(names(avilist_short), names(avilist))

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
  file_full,
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
full_fields <- names(avilist)
short_fields <- names(avilist_short)
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
      source == "AviList" ~ paste0("v", avilist_version)
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
avilist_stats <- list(
  version = avilist_version,
  release_date = as.character(avilist_release_date),
  total_records = nrow(avilist),
  species_count = sum(avilist$taxon_rank == "species", na.rm = TRUE),
  subspecies_count = sum(avilist$taxon_rank == "subspecies", na.rm = TRUE),
  genera_count = length(unique(str_extract(avilist$scientific_name[avilist$taxon_rank == "species"], "^[A-Z][a-z]+"))),
  families_count = length(unique(avilist$family)),
  orders_count = length(unique(avilist$order)),
  fields_count = ncol(avilist),
  citation = names(read_excel(file_full, sheet = "Recommended citation", n_max = 0))
)

# =============================================================================
# 5. DATA VALIDATION AND QUALITY CHECKS
# =============================================================================

message("Performing data quality checks...")

# Check for duplicates
duplicates <- avilist %>%
  filter(taxon_rank == "species") %>%
  group_by(scientific_name) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicates) > 0) {
  warning(paste("Found", nrow(duplicates), "duplicate species records"))
  print(duplicates %>% select(scientific_name, order, family))
}

# Check for missing essential data
missing_sci_names <- sum(is.na(avilist$scientific_name))
missing_families <- sum(is.na(avilist$family))
missing_orders <- sum(is.na(avilist$order))

message(paste("Missing scientific names:", missing_sci_names))
message(paste("Missing families:", missing_families))
message(paste("Missing orders:", missing_orders))

# Validate taxonomic hierarchy
invalid_ranks <- avilist %>%
  filter(!taxon_rank %in% c("species", "subspecies", "genus", "family", "order")) %>%
  distinct(taxon_rank)

if (nrow(invalid_ranks) > 0) {
  warning("Found unexpected taxonomic ranks:")
  print(invalid_ranks)
}

# =============================================================================
# 6. SAVE DATA OBJECTS
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

# ONLY save the three main datasets that users should access, under their
# versioned names (e.g. avilist_2026)
assign(name_full, avilist)
assign(name_short, avilist_short)
do.call(usethis::use_data, list(as.name(name_full), overwrite = TRUE))
do.call(usethis::use_data, list(as.name(name_short), overwrite = TRUE))
usethis::use_data(avilist_metadata, overwrite = TRUE)

message("✓ Saved three main datasets")

# Verify final state
final_files <- list.files("data/", pattern = "\\.rda$")
message("Final data files:", paste(final_files, collapse = ", "))

# Should only be: avilist_<version>.rda, avilist_<version>_short.rda,
# avilist_metadata.rda

# =============================================================================
# 7. CREATE DOCUMENTATION STUBS
# =============================================================================

message("Creating documentation templates...")

# Create data documentation template for FULL dataset
data_doc_template_full <- '
#\' AviList Global Avian Checklist v{version} (Full Version)
#\'
#\' The complete AviList dataset containing all bird species, subspecies, and
#\' taxonomic information as of {release}. This is the extended version with
#\' all available fields including nomenclatural details, bibliographic
#\' information, and external database links.
#\'
#\' @format A data frame with {nrow} rows and {ncol} columns.
#\'   See avilist_metadata for detailed field descriptions.
#\'
#\' @source AviList Core Team. {version}. AviList: The Global Avian Checklist, v{version}.
#\'   \\doi{10.2173/avilist.v{version}}
#\'
#\' @examples
#\' # Load the full dataset
#\' data({name})
#\'
#\' # View summary
#\' str({name})

"{name}"
'

# Create data documentation template for SHORT dataset
data_doc_template_short <- '
#\' AviList Global Avian Checklist v{version} (Short Version)
#\'
#\' The essential fields from the AviList dataset containing core taxonomic
#\' information for all bird species and subspecies as of {release}. This is
#\' the official short version provided by the AviList team, optimized for
#\' faster loading and basic taxonomic operations.
#\'
#\' @format A data frame with {nrow} rows and {ncol} columns.
#\'   See avilist_metadata for detailed field descriptions.
#\'
#\' @source AviList Core Team. {version}. AviList: The Global Avian Checklist, v{version}.
#\'   \\doi{10.2173/avilist.v{version}}
#\'
#\' @examples
#\' # Load the short dataset (faster loading)
#\' data({name})
#\'
#\' # View summary
#\' str({name})
#\'
"{name}"
'

# Fill in templates (without detailed field descriptions to avoid LaTeX issues)
fill_doc_template <- function(template, data, name) {
  str_replace_all(template, c(
    "\\{name\\}" = name,
    "\\{version\\}" = avilist_version,
    "\\{release\\}" = paste(month.name[as.integer(format(avilist_release_date, "%m"))],
                            format(avilist_release_date, "%Y")),
    "\\{nrow\\}" = as.character(nrow(data)),
    "\\{ncol\\}" = as.character(ncol(data))
  ))
}

complete_doc_full <- fill_doc_template(data_doc_template_full, avilist, name_full)
complete_doc_short <- fill_doc_template(data_doc_template_short, avilist_short, name_short)

# Remove documentation files of previous versions (e.g. R/avilist_2025.R)
old_docs <- list.files("R", pattern = "^avilist_[0-9]{4}(_short)?\\.R$", full.names = TRUE)
file.remove(old_docs)

# Write documentation files
writeLines(complete_doc_full, file.path("R", paste0(name_full, ".R")))
writeLines(complete_doc_short, file.path("R", paste0(name_short, ".R")))

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
# 8. FINAL SUMMARY
# =============================================================================

message("\n", strrep("=", 60))
message("DATA PREPARATION COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(paste("Total records processed:", nrow(avilist)))
message(paste("Species count:", avilist_stats$species_count))
message(paste("Subspecies count:", avilist_stats$subspecies_count))
message(paste("Families count:", avilist_stats$families_count))
message(paste("Orders count:", avilist_stats$orders_count))
message(paste("Fields in full dataset:", ncol(avilist)))
message(paste("Fields in short dataset:", ncol(avilist_short)))
message("\nFiles created:")
message("- data/", name_full, ".rda")
message("- data/", name_short, ".rda")
message("- data/avilist_metadata.rda")
message("- R/", name_full, ".R (full dataset documentation)")
message("- R/", name_short, ".R (short dataset documentation)")
message("- R/avilist_metadata.R (metadata documentation)")
message("\nReady for package development!")

# Display top families by species count (calculated on the fly)
message("\nTop 10 families by species count:")
print(avilist %>%
        filter(taxon_rank == "species") %>%
        group_by(family) %>%
        summarise(species_count = n(), .groups = "drop") %>%
        arrange(desc(species_count)) %>%
        slice_head(n = 10) %>%
        select(family, species_count))

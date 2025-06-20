
<!-- README.md is generated from README.Rmd. Please edit that file -->

# avilistr <img src="man/figures/logo.png" align="right" height="200" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/avilistr)](https://www.r-pkg.org/badges/version/avilistr)
[![License:
CC0-1.0](https://img.shields.io/badge/License-CC0%201.0-lightgrey.svg)](https://creativecommons.org/public-domain/cc0/)
<!-- badges: end -->

> **Access the AviList Global Avian Checklist in R**

`avilistr` provides easy access to the [AviList Global Avian
Checklist](https://www.avilist.org/), the first unified global bird
taxonomy that harmonizes previous differences between IOC, Clements, and
BirdLife checklists. This package contains the complete AviList dataset
as R data objects, ready for analysis and research.

## 🌍 About AviList

[AviList](https://www.avilist.org/) represents a landmark achievement in
ornithology - the first unified global checklist of Earth’s bird
species. Released in June 2025, it contains:

- **11,131 species** across 2,376 genera
- **19,879 subspecies**
- **252 families** in 46 orders
- **Unified taxonomy** reconciling IOC, Clements, and BirdLife
  differences
- **Rich metadata** including nomenclatural details and external
  database links

## 🦅 Installation

Install from CRAN:

``` r
install.packages("avilistr")
```

Or install the development version from GitHub:

``` r
# Install from GitHub
devtools::install_github("dalyanalytics/avilistr")

# Or using pak
pak::pak("dalyanalytics/avilistr")
```

## 🦆 Quick Start

``` r
library(avilistr)

# Load the complete AviList dataset
data(avilist_2025)

# Or load the short version with essential fields only
data(avilist_2025_short)

# Get basic information about the data
nrow(avilist_2025)  # Total records
sum(avilist_2025$Taxon_rank == "species")  # Number of species
```

**📖 For detailed examples and tutorials, see the [Getting Started
vignette](https://dalyanalytics.github.io/avilistr/articles/getting-started.html).**

## 🦜 Data Overview

The package provides three main datasets:

### **Main Datasets**

| Dataset | Records | Fields | Description |
|----|----|----|----|
| `avilist_2025` | 33,685 | 26 | Complete dataset with all available fields |
| `avilist_2025_short` | 33,685 | ~12 | Essential taxonomic fields only |
| `avilist_metadata` | 26 | 6 | Field descriptions and metadata |

### Key Fields

- `Scientific_name`: Binomial scientific name
- `English_name_AviList`: Official AviList common name  
- `English_name_Clements_v2024`: Clements common name
- `Order`, `Family`: Taxonomic classification
- `Authority`: Original description author and year
- `Taxon_rank`: Species, subspecies, etc.
- `AvibaseID`: Link to Avibase database
- Plus 19 additional fields in full version

## 🎯 Use Cases

### 🔬 **Research Applications**

- **Taxonomic studies**: Unified species concepts across databases
- **Biodiversity analysis**: Consistent species counts and
  classifications  
- **Conservation planning**: Link with IUCN Red List via BirdLife
  integration
- **Phylogenetic studies**: Standardized taxonomic framework

### 🐦 **Birding & Citizen Science**

- **List standardization**: Convert between different checklist
  authorities
- **Species validation**: Check spelling and current accepted names
- **Range verification**: Cross-reference with multiple databases
- **Trip planning**: Filter by geographic regions

### 📊 **Data Management**

- **Database cleaning**: Standardize heterogeneous species lists
- **API integration**: Connect with eBird, BirdLife, and other services
- **Reporting**: Generate taxonomically consistent summaries
- **Quality control**: Validate species names in datasets

## 🕊️ Citation

If you use `avilistr` in your research, please cite both the package and
the underlying AviList data:

**Package Citation:**

``` r
citation("avilistr")
```

**AviList Citation:**

    AviList Core Team. 2025. AviList: The Global Avian Checklist, v2025. https://doi.org/10.2173/avilist.v2025

## 🦅 Future Development

This package currently provides the core AviList datasets. Future
versions *may* include:

- **Search and filtering functions** for easier data exploration
- **Taxonomic reconciliation tools** for converting between different
  checklist authorities  
- **Integration helpers** for connecting with other ornithological
  databases
- **Validation functions** for species name checking
- **Data update utilities** for annual AviList releases

## 📄 License

This package is licensed under the CC0 License (public domain). The
AviList data is licensed under [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/).

## 🙏 Acknowledgments

- **AviList Core Team** for creating the unified global bird checklist
- **International Ornithologists’ Union** for coordinating the Working
  Group on Avian Checklists  
- **BirdLife International**, **Cornell Lab of Ornithology**, and other
  partners
- The R community for excellent tools and inspiration

## 📞 Getting Help

- 🐛 **Bug reports**: [GitHub
  Issues](https://github.com/dalyanalytics/avilistr/issues)
- 💡 **Feature requests**: [GitHub
  Discussions](https://github.com/dalyanalytics/avilistr/discussions)  
- 📚 **Documentation**: [Package
  website](https://dalyanalytics.github.io/avilistr/)

------------------------------------------------------------------------

*Built with ❤️ for the global ornithology community*

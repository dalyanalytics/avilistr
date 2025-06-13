
<!-- README.md is generated from README.Rmd. Please edit that file -->

# avilistr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/avilistr)](https://CRAN.R-project.org/package=avilistr)
[![License:
CC0-1.0](https://img.shields.io/badge/License-CC0%201.0-lightgrey.svg)](http://creativecommons.org/publicdomain/cc0/)
<!-- badges: end -->

> **Access and work with the AviList Global Avian Checklist in R**

`avilistr` provides easy access to the [AviList Global Avian
Checklist](https://www.avilist.org/), the first unified global bird
taxonomy that harmonizes previous differences between IOC, Clements, and
BirdLife checklists. This package includes tools for taxonomic
reconciliation, species lookup, and integration with other
ornithological databases.

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

## Installation

To install the package type the following:

``` r
install.packages("avilistr")
library("avilistr")
```

Or you can install the development version of `avilistr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dalyanalytics/avilistr")
```

## ⚡ Quick Start

``` r
library(avilistr)

# Load the full AviList dataset
data(avilist_2025)

data(avilist_2025_short)

data(avilist_metadata)
```

## 📚 Data Structure

### Full vs Short Versions

| Version   | Records | Fields | Use Case                                  |
|-----------|---------|--------|-------------------------------------------|
| **Full**  | 33,685  | 26     | Complete analyses, nomenclatural research |
| **Short** | 33,685  | ~10    | Basic taxonomic work, faster loading      |

### Key Fields

- `Scientific_name`: Binomial scientific name
- `English_name_AviList`: Official AviList common name  
- `English_name_Clements_v2024`: Clements common name
- `Order`, `Family`: Taxonomic classification
- `Authority`: Original description author and year
- `Taxon_rank`: Species, subspecies, etc.
- `AvibaseID`: Link to Avibase database
- Plus 19 additional fields in full version

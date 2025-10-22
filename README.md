
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kmetadata

<!-- badges: start -->
<!-- badges: end -->

The **kmetadata** package provides tools for creating and managing
metadata for R datasets. It simplifies documenting variable names,
labels, and value definitions through an Excel-based workflow.

## Installation

Install the development version from GitLab:

``` r
# install.packages("remotes")
remotes::install_gitlab("kbosirany/kmetadata")
```

## Quick Start

``` r
library(kmetadata)

# Sample data
data <- data.frame(
  id = 1:3,
  gender = c("M", "F", "M"),
  status = c("Active", "Inactive", "Active")
)

# Create variable metadata
var_meta <- create_metadata(data, path = "data.csv", which = "vars")
var_meta
#> [[1]]
#>    label new_label description
#> 1     id        NA          NA
#> 2 gender        NA          NA
#> 3 status        NA          NA

# Create value metadata for categorical variables
val_meta <- create_metadata(data, path = "data.csv", which = "values",
                            vars = c("gender", "status"))
val_meta$gender
#> # A tibble: 2 × 3
#>   label new_label description
#>   <chr> <lgl>     <lgl>      
#> 1 F     NA        NA         
#> 2 M     NA        NA
```

## Key Features

- **Create metadata templates** for variables and values
- **Export to Excel** for manual editing and documentation
- **Smart caching** with `load_metadata()` - loads existing or creates
  new
- **Multi-sheet support** for Excel workbooks
- **Simple workflow** - create, edit, load

## Typical Workflow

1.  **Create metadata template**:

``` r
create_metadata("data/survey.csv", which = "vars",
                write_results = TRUE)
```

2.  **Edit in Excel** - add labels and descriptions

3.  **Load edited metadata**:

``` r
metadata <- load_metadata(path = "data/survey.csv",
                          which = "vars",
                          vars = colnames(data))
```

## Documentation

- **Vignette**: `vignette("kmetadata-intro")` - Comprehensive guide with
  examples
- **Functions**: `?create_metadata`, `?load_metadata`
- **File naming**: Outputs follow pattern
  `[filename] - [type] - metadata.xlsx`

## Learn More

See the [package vignette](vignettes/kmetadata-intro.Rmd) for detailed
examples including:

- Variable and value metadata creation
- Working with multi-sheet Excel files
- Custom file paths and options
- Complete workflow examples
- Tips and best practices

## License

AGPL (\>= 3)

## Author

Kevin Bosirany Orlando  
Email: <kevinbosirany@gmail.com>  
ORCID: [0009-0009-2784-3108](https://orcid.org/0009-0009-2784-3108)

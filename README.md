# mtpR

Tools for designing and reading **m**icro**t**iter **p**late layouts in R.

`mtpR` helps you:

- convert between well labels (`"A01"`) and numeric indices, row- or column-wise
  (`convert_well()`, `to_rows_columns()`)
- assign reactions and replicates to well positions with configurable spacing and
  priority (`replicate_reactions()`), spread them across multiple plates
  (`add_plates()`), and duplicate whole plates (`add_plate_replicates()`)
- draw plate maps with `ggplot2` (`plot_plate()`)
- reshape plate-shaped tables into tidy long format (`readPlate()`)

Supported plate sizes: 6, 12, 24, 48, 96, 384, 1536.

## Installation

```r
# from a local clone
devtools::install("path/to/mtpR")

# or directly from GitHub
devtools::install_github("ejpmohr/mtpR")
```

## Example: design a plate

```r
library(mtpR)

design <- data.frame(rxn = 1:8) |>
  replicate_reactions(
    "rxn",
    num_replicates = 3, priority = "Reaction",
    inter_spacing = 2, intra_spacing = 1,
    start_position = convert_well("A01", "to_num", plate = 96)
  ) |>
  add_plates("position", plate_size = 96) |>
  dplyr::mutate(well = convert_well(Well.Index, "to_well", plate = 96))

plot_plate(design, fill = "rxn", well_id = "well", plate = 96)
```

## Example: read plate-shaped data

```r
sample_plate <- readRDS(
  system.file("extdata", "SamplePlateShapedData.rds", package = "mtpR")
)

readPlate(sample_plate, plate.size = 96)
#> # A tibble: 96 x 2
#>   well  value
#>   <chr> <dbl>
#> 1 A1    0.172
#> ...
```

## Experimental Shiny app

`design_plate()` launches a graphical front-end for the functions above. It is
experimental; the programmatic functions are the supported interface.

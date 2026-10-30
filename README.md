# mtpR

Tools for designing and reading **m**icro**t**iter **p**late layouts in R.

`mtpR` helps you:

- standardise well labels from other sources (`clean_wells()`), convert between
  well labels (`"A01"`) and numeric indices, row- or column-wise (`convert_well()`,
  `to_rows_columns()`), and find a well's quadrant (`well_quadrant()`)
- reserve wells for controls (`place_controls()`) and check that a layout fits
  before you build it (`plate_capacity()`)
- assign reactions and replicates to well positions in grouped, dealt, scattered
  or quadrant patterns with configurable spacing (`replicate_reactions()`), spread
  them across multiple plates (`add_plates()`), and duplicate whole plates
  (`add_plate_replicates()`)
- draw plate maps with `ggplot2` (`plot_plate()`), mark which wells hold reactions
  (`plate_roles()`), and draw and count picklist transfers between plates
  (`plot_plate_pair()`, `picklist_summary()`)
- paint wells by hand in Shiny or an offline HTML file, with several features per
  well, and read the result back into R (`plate_picker()`, `read_plate_picker()`)
- reshape plate-shaped tables into tidy long format (`read_plate()`)

Supported plate sizes: 6, 12, 24, 48, 96, 384, 1536.

## Installation

```r
# from a local clone
devtools::install("path/to/mtpR")

# or directly from GitHub
devtools::install_github("EM-XS/mtpR")
```

## Example: design a plate

```r
library(mtpR)

design <- data.frame(rxn = 1:8) |>
  replicate_reactions(
    "rxn",
    num_replicates = 3, pattern = "grouped",
    inter_spacing = 2, intra_spacing = 1,
    start_position = "A01", plate = 96
  ) |>
  add_plates("position", plate_size = 96) |>
  dplyr::mutate(well = convert_well(Well.Index, "to_well", plate = 96))

plot_plate(design, fill = "rxn", well_id = "well", plate = 96)
```

`pattern` sets the layout. For three reactions in triplicate, `"grouped"` gives
`111 222 333` and `"dealt"` gives `123 123 123`. `"scattered"` randomises the
order (reproducibly, with `seed`), and `"quadrants"` puts replicate `k` of each
reaction in quadrant `k` of a 2x2 square.

## Example: plan around controls

```r
controls <- place_controls(96, "edge", name = "buffer")

# Stops with an explanation if the layout does not fit
plate_capacity(8, replicates = 3, plate = 96, controls = controls)

design <- data.frame(rxn = 1:8) |>
  replicate_reactions("rxn", 3, pattern = "grouped", reserved = controls) |>
  dplyr::mutate(well = convert_well(position, "to_well", plate = 96))

plate_roles(controls, design, well_id = "well") |>
  plot_plate(fill = "Role", well_id = "Well", plate = 96)
```

Other placements include `"lines"` (whole rows or columns), `"spread"` (evenly
among the reactions), `"random"` and `"quadrant"`; see `?place_controls`.

## Example: read plate-shaped data

```r
sample_plate <- readRDS(
  system.file("extdata", "SamplePlateShapedData.rds", package = "mtpR")
)

read_plate(sample_plate, plate_size = 96)
#> # A tibble: 96 x 2
#>   well  value
#>   <chr> <dbl>
#> 1 A01   0.172
#> ...
```

## Example: tidy well labels

```r
clean_wells(c("a1", "A01", "B-12"), plate = 96)
#> [1] "A01" "A01" "B12"

clean_wells(c("R1C1", "R2C12"), plate = 96, mode = "guess")
#> Interpreted 2 label(s) using format "R1C1": R1C1 -> A01, ...
```

`mode = "strict"` accepts only `"A01"`-style labels, `"common"` (the default) also
accepts other case, padding and separators, and `"guess"` also tries column-first
labels, row and column numbers, and wells inside other text. Every mode stops on
labels it cannot read or wells that are not on the plate.

## Example: paint wells

```r
p <- plate_picker(96, labels = c("Blank", "Standard"))
p                                    # click or drag across wells to paint them
plate_picker_html(p, "plate.html")   # one offline file; Save page keeps the painting
read_plate_picker("plate.html")      # one row per well
```

In Shiny, use `plate_pickerOutput()` and `renderPlate_picker()`, read the result
with `plate_picker_value(input$id)`, and change it with `update_plate_picker()`.

`plate_picker()` is the portable drawing side of mtpR, so it uses the same names.
A well can carry several features at once, with one label per feature:

```r
layers <- data.frame(
  label   = c("HeLa", "HEK293", "Drug A", "Drug B", "100", "50"),
  feature = rep(c("cell_line", "treatment", "conc_uM"), each = 2)
)

p <- plate_picker(96, labels = layers, locked = place_controls(96, "edge"))
plate_picker_html(p, "plate.html", title = "Run 12")
```

In the page, painting a label changes only that feature in those wells, so you can
paint one layer at a time. You can also:

- add and edit features, labels and attributes
- switch on *In sequence* for a feature, so one click lays out all its labels from
  that well, using `pattern`, `num_replicates`, `inter_spacing`, `intra_spacing`
  and `fill_wise` as in `replicate_reactions()`
- change the plate size, or load a CSV with one row per well
- colour the plate with `fill` by any feature or attribute
- export a PNG, an SVG or a CSV, or use Save page to keep everything in one file

The result has one row per well, and reads straight back into R:

```r
d <- read_plate_picker("plate.html")   # well, cell_line, treatment, conc_uM
plot_plate(d, fill = "treatment", well_id = "well", plate = 96)
plate_picker(96, value = d)            # carry on painting
```

## Example: draw a picklist

```r
picklist <- data.frame(
  `Source Plate Name` = "Src1",
  `Source Well` = c("A1", "A1", "B1"),
  `Destination Well` = c("A1", "A2", "B1"),
  `Transfer Volume` = c(25, 50, 75),
  check.names = FALSE
)

picklist_summary(picklist)$total_volume
plot_plate_pair(picklist, source_plate = 384, dest_plate = 96)
```

Pass `interactive = TRUE` to highlight linked wells on hover (needs `ggiraph`).

## Experimental Shiny app

`design_plate()` launches a graphical front-end that lays out a plate from rules
such as the replicate count and spacing, using the functions above. It is
experimental; the programmatic functions are the supported interface. To place
wells by hand instead, use `plate_picker()`.

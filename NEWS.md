# mtpR 0.4.4

## plate_picker(): several features per well

* A well can now carry several characteristics at once. Each is a *feature*, its
  values are its labels, and a well holds one label per feature: `labels` takes a
  `feature` column in place of `group`. Painting replaces only that feature's value
  in those wells, and the eraser works on the active feature.
* The result has one row per well and one column per feature, then any attributes,
  so it goes straight into `plot_plate()`. This is the shape of
  `plate_picker_value()`, `read_plate_picker()`, the CSV export and the `value`
  argument. A plain `plate_picker(96, c("Blank", "Standard"))` still gives `well`
  and `label`.
* `fill` picks a feature or an attribute. A painted well with no value for it uses
  the NA colour, as `plot_plate()` would draw it.
* Load CSV reads a file with a well column as one row per well, with each other
  column as a feature.
* Sequence painting fills one feature and leaves the others alone.
* `multiple` is removed. Use features for wells that carry several things.

# mtpR 0.4.3

## plate_picker() is now a portable plate editor

* `labels` can be a data frame with one row per label, like the `rxn_df` of
  `replicate_reactions()`: a `label` column, optional `group` and `colour`, and
  any other columns as attributes of that label. A `value` data frame with these
  columns also works on its own.
* With `editable = TRUE` (the default) labels, groups and attributes can be
  added, changed and deleted in the browser, and Undo covers them.
* A group can be painted *in sequence*: one click lays out all its labels from
  that well, as `replicate_reactions()` does. It uses the same `pattern`
  (`"grouped"` or `"dealt"`), `num_replicates`, `inter_spacing`, `intra_spacing`,
  `start_position` and `fill_wise`, and locked wells act as `reserved`. The panel
  shows the matching R call.
* The plate size can be changed in the browser, keeping positions, filling in
  order, or with each well becoming a 2x2 square (`quadrants`). In Shiny it is
  reported as `input$<id>_plate`.
* Load CSV reads a finished layout or a list of labels, matches column names used
  across mtpR, and reads wells by the `"common"` rule of `clean_wells()`.
* `fill` colours the plate by `"label"`, `"group"` or an attribute, using the
  continuous scale and NA colour of `plot_plate()` for numbers.
* Export writes a PNG, an SVG or a CSV. The CSV has `well`, `label`, `group` and
  each attribute, and Save page keeps the plate, labels and groups too.
* `plate_picker_value()` and `read_plate_picker()` return `group` and attribute
  columns when the labels have them.
* `locked` accepts a plate from `place_controls()`.
* `update_plate_picker()` gains `plate`, `labels` and `fill`.

## Behaviour changes

* More than eight labels no longer need `colours`: the palette continues in
  lighter rounds, and labels in a group share a hue. Labels with no group keep
  their colours.
* The label chips in the toolbar are replaced by a side panel, and Copy rows and
  Download CSV moved into the Export panel.

# mtpR 0.4.2

## New features

* New `clean_wells()` rewrites well labels in the standard form (`"A01"`). Its
  `mode` is `"strict"` (only the standard form), `"common"` (the default: also
  other case, padding, surrounding whitespace and one separator) or `"guess"`
  (also `"1A"`, `"R1C1"`, `"1,1"` and wells inside text such as `"Plate1_A01"`).
  `"guess"` reports what it changed and stops when formats disagree.

## Behaviour changes

* `read_plate()` returns `"A01"` instead of `"A1"`, so its wells match those of
  `convert_well()`. Code that compares its wells with `"A1"` needs updating.
* All functions now read well labels by the `"common"` rule. `convert_well()`,
  `well_quadrant()` and others accept `"A 1"`, `"A-5"`, `"A_5"` and `"A:5"`, which
  0.4.1 rejected. `"A1B2"` is still an error.
* `plot_plate_pair()` shows and identifies wells as `"A01"` in tooltips and in
  interactive ids.

## Bug fixes

* `plot_plate()` warns when one well is labelled in more than one way, such as
  `"A1"` and `"A01"`, which used to draw one tile over the other without saying so.

# mtpR 0.4.1

## Bug fixes

* `convert_well()` rejects malformed labels such as `"A1B2"`, `"A-5"` and
  `"A 1"`. They used to be read as a real well (`"A1B2"` as `"A02"`). Functions
  that take well labels through it, such as `well_quadrant()`, get the same
  check; `place_controls()` removes spaces first, so it still accepts `"A 1"`.
  Non-whole or `NA` indices now give a clear error.
* `read_plate()` stops when two cells map to one well. Column names with an
  earlier number (`"Plate1_col3"`) and repeated row labels used to return
  duplicate wells without any message. `value_name = "well"` is now an error.
* `replicate_reactions()` stops on `NA` reaction identifiers instead of dropping
  those reactions.
* `plot_plate()` warns about wells that are not on the plate instead of leaving
  them out without saying so.
* `plate_picker()` no longer leaves the previous widget's keyboard handler behind
  after a re-render, where Undo could report the old painting to `input$<id>`.
* `to_rows_columns()` gives a clear error for a missing column.
* The help pages now render their Markdown (links, lists and code).
* The `URL` and `BugReports` fields and the install instructions pointed to a
  repository that does not exist; they now point to `EM-XS/mtpR`.

## Deprecations and naming

* New `read_plate()`, with a `plate_size` argument. `readPlate()` still works but
  is deprecated, and will be removed in 0.5.0.
* `place_controls(method = "every")` will be removed in 0.5.0.
* `replicate_reactions(priority = )` is documented as superseded by `pattern`. It
  keeps working, gives no warning and has no removal date.

# mtpR 0.4.0

* New `plate_picker()`, an 'htmlwidgets' plate map you click or drag across to
  paint wells with labels. Drag to paint (or erase, when the drag starts on a
  painted well), click a row or column header to paint the line, Shift+drag for
  a rectangle, Undo/Redo, keyboard access and light/dark themes. Works for plates
  of 6 to 1536 wells.
* In Shiny, use `plate_pickerOutput()` / `renderPlate_picker()`; the painting
  arrives as `input$<id>` (turn it into a data frame with `plate_picker_value()`)
  and `update_plate_picker()` changes it without re-rendering.
* `plate_picker_html()` writes one self-contained HTML file without pandoc;
  `read_plate_picker()` reads a CSV or a saved page back.
* 'htmlwidgets', 'htmltools' and 'jsonlite' are now imported.

# mtpR 0.3.1

* `plot_plate()` gains a `labeller` argument for the facet strips. The default,
  `NULL`, keeps the strips as before (`column: value` for a rows-and-columns grid,
  the bare value for one facet).

# mtpR 0.3.0

* `place_controls()` has new methods. `"lines"` reserves whole columns or rows
  (`lines = c(1, 24)`, or row letters with `by = "row"`); `"spread"` spreads `n`
  control wells evenly among the reactions (`among`, `start`); `"quadrant"`
  reserves whole quadrants. `"fixed"` accepts rectangular ranges such as
  `"A01:H01"`, and `"edge"` gains `depth`. A count is always a number of wells.
* `place_controls()` warns when a later call takes wells that an earlier call
  already reserved (the later call still wins).
* `place_controls(method = "every")` still works but is deprecated: use `"lines"`
  for whole columns or `"spread"` for `n` wells spread out.
* `replicate_reactions(pattern = "quadrants")` tiles the plate into 2x2 squares;
  reaction `i` takes square `i` and replicate `k` goes in position `k` of the
  square. More than 4 replicates take further squares; squares containing a
  reserved well are skipped. The output gains a `Quadrant` column.
* New `well_quadrant()` returns the quadrant (1-4) of any well.
* `replicate_reactions(controls_spread = n)` interleaves `n` controls among the
  reactions in one call and returns the plate map as `attr(, "plate_map")`.
* `plate_capacity(pattern = "quadrants")` counts squares, including squares
  skipped because they hold controls.
* The `replicate_reactions()` example no longer fails R CMD check.

# mtpR 0.2.0

* New `plot_plate_pair()` draws a source plate and a destination plate side by
  side with transfer links, from an Echo-style picklist. Wells are shaded by
  volume drawn / received (or transfer count). With `interactive = TRUE`,
  hovering a well highlights its partner wells and links, with tooltips.
  Static plots draw curved or straight links with alpha scaled to the number of
  transfers, and none by default above 500 transfers.
* `plot_plate_pair()` draws every plate of a multi-plate picklist: sources are
  stacked on the left, destinations on the right, each sized from its plate type
  (e.g. `6RES`, `384PP`) or from `source_plate` / `dest_plate` (a number or a
  named vector). Hover ids include the plate name.
* New `picklist_summary()` counts transfers, source and destination wells, total
  volume, and volume by source and destination plate. `plot_plate_pair()` attaches it as
  `attr(, "summary")`.
* `plot_plate()` gains `tooltip` (a column to use as tooltip text when
  interactive) and `interactive = "ggplot"`, which returns the interactive
  ggplot before it is wrapped by `ggiraph::girafe()`. Existing calls are
  unchanged.

# mtpR 0.1.0

* Plate maps drawn as tiles, with labels, highlights, continuous fill and
  ggiraph output; `place_controls()`, `plate_capacity()` and replicate
  patterns (see earlier release notes in the git history).

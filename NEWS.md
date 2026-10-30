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

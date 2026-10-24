# mtpR 0.3.1

* `plot_plate()` gains a `labeller` argument for the facet strips. The default
  (`ggplot2::label_both`) is used for every facet layout, so a plate map facetted
  on one variable now also prints `column: value` strips; pass
  `labeller = ggplot2::label_value` for the bare value.

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

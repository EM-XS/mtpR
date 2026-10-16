# mtpR 0.2.0

* New `plot_plate_pair()` draws a source plate and a destination plate side by
  side with transfer links, from an Echo-style picklist. Wells are shaded by
  volume drawn / received (or transfer count). With `interactive = TRUE`,
  hovering a well highlights its partner wells and links, with tooltips.
  Static plots draw curved or straight links with alpha scaled to the number of
  transfers, and none by default above 500 transfers.
* New `picklist_summary()` counts transfers, source and destination wells, total
  volume and volume by source plate. `plot_plate_pair()` attaches it as
  `attr(, "summary")`.
* `plot_plate()` gains `tooltip` (a column to use as tooltip text when
  interactive) and `interactive = "ggplot"`, which returns the interactive
  ggplot before it is wrapped by `ggiraph::girafe()`. Existing calls are
  unchanged.

# mtpR 0.1.0

* Plate maps drawn as tiles, with labels, highlights, continuous fill and
  ggiraph output; `place_controls()`, `plate_capacity()` and replicate
  patterns (see earlier release notes in the git history).

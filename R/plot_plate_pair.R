#' Summarise a picklist
#'
#' Counts what a transfer list (for example a Labcyte Echo picklist) moves.
#'
#' @param transfers A data frame with one row per transfer.
#' @param source_well,dest_well Names of the columns holding the source and
#'   destination wells.
#' @param volume Name of the transfer volume column, or `NULL` for none. The
#'   default `"Transfer Volume"` is dropped quietly when the column is absent.
#' @param source_plate_col,dest_plate_col Names of the source and destination
#'   plate name columns. If a column is absent all rows count as one plate.
#'
#' @return A list with
#'   * `transfers`: the number of transfers (rows);
#'   * `source_wells`, `dest_wells`: distinct wells used (per plate name);
#'   * `total_volume`: the summed volume (`NA` without a volume column);
#'   * `by_source_plate`: a data frame with `source_plate`, `transfers`,
#'     `source_wells` and `volume` (drawn) for each source plate;
#'   * `by_dest_plate`: the same for destination plates, with `dest_plate`,
#'     `transfers`, `dest_wells` and `volume` (received).
#' @examples
#' pl <- data.frame(`Source Plate Name` = "Src1",
#'                  `Source Well` = c("A1", "A1", "B1"),
#'                  `Destination Well` = c("A1", "A2", "A1"),
#'                  `Transfer Volume` = c(25, 50, 75), check.names = FALSE)
#' picklist_summary(pl)
#' @md
#' @export
picklist_summary <- function(transfers,
                             source_well = "Source Well",
                             dest_well = "Destination Well",
                             volume = "Transfer Volume",
                             source_plate_col = "Source Plate Name",
                             dest_plate_col = "Destination Plate Name") {
  if (!is.data.frame(transfers)) {
    stop("`transfers` must be a data frame.", call. = FALSE)
  }
  check_col(transfers, source_well, "source_well")
  check_col(transfers, dest_well, "dest_well")
  volume <- resolve_volume(transfers, volume)

  col_or_all <- function(col) {
    if (!is.null(col) && col %in% names(transfers)) {
      as.character(transfers[[col]])
    } else {
      rep("(all)", nrow(transfers))
    }
  }
  sp <- col_or_all(source_plate_col)
  dp <- col_or_all(dest_plate_col)
  canon <- function(w) sub("^([A-Z]+)0*([0-9]+)$", "\\1\\2", toupper(as.character(w)))
  sw <- canon(transfers[[source_well]])
  dw <- canon(transfers[[dest_well]])
  vol <- if (is.null(volume)) rep(NA_real_, nrow(transfers)) else as.numeric(transfers[[volume]])

  by_plate <- function(plate, well, label, name) {
    out <- lapply(unique(plate), function(p) {
      i <- plate == p
      r <- data.frame(plate = p, transfers = sum(i),
                      wells = length(unique(well[i])),
                      volume = if (is.null(volume)) NA_real_ else sum(vol[i], na.rm = TRUE),
                      stringsAsFactors = FALSE)
      names(r) <- c(name, "transfers", label, "volume")
      r
    })
    if (length(out)) do.call(rbind, out) else {
      r <- data.frame(character(), integer(), integer(), numeric())
      names(r) <- c(name, "transfers", label, "volume")
      r
    }
  }

  list(
    transfers = nrow(transfers),
    source_wells = length(unique(paste(sp, sw))),
    dest_wells = length(unique(paste(dp, dw))),
    total_volume = if (is.null(volume)) NA_real_ else sum(vol, na.rm = TRUE),
    by_source_plate = by_plate(sp, sw, "source_wells", "source_plate"),
    by_dest_plate = by_plate(dp, dw, "dest_wells", "dest_plate")
  )
}

#' Plot a source plate and a destination plate with transfer links
#'
#' Draws the plate(s) transfers go *from* beside the plate(s) they go *to*, e.g.
#' an Echo picklist as a 384PP source plate next to a 384-well assay plate.
#' When the picklist uses several plates, all source plates are stacked down
#' the left and all destination plates down the right of the same panel, each
#' titled with its name and type, so links can run between any two plates. Each
#' well is either used by the transfers or empty. Source wells are shaded by
#' the volume drawn from them (or the number of transfers), destination wells
#' by the volume received.
#'
#' Both plates are drawn in one panel, so a destination well and a source well
#' of different plate sizes sit on the same grid spacing. There is one fill
#' legend per side in the plate titles (the maximum of the scale) rather than
#' a ggplot legend, because a single ggplot cannot hold two fill scales.
#'
#' @section Interactive use:
#' With `interactive = TRUE`, hovering a source well outlines the destination
#' wells it feeds and brightens the links to them; hovering a destination well
#' does the same for its sources. Tooltips give the well, the number of
#' transfers and the total volume. Highlighting runs a small piece of
#' JavaScript added with `htmlwidgets::onRender()` that matches the ggiraph
#' `data_id`s (`s:<plate>:<well>` source, `d:<plate>:<well>` destination,
#' `l:<splate>:<well>><dplate>:<well>` link), so the same well on two plates
#' highlights separately. With `links = "hover"` links are invisible until a well is hovered;
#' above 2000 distinct source-destination pairs they are not drawn.
#'
#' @section Static use:
#' With `links = "hover"` (the default) a static plot cannot hover, so it
#' draws all links when there are at most 500 transfers and none above that;
#' the caption says so. An explicit `links = "all"` always draws them, with
#' alpha scaled down for many transfers.
#'
#' @param transfers A data frame with one row per transfer (an Echo picklist).
#' @param source_well,dest_well Names of the source and destination well
#'   columns. Defaults match the Echo picklist.
#' @param volume Name of the transfer volume column, or `NULL` to shade by
#'   transfer count. The default `"Transfer Volume"` falls back to counts when
#'   the column is absent.
#' @param source_plate,dest_plate Plate sizes (number of wells), as accepted by
#'   [plot_plate()]. One number applies to every plate on that side; a named
#'   vector or list keyed by plate name sets each plate (`c(Src1 = 6, Src2 = 384)`).
#'   `NULL` (default) reads the size from the plate type column
#'   (`6RES` is 6, `384PP_AQ_BP` and `384LDV` are 384, `1536LDV` is 1536; an
#'   unrecognised type is taken as 384), or uses 384 without one.
#' @param source_type_col,dest_type_col Names of the plate type columns, used
#'   for sizes and titles when present.
#' @param source_fill,dest_fill What shades each side: `"volume"` (default),
#'   `"count"` of transfers, or the name of a column of `transfers` (its first
#'   value per well, drawn as discrete colours).
#' @param links `"hover"` (default), `"all"` or `"none"`; see the sections
#'   above.
#' @param link_curvature Curvature of the links; `0` draws straight segments.
#' @param link_alpha Link transparency; the default scales with the number of
#'   links.
#' @param source_plate_col,dest_plate_col Names of the plate name columns.
#' @param source_plate_id,dest_plate_id Optional filter: draw only these plates
#'   (values of the plate name column). By default all plates are drawn.
#' @param volume_unit Unit shown in tooltips and titles.
#' @param interactive If `TRUE`, return a ggiraph girafe object (needs the
#'   ggiraph and htmlwidgets packages).
#'
#' @return A ggplot, or a girafe object when `interactive = TRUE`. The result
#'   carries `attr(, "summary")`, the [picklist_summary()] of the plotted
#'   transfers (for a girafe, on the `x` list element too:
#'   `attr(g, "summary")`).
#' @seealso [picklist_summary()], [plot_plate()]
#' @examples
#' pl <- data.frame(
#'   `Source Plate Name` = "Src1",
#'   `Source Well` = rep(c("A1", "B1", "C1"), c(3, 2, 2)),
#'   `Destination Well` = c("A1", "A2", "A3", "A1", "B1", "B2", "B3"),
#'   `Transfer Volume` = c(25, 50, 25, 100, 25, 50, 75),
#'   check.names = FALSE
#' )
#' p <- plot_plate_pair(pl, source_plate = 384, dest_plate = 96)
#' attr(p, "summary")$transfers
#' p
#'
#' \dontrun{
#' plot_plate_pair(pl, source_plate = 384, dest_plate = 96, interactive = TRUE)
#' }
#' @md
#' @export
plot_plate_pair <- function(transfers,
                            source_well = "Source Well",
                            dest_well = "Destination Well",
                            volume = "Transfer Volume",
                            source_plate = NULL,
                            dest_plate = NULL,
                            source_fill = "volume",
                            dest_fill = "volume",
                            links = c("hover", "all", "none"),
                            link_curvature = 0.1,
                            link_alpha = NULL,
                            source_plate_col = "Source Plate Name",
                            dest_plate_col = "Destination Plate Name",
                            source_type_col = "Source Plate Type",
                            dest_type_col = "Destination Plate Type",
                            source_plate_id = NULL,
                            dest_plate_id = NULL,
                            volume_unit = "nL",
                            interactive = FALSE) {
  links <- match.arg(links)
  if (!is.data.frame(transfers)) {
    stop("`transfers` must be a data frame.", call. = FALSE)
  }
  if (!(isTRUE(interactive) || identical(interactive, FALSE))) {
    stop("`interactive` must be TRUE or FALSE.", call. = FALSE)
  }
  if (interactive) {
    for (pkg in c("ggiraph", "htmlwidgets")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("`interactive = TRUE` needs the '", pkg, "' package. ",
             "Install it with install.packages(\"", pkg, "\").", call. = FALSE)
      }
    }
  }
  check_col(transfers, source_well, "source_well")
  check_col(transfers, dest_well, "dest_well")
  volume <- resolve_volume(transfers, volume)

  transfers <- pick_plate(transfers, source_plate_col, source_plate_id, "source")
  transfers <- pick_plate(transfers, dest_plate_col, dest_plate_id, "destination")
  if (nrow(transfers) == 0) {
    stop("`transfers` has no rows to draw.", call. = FALSE)
  }

  summary <- picklist_summary(transfers, source_well, dest_well, volume,
                              source_plate_col, dest_plate_col)
  n_transfers <- nrow(transfers)

  sp <- plate_names(transfers, source_plate_col)
  dp <- plate_names(transfers, dest_plate_col)
  src_lay <- plate_layout(sp, type_values(transfers, source_type_col), source_plate, "source")
  dst_lay <- plate_layout(dp, type_values(transfers, dest_type_col), dest_plate, "destination")

  # Canonical well keys (A1, not A01) so A1 and A01 are the same well, checked
  # against the size of the plate each transfer belongs to.
  tr <- data.frame(
    sp = sp, dp = dp,
    s = plate_well_keys(transfers[[source_well]], sp, src_lay, "source"),
    d = plate_well_keys(transfers[[dest_well]], dp, dst_lay, "destination"),
    vol = if (is.null(volume)) 1 else as.numeric(transfers[[volume]]),
    stringsAsFactors = FALSE
  )
  tr$vol[is.na(tr$vol)] <- 0
  tr$su <- paste(tr$sp, tr$s, sep = ":")
  tr$du <- paste(tr$dp, tr$d, sep = ":")
  use_volume <- !is.null(volume)

  # Layout: one panel. Source plates are stacked down the left, destination
  # plates down the right; y runs downwards as negative numbers.
  gap <- max(5, round(0.15 * (max(src_lay$colmax) + max(dst_lay$colmax))))
  ox <- max(src_lay$colmax) + gap
  dst_lay$xo <- ox
  src_lay$xo <- 0
  total_w <- ox + max(dst_lay$colmax) + 4
  height_rows <- max(sum(src_lay$rowmax + 5) - 5, sum(dst_lay$rowmax + 5) - 5)

  src <- place_wells(summarise_wells(tr, "su", "sp", "s", transfers, source_fill, "source_fill"), src_lay)
  dst <- place_wells(summarise_wells(tr, "du", "dp", "d", transfers, dest_fill, "dest_fill"), dst_lay)

  pairs <- unique(tr[, c("su", "du")])
  names(pairs) <- c("s", "d")
  n_links <- nrow(pairs)

  # Decide what links to draw.
  link_note <- NULL
  if (interactive) {
    draw_links <- links != "none" && n_links <= 2000
    if (links != "none" && !draw_links) {
      link_note <- "Links not drawn: more than 2000 source-destination pairs."
    }
  } else {
    draw_links <- links == "all" || (links == "hover" && n_transfers <= 500)
    if (links == "hover" && n_transfers > 500) {
      link_note <- paste0("Links not drawn: more than 500 transfers (",
                          n_transfers, "). Use links = \"all\" to draw them.")
    }
  }

  hover_mode <- interactive && links == "hover"
  tiles_src <- tile_data(src, "s", "Source", use_volume, volume_unit,
                         source_fill, lo = "#cfe3e8", hi = "#1f5f70")
  tiles_dst <- tile_data(dst, "d", "Destination", use_volume, volume_unit,
                         dest_fill, lo = "#f1dcc9", hi = "#8a3b12")

  p <- ggplot2::ggplot()
  for (lay in list(src_lay, dst_lay)) {
    for (i in seq_len(nrow(lay))) {
      p <- p + plate_empty_layer(lay$info[[i]], x_offset = lay$xo[i],
                                 y_sign = -1, y_offset = lay$yo[i])
    }
  }

  if (draw_links) {
    ls <- src[match(pairs$s, src$key), c("x", "y")]
    ld <- dst[match(pairs$d, dst$key), c("x", "y")]
    ldat <- data.frame(x = ls$x, y = ls$y, xend = ld$x, yend = ld$y,
                       id = paste0("l:", pairs$s, ">", pairs$d),
                       stringsAsFactors = FALSE)
    alpha <- if (hover_mode) 0 else if (!is.null(link_alpha)) link_alpha else
      min(0.7, max(0.05, 20 / n_links))
    link_geom <- if (interactive) {
      if (link_curvature == 0) ggiraph::geom_segment_interactive else
        ggiraph::geom_curve_interactive
    } else {
      if (link_curvature == 0) ggplot2::geom_segment else ggplot2::geom_curve
    }
    args <- list(data = ldat, colour = "grey20", alpha = alpha, linewidth = 0.3,
                 inherit.aes = FALSE)
    args$mapping <- if (interactive) {
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, data_id = id)
    } else {
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
    }
    if (link_curvature != 0) args$curvature <- link_curvature
    p <- p + do.call(link_geom, args)
  }

  tile_geom <- if (interactive) ggiraph::geom_tile_interactive else ggplot2::geom_tile
  for (td in list(tiles_src, tiles_dst)) {
    m <- if (interactive) {
      ggplot2::aes(x = x, y = y, fill = fill, tooltip = tooltip, data_id = id)
    } else {
      ggplot2::aes(x = x, y = y, fill = fill)
    }
    p <- p + tile_geom(data = td$data, mapping = m, colour = "white",
                       width = 0.92, height = 0.92, linewidth = 0.2,
                       inherit.aes = FALSE)
  }

  axes <- do.call(rbind, c(
    lapply(seq_len(nrow(src_lay)), function(i)
      axis_labels(src_lay$info[[i]], 0, "left", src_lay$size[i], src_lay$yo[i])),
    lapply(seq_len(nrow(dst_lay)), function(i)
      axis_labels(dst_lay$info[[i]], ox, "right", dst_lay$size[i], dst_lay$yo[i]))
  ))
  titles <- rbind(
    plate_titles(src_lay, "Source", tiles_src$caption, 0),
    plate_titles(dst_lay, "Destination", tiles_dst$caption, ox)
  )
  caption <- paste0(
    format(summary$transfers, big.mark = ","), " transfers, ",
    summary$source_wells, " source wells to ", summary$dest_wells,
    " destination wells",
    if (use_volume) paste0(", ", format(summary$total_volume, big.mark = ",",
                                        trim = TRUE), " ", volume_unit),
    ".", if (!is.null(link_note)) paste0("\n", link_note) else ""
  )

  rmax <- height_rows

  p <- p +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(data = axes, ggplot2::aes(x = x, y = y, label = label,
                                                 hjust = hjust, size = size),
                       colour = "grey30", inherit.aes = FALSE,
                       show.legend = FALSE) +
    ggplot2::scale_size_identity() +
    ggplot2::geom_text(data = titles, ggplot2::aes(x = x, y = y, label = label),
                       hjust = 0, vjust = 0, size = 3.4, lineheight = 0.95,
                       colour = "grey15", inherit.aes = FALSE) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(-2, total_w - 2),
                         ylim = c(-rmax - 1.8, 3.6), expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, caption = caption) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9,
                                                        colour = "grey25"),
                   plot.margin = ggplot2::margin(4, 6, 4, 6))

  if (interactive) {
    rel <- link_relations(pairs)
    g <- ggiraph::girafe(
      ggobj = p, width_svg = 11,
      height_svg = 11 * (rmax + 6.5) / (total_w) + 0.5,
      options = list(
        ggiraph::opts_hover(css = "stroke:#000;stroke-width:1.5px;"),
        ggiraph::opts_tooltip(use_fill = FALSE, opacity = 0.95)
      )
    )
    g <- htmlwidgets::onRender(g, pair_hover_js, data = rel)
    attr(g, "summary") <- summary
    return(g)
  }
  attr(p, "summary") <- summary
  p
}

# ---- helpers ---------------------------------------------------------------

check_col <- function(df, col, arg) {
  if (!is.character(col) || length(col) != 1 || !col %in% names(df)) {
    stop("`", arg, "` column '", paste(col, collapse = ", "),
         "' not found in `transfers`.", call. = FALSE)
  }
}

# NULL if there is no volume column. The default name is dropped quietly when
# absent; any other missing name is an error.
resolve_volume <- function(df, volume) {
  if (is.null(volume)) return(NULL)
  if (!is.character(volume) || length(volume) != 1) {
    stop("`volume` must be a column name or NULL.", call. = FALSE)
  }
  if (volume %in% names(df)) {
    if (!is.numeric(df[[volume]])) {
      stop("`volume` column '", volume, "' must be numeric.", call. = FALSE)
    }
    return(volume)
  }
  if (identical(volume, "Transfer Volume")) return(NULL)
  stop("`volume` column '", volume, "' not found in `transfers`.", call. = FALSE)
}

# Optional filter to the named plate(s); drawing all plates is the default.
pick_plate <- function(df, col, id, what) {
  if (is.null(id)) return(df)
  if (is.null(col) || !col %in% names(df)) {
    stop("`", col, "` is not a column of `transfers`.", call. = FALSE)
  }
  plates <- unique(as.character(df[[col]]))
  missing_id <- setdiff(id, plates)
  if (length(missing_id)) {
    stop("No ", what, " plate named '", missing_id[1], "' in `", col,
         "`. Found: ", paste(plates, collapse = ", "), ".", call. = FALSE)
  }
  df[as.character(df[[col]]) %in% id, , drop = FALSE]
}

plate_names <- function(df, col) {
  if (!is.null(col) && col %in% names(df)) as.character(df[[col]]) else
    rep("(all)", nrow(df))
}

type_values <- function(df, col) {
  if (!is.null(col) && col %in% names(df)) as.character(df[[col]]) else NULL
}

# "6RES" -> 6, "384PP_AQ_BP" / "384LDV" -> 384, "1536LDV" -> 1536; unknown -> 384.
size_from_type <- function(type) {
  n <- suppressWarnings(as.integer(sub("^([0-9]+).*$", "\\1", type)))
  ifelse(grepl("^[0-9]+", type) & n %in% c(6, 12, 24, 48, 96, 384, 1536), n, 384L)
}

# One row per plate (in order of appearance) with its size, type label, layout
# info and vertical offset. `spec`: NULL (from the type column, else 384), one
# number for all plates, or a named vector/list keyed by plate name.
plate_layout <- function(names_vec, types, spec, what) {
  plates <- unique(names_vec)
  first <- match(plates, names_vec)
  type <- if (is.null(types)) rep(NA_character_, length(plates)) else types[first]
  if (is.null(spec)) {
    size <- if (is.null(types)) rep(384L, length(plates)) else size_from_type(type)
  } else {
    spec <- unlist(spec)
    if (is.null(names(spec))) {
      if (length(spec) != 1) {
        stop("`", what, "_plate` must be one size or a named vector keyed by plate name.",
             call. = FALSE)
      }
      size <- rep(spec[[1]], length(plates))
    } else {
      miss <- setdiff(plates, names(spec))
      if (length(miss)) {
        stop("`", what, "_plate` has no size for plate '", miss[1], "'.", call. = FALSE)
      }
      size <- unname(spec[plates])
    }
  }
  info <- lapply(size, platesize_check)
  rowmax <- vapply(info, function(i) i$rowmax, 1)
  out <- data.frame(name = plates, type = type, size = size, rowmax = rowmax,
                    colmax = vapply(info, function(i) i$colmax, 1),
                    yo = cumsum(c(0, rowmax + 5))[seq_along(plates)],
                    xo = 0, stringsAsFactors = FALSE)
  out$info <- info
  out
}

plate_well_keys <- function(wells, plate_vec, lay, what) {
  out <- character(length(wells))
  for (i in seq_len(nrow(lay))) {
    j <- plate_vec == lay$name[i]
    out[j] <- well_key(wells[j], lay$info[[i]], lay$size[i], what)
  }
  out
}

plate_titles <- function(lay, side, caption, x_offset) {
  title <- vapply(seq_len(nrow(lay)), function(i) {
    nm <- if (lay$name[i] == "(all)") "" else paste0(" ", lay$name[i])
    kind <- if (is.na(lay$type[i])) paste0(lay$size[i], "-well") else lay$type[i]
    t <- paste0(side, nm, " (", kind, ")")
    if (i == 1) paste0(t, "\n", caption) else t
  }, "")
  data.frame(x = x_offset + 0.5, y = 1.6 - lay$yo, label = title,
             stringsAsFactors = FALSE)
}

# "A01"/"a1" -> "A1", checked against the plate size.
well_key <- function(wells, info, size, what) {
  wells <- as.character(wells)
  if (anyNA(wells)) {
    stop("Missing ", what, " well in `transfers`.", call. = FALSE)
  }
  rc <- to_rows_columns(data.frame(w = wells, stringsAsFactors = FALSE), "w")
  bad <- rc$Row > info$rowmax | rc$Column > info$colmax | rc$Column < 1
  if (any(bad)) {
    stop(what, " well '", wells[bad][1], "' does not fit a ", size,
         "-well plate.", call. = FALSE)
  }
  paste0(info$rows[rc$Row], rc$Column)
}

# One row per used well. `uid` is "<plate>:<well>".
summarise_wells <- function(tr, uid, plate, well, transfers, fill, arg) {
  idx <- split(seq_len(nrow(tr)), tr[[uid]])
  out <- data.frame(
    key = names(idx),
    plate = vapply(idx, function(i) tr[[plate]][i[1]], ""),
    well = vapply(idx, function(i) tr[[well]][i[1]], ""),
    n = vapply(idx, length, 1L),
    volume = vapply(idx, function(i) sum(tr$vol[i]), 1),
    stringsAsFactors = FALSE, row.names = NULL
  )
  out$group <- NA_character_
  if (!fill %in% c("volume", "count")) {
    if (!fill %in% names(transfers)) {
      stop("`", arg, "` must be \"volume\", \"count\" or a column of ",
           "`transfers`; '", fill, "' was not found.", call. = FALSE)
    }
    out$group <- vapply(idx, function(i) as.character(transfers[[fill]][i[1]]), "")
  }
  out
}

place_wells <- function(df, lay) {
  p <- match(df$plate, lay$name)
  m <- regmatches(df$well, regexec("^([A-Z]+)([0-9]+)$", df$well))
  row <- vapply(seq_along(p), function(i) match(m[[i]][2], lay$info[[p[i]]]$rows), 1L)
  df$x <- as.numeric(vapply(m, `[`, "", 3)) + lay$xo[p]
  df$y <- -row - lay$yo[p]
  df
}

# Colour each used well and describe the scale for the plate title.
tile_data <- function(w, prefix, side, use_volume, unit, fill, lo, hi) {
  w$id <- paste0(prefix, ":", w$key)
  label <- ifelse(w$plate == "(all)", w$well, paste0(w$plate, " ", w$well))
  vol_txt <- if (use_volume) {
    paste0(", ", format(w$volume, big.mark = ",", trim = TRUE), " ", unit)
  } else ""
  w$tooltip <- paste0(side, " ", label, ": ", w$n,
                      ifelse(w$n == 1, " transfer", " transfers"), vol_txt)
  ramp <- grDevices::colorRamp(c(lo, hi))
  shade <- function(v) {
    top <- max(v)
    t <- if (top > 0) v / top else rep(1, length(v))
    t <- 0.15 + 0.85 * t
    grDevices::rgb(ramp(t), maxColorValue = 255)
  }
  if (fill == "volume") {
    v <- if (use_volume) w$volume else w$n
    w$fill <- shade(v)
    caption <- if (use_volume) {
      paste0("shaded by ", unit, " (max ", format(max(v), big.mark = ",", trim = TRUE), ")")
    } else {
      paste0("shaded by transfers (max ", max(v), ")")
    }
  } else if (fill == "count") {
    w$fill <- shade(w$n)
    caption <- paste0("shaded by transfers (max ", max(w$n), ")")
  } else {
    lev <- sort(unique(w$group))
    w$fill <- grDevices::hcl.colors(max(length(lev), 3), "Set 2")[match(w$group, lev)]
    caption <- paste0("coloured by ", fill, " (", length(lev), " groups)")
  }
  list(data = w, caption = caption)
}

axis_labels <- function(info, x_offset, side, size, y_offset = 0) {
  step <- if (info$colmax > 24) 2 else 1
  txt <- switch(as.character(size), "1536" = 1.5, "384" = 2.2, 2.8)
  cols <- seq(1, info$colmax, by = step)
  rows <- seq(1, info$rowmax, by = step)
  rbind(
    data.frame(x = cols + x_offset, y = 0 - y_offset, label = as.character(cols),
               hjust = 0.5, size = txt, stringsAsFactors = FALSE),
    data.frame(x = if (side == "left") x_offset + 0 else x_offset + info$colmax + 1,
               y = -rows - y_offset, label = info$rows[rows],
               hjust = if (side == "left") 1 else 0, size = txt,
               stringsAsFactors = FALSE)
  )
}

# For each id, the ids that should light up when it is hovered.
link_relations <- function(pairs) {
  by_s <- split(pairs$d, pairs$s)
  by_d <- split(pairs$s, pairs$d)
  rel <- list()
  for (s in names(by_s)) {
    rel[[paste0("s:", s)]] <- as.list(c(paste0("d:", by_s[[s]]),
                                        paste0("l:", s, ">", by_s[[s]])))
  }
  for (d in names(by_d)) {
    rel[[paste0("d:", d)]] <- as.list(c(paste0("s:", by_d[[d]]),
                                        paste0("l:", by_d[[d]], ">", d)))
  }
  rel
}

# Runs after ggiraph has drawn the svg. Elements carry data-id attributes.
pair_hover_js <- "
function(el, x, rel) {
  var svg = el.querySelector('svg');
  if (!svg) return;
  var lit = [];
  function all(id) {
    var out = [], nodes = svg.querySelectorAll('[data-id]');
    for (var i = 0; i < nodes.length; i++) {
      if (nodes[i].getAttribute('data-id') === id) out.push(nodes[i]);
    }
    return out;
  }
  // Links never take the mouse, so wells underneath stay hoverable.
  var nodes = svg.querySelectorAll('[data-id^=\"l:\"]');
  for (var i = 0; i < nodes.length; i++) nodes[i].style.pointerEvents = 'none';
  function clear() {
    lit.forEach(function(n) {
      n.style.removeProperty('stroke'); n.style.removeProperty('stroke-width');
      n.style.removeProperty('stroke-opacity');
    });
    lit = [];
  }
  svg.addEventListener('mouseover', function(ev) {
    var t = ev.target.closest ? ev.target.closest('[data-id]') : null;
    clear();
    if (!t) return;
    var ids = [].concat(rel[t.getAttribute('data-id')] || []);
    ids.forEach(function(id) {
      all(id).forEach(function(n) {
        if (id.indexOf('l:') === 0) {
          n.style.strokeOpacity = '0.9'; n.style.stroke = '#c0392b';
          n.style.strokeWidth = '1.4px';
        } else {
          n.style.stroke = '#c0392b'; n.style.strokeWidth = '1.6px';
        }
        lit.push(n);
      });
    });
  });
  svg.addEventListener('mouseleave', clear);
}
"

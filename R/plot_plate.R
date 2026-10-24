#' Plot Plates
#'
#' Draws a plate map. By default every well is a tile that fills the panel,
#' with row letters and column numbers on the margins; wells with no data are
#' drawn faint so the outline of the plate is always visible.
#'
#' @param data A data frame containing the well locations and the values to fill
#' @param fill The column name of the value to fill consider wrapping in double brackets and quotes
#' @param well_id The column name of the character column of well locations
#' @param facet_rows The column name of the character column selected for the row facet
#' @param facet_cols The column name of the character column selected for the column facet
#' @param labeller Labeller for the facet strips, passed to \code{ggplot2::facet_grid()}.
#'   The default, \code{ggplot2::label_both}, prints \code{column: value}; use e.g.
#'   \code{ggplot2::label_value} or \code{ggplot2::as_labeller()} for custom strip text.
#' @param plate The size of the plate
#' @param size The size of the plotted data on the plate (`style = "points"` only)
#' @param shape The shape of each plotted data point on the plate (`style = "points"` only)
#' @param na_fill Fill color for the plate (`style = "points"` only)
#' @param na_size_ratio Background ratio (`style = "points"` only)
#' @param na_alpha Background transparency (`style = "points"` only)
#' @param style `"tiles"` (default) or `"points"`, the original fixed-size point look.
#' @param label Optional name of a column to print in each well.
#' @param label_size Text size for `label`. The default depends on the plate size.
#' @param legend `"auto"` (default) shows a legend unless there are more than 12
#'   keys; `TRUE` and `FALSE` force it on or off. Ignored for `style = "points"`,
#'   which never had one.
#' @param scale `"auto"` (default) fills numeric columns on a continuous
#'   sequential scale and everything else with discrete colours; use
#'   `"discrete"` to treat numbers as categories.
#' @param highlight Optional vector of wells (e.g. `c("A01", "B07")`) to outline.
#' @param empty_fill,empty_colour Fill and outline of wells without data.
#' @param interactive `FALSE` (default) returns a plain ggplot. `TRUE` returns a
#'   'ggiraph' girafe object where each well has `data_id` set to its well label
#'   and a tooltip. `"ggplot"` returns the interactive ggplot before it is
#'   wrapped by `ggiraph::girafe()`, so you can add layers, change the tooltip
#'   or set your own girafe options. Both interactive modes need the ggiraph
#'   package.
#' @param tooltip Optional name of a column whose values are used as the tooltip
#'   text when interactive (default: the well and the `fill` value). Ignored
#'   when not interactive.
#'
#' @return A ggplot object, or a girafe object when `interactive = TRUE`. With
#'   `interactive = "ggplot"` the ggplot has interactive layers (the well tiles
#'   use `ggiraph::geom_tile_interactive()`) and has not been through `girafe()`.
#' @examples
#' d <- data.frame(well = convert_well(1:36, "to_well", plate = 96),
#'                 grp = rep(c("x", "y", "z"), 12), n = 1:36)
#' plot_plate(d, fill = "grp", well_id = "well", plate = 96, label = "n")
#' plot_plate(d, fill = "n", well_id = "well", plate = 96, highlight = c("A01", "A02"))
#'
#' \dontrun{
#' # Custom tooltip, and the interactive ggplot before girafe() wraps it
#' d$note <- paste("well", d$well, "holds", d$grp)
#' plot_plate(d, "grp", "well", plate = 96, interactive = TRUE, tooltip = "note")
#' g <- plot_plate(d, "grp", "well", plate = 96, interactive = "ggplot", tooltip = "note")
#' ggiraph::girafe(ggobj = g)
#' }
#' @export


plot_plate <- function(data,
                            fill,
                            well_id,
                            facet_rows = NULL,
                            facet_cols = NULL,
                            labeller = ggplot2::label_both,
                            plate = 384,
                            size = 5,
                            shape = 22,
                            na_fill = "white",
                            na_size_ratio = 0.95,
                            na_alpha = 0.1,
                            style = c("tiles", "points"),
                            label = NULL,
                            label_size = NULL,
                            legend = "auto",
                            scale = c("auto", "discrete", "continuous"),
                            highlight = NULL,
                            empty_fill = "grey93",
                            empty_colour = "grey80",
                            interactive = FALSE,
                            tooltip = NULL) {

  style <- match.arg(style)
  scale <- match.arg(scale)
  plate_info <- platesize_check(plate)

  xlim <- c(0.5, plate_info$colmax + 0.5)
  ylim <- c(plate_info$rowmax + 0.5, 0.5)

  if (!fill %in% names(data)) {
    stop("`fill` column '", fill, "' not found in `data`.", call. = FALSE)
  }
  if (!is.null(label) && !label %in% names(data)) {
    stop("`label` column '", label, "' not found in `data`.", call. = FALSE)
  }
  if (!(identical(legend, "auto") || isTRUE(legend) || identical(legend, FALSE))) {
    stop("`legend` must be \"auto\", TRUE or FALSE.", call. = FALSE)
  }
  if (!(isTRUE(interactive) || identical(interactive, FALSE) ||
        identical(interactive, "ggplot"))) {
    stop("`interactive` must be FALSE, TRUE or \"ggplot\".", call. = FALSE)
  }
  if (!is.null(tooltip) && !(is.character(tooltip) && length(tooltip) == 1 &&
                             tooltip %in% names(data))) {
    stop("`tooltip` column '", paste(tooltip, collapse = ", "),
         "' not found in `data`.", call. = FALSE)
  }
  as_ggplot <- identical(interactive, "ggplot")
  interactive <- isTRUE(interactive) || as_ggplot
  if (interactive && !requireNamespace("ggiraph", quietly = TRUE)) {
    stop("`interactive = TRUE` needs the 'ggiraph' package. ",
         "Install it with install.packages(\"ggiraph\").", call. = FALSE)
  }

  plotting_data <- to_rows_columns(data, well_id)
  plotting_data$.well <- as.character(plotting_data[[well_id]])

  continuous <- scale == "continuous" ||
    (scale == "auto" && is.numeric(data[[fill]]))
  # Bind fill data into the dataframe for plotting, as a new column
  plotting_data$fill <- if (continuous) plotting_data[[fill]] else as.factor(plotting_data[[fill]])

  if (style == "points") {
    p <- plot_plate_points(plotting_data, plate_info, xlim, ylim,
                           size, shape, na_fill, na_size_ratio, na_alpha)
  } else {
    p <- plot_plate_tiles(plotting_data, plate_info, xlim, ylim, fill, continuous,
                          label, label_size, legend, highlight,
                          empty_fill, empty_colour, interactive, plate, tooltip)
  }

  # Optional faceting
  if (!is.null(facet_rows)) {
    if (!is.null(facet_cols)) {
      p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data[[facet_rows]]), cols = ggplot2::vars(.data[[facet_cols]]), labeller = labeller)
    } else {
      p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data[[facet_rows]]), labeller = labeller)
    }
  } else if (!is.null(facet_cols)) {
    p <- p + ggplot2::facet_grid(cols = ggplot2::vars(.data[[facet_cols]]), labeller = labeller)
  }

  if (interactive && !as_ggplot) {
    return(ggiraph::girafe(ggobj = p))
  }
  return(p)
}

# The original look: fixed-size points, no legend.
plot_plate_points <- function(plotting_data, plate_info, xlim, ylim,
                              size, shape, na_fill, na_size_ratio, na_alpha) {
  ggplot2::ggplot(plotting_data, ggplot2::aes(x = Column, y = Row, fill = fill)) +
    ggplot2::geom_point(colour = "gray20", shape = shape, size = size,
                        show.legend = FALSE) +
    ggplot2::geom_point(data = expand.grid(Column = seq(plate_info$colmax), Row = seq(1, plate_info$rowmax)),
                        ggplot2::aes(x = Column, y = Row), color = "grey90", fill = na_fill,
               shape = shape, size = size * na_size_ratio, alpha = na_alpha,
               show.legend = FALSE) +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
    ggplot2::scale_y_reverse(breaks = seq(1, plate_info$rowmax), labels = plate_info$rows) +
    ggplot2::scale_x_continuous(breaks = seq(1, plate_info$colmax), position = "top")
}

plot_plate_tiles <- function(plotting_data, plate_info, xlim, ylim, fill_name, continuous,
                             label, label_size, legend, highlight,
                             empty_fill, empty_colour, interactive, plate,
                             tooltip = NULL) {
  tile_geom <- if (isTRUE(interactive)) ggiraph::geom_tile_interactive else ggplot2::geom_tile

  # Empty wells first, so the plate outline shows wherever there is no data.
  p <- ggplot2::ggplot(plotting_data, ggplot2::aes(x = Column, y = Row)) +
    plate_empty_layer(plate_info, empty_fill, empty_colour)

  if (isTRUE(interactive)) {
    plotting_data$.tooltip <- if (is.null(tooltip)) {
      paste0(plotting_data$.well, ": ", plotting_data[[fill_name]])
    } else {
      as.character(plotting_data[[tooltip]])
    }
    p <- p + tile_geom(
      data = plotting_data,
      ggplot2::aes(fill = fill, tooltip = .tooltip, data_id = .well),
      colour = "white", width = 0.92, height = 0.92, linewidth = 0.2
    )
  } else {
    p <- p + tile_geom(ggplot2::aes(fill = fill),
                       colour = "white", width = 0.92, height = 0.92, linewidth = 0.2)
  }

  if (!is.null(label)) {
    if (is.null(label_size)) {
      label_size <- switch(as.character(plate), "1536" = 0.8, "384" = 1.7,
                           "96" = 3, "48" = 3.5, 4)
    }
    plotting_data$.label <- as.character(plotting_data[[label]])
    p <- p + ggplot2::geom_text(data = plotting_data,
                                ggplot2::aes(label = .label),
                                size = label_size, colour = "grey10",
                                inherit.aes = TRUE)
  }

  if (!is.null(highlight)) {
    hl <- data.frame(.well = toupper(as.character(highlight)), stringsAsFactors = FALSE)
    hl <- to_rows_columns(hl, ".well")
    p <- p + ggplot2::geom_tile(data = hl, ggplot2::aes(x = Column, y = Row),
                                fill = NA, colour = "black", width = 0.96,
                                height = 0.96, linewidth = 0.7,
                                inherit.aes = FALSE)
  }

  if (continuous) {
    p <- p + ggplot2::scale_fill_gradient(low = "#f4efe6", high = "#2b7a8c",
                                          na.value = "grey70")
    show_legend <- !identical(legend, FALSE)
  } else {
    n_keys <- length(unique(stats::na.omit(plotting_data$fill)))
    show_legend <- if (identical(legend, "auto")) n_keys <= 12 else isTRUE(legend)
  }

  axis_size <- plate_axis_size(plate)
  p <- p +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::scale_y_reverse(breaks = seq_len(plate_info$rowmax), labels = plate_info$rows) +
    ggplot2::scale_x_continuous(breaks = seq_len(plate_info$colmax), position = "top") +
    ggplot2::labs(x = NULL, y = NULL, fill = fill_name) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = axis_size),
      legend.position = if (show_legend) "right" else "none"
    )
  p
}

# Shared by plot_plate() and plot_plate_pair().
# Faint tile for every well of a plate, optionally shifted right by `x_offset`
# (and given a y-direction `y_sign`, -1 to draw rows downwards without a
# reversed scale).
plate_empty_layer <- function(plate_info, empty_fill = "grey93",
                              empty_colour = "grey80", x_offset = 0, y_sign = 1,
                              y_offset = 0) {
  grid <- expand.grid(Column = seq_len(plate_info$colmax),
                      Row = seq_len(plate_info$rowmax))
  grid$x <- grid$Column + x_offset
  grid$y <- y_sign * grid$Row - y_offset
  ggplot2::geom_tile(data = grid, ggplot2::aes(x = x, y = y),
                     fill = empty_fill, colour = empty_colour,
                     width = 0.92, height = 0.92, linewidth = 0.2,
                     inherit.aes = FALSE)
}

plate_axis_size <- function(plate) {
  switch(as.character(plate), "1536" = 5, "384" = 7, 9)
}

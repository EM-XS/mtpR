#' Paint wells of a microtiter plate with labels
#'
#' An 'htmlwidgets' plate map you click or drag across to give wells labels
#' ("paint" them). It works in Shiny, in R Markdown and the RStudio viewer, and
#' as a standalone offline HTML file ([plate_picker_html()]).
#'
#' The widget is a plain SVG grid with row letters and column numbers as
#' headers. Click a well to toggle the active label; press and drag to paint
#' along a stroke (a drag that starts on a well already holding the active
#' label erases along the stroke instead); click a row letter or column number
#' to paint the whole line, or to clear it when every well in it already has the
#' label; hold Shift while dragging to paint a rectangle. Locked wells are
#' skipped. The toolbar has one chip per label (also keys 1-9), an eraser,
#' Undo/Redo (Ctrl/Cmd+Z, with Shift to redo), Clear and a live count per label.
#' The grid is keyboard accessible: arrow keys move a focus ring, Space or
#' Enter toggles the focused well and Escape cancels a stroke in progress.
#'
#' @section Colours:
#' Labels get a fixed colour-blind-safer palette (eight hues, in a fixed order).
#' There are only eight default colours, so more than eight labels is an error
#' unless `colours` gives a colour for every label.
#'
#' @section Painting over a background:
#' `background` fills the wells themselves (for example a host app's reaction
#' colours). A painted label is drawn on top as a rounded chip in the label's
#' colour with a surface-coloured halo and a thin edge, so it stays readable on
#' any background and at 384 and 1536 wells. With `multiple = TRUE` a well
#' holding several labels shows the chip split into stacked stripes. Background
#' `text` is printed over the chip when the wells are big enough (about 24 px).
#'
#' @section Shiny:
#' The widget sets `input$<id>` to the assignments at the end of every stroke,
#' header click, undo, redo and clear (not on every pointer move), and
#' `input$<id>_active` to the active label when it changes. Shiny delivers the
#' assignments as a *list* of `well` and `label` vectors (each itself a list);
#' turn it into a data frame with [plate_picker_value()]. Use
#' [update_plate_picker()] to change the painting without re-rendering. The
#' Copy rows, Download CSV and Save page buttons are hidden in Shiny unless
#' `export_buttons = TRUE`.
#'
#' @param plate Plate size, one of the sizes [platesize_check()] allows.
#' @param labels Character vector of label names the user paints with, at most
#'   eight unless `colours` names a colour for each. `NULL` (the default) gives
#'   one label, `"selected"`, for plain well selection.
#' @param value Initial assignments: a data frame with columns `well` and
#'   `label`, or a named character vector (`c(A01 = "Blank")`). With
#'   `labels = NULL` a bare character vector of wells also works. Wells are
#'   checked against the plate and normalised (`"A1"` becomes `"A01"`).
#' @param background Optional data frame with columns `well` and `fill` (a
#'   colour), and optionally `text` and `tooltip`, drawn under the labels.
#' @param locked Wells that cannot be painted; drawn muted.
#' @param active The label the brush starts with (default: the first).
#' @param colours Optional named vector, label to colour.
#' @param multiple If `FALSE` (default) a well holds one label and painting a
#'   new one replaces it; if `TRUE` a well may hold several.
#' @param toolbar Show the toolbar?
#' @param theme `"auto"` (follow `prefers-color-scheme`), `"light"` or `"dark"`.
#' @param export_buttons `NULL` (default) shows Copy rows, Download CSV and Save
#'   page outside Shiny only; `TRUE` or `FALSE` forces them on or off.
#' @param width,height Widget size. The default width is the container width
#'   and the height follows from it.
#' @param elementId Optional HTML id.
#'
#' @return An 'htmlwidgets' object.
#' @examples
#' p <- plate_picker(96, labels = c("Blank", "Standard"),
#'                   value = c(A01 = "Blank", A02 = "Blank", B01 = "Standard"))
#' p
#'
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(plate_pickerOutput("plate"), tableOutput("picked"))
#'   server <- function(input, output, session) {
#'     output$plate <- renderPlate_picker(plate_picker(96, c("Blank", "Standard")))
#'     output$picked <- renderTable(plate_picker_value(input$plate))
#'   }
#'   shinyApp(ui, server)
#' }
#' @seealso [plate_picker_html()], [read_plate_picker()], [plate_picker_value()],
#'   [update_plate_picker()]. [design_plate()] lays wells out from rules instead
#'   of by hand.
#' @export
plate_picker <- function(plate = 96,
                         labels = NULL,
                         value = NULL,
                         background = NULL,
                         locked = character(),
                         active = NULL,
                         colours = NULL,
                         multiple = FALSE,
                         toolbar = TRUE,
                         theme = c("auto", "light", "dark"),
                         export_buttons = NULL,
                         width = NULL,
                         height = NULL,
                         elementId = NULL) {
  theme <- match.arg(theme)
  info <- platesize_check(plate)
  if (!is.logical(multiple) || length(multiple) != 1 || is.na(multiple)) {
    stop("`multiple` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(toolbar) || length(toolbar) != 1 || is.na(toolbar)) {
    stop("`toolbar` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(export_buttons) &&
      (!is.logical(export_buttons) || length(export_buttons) != 1 || is.na(export_buttons))) {
    stop("`export_buttons` must be NULL, TRUE or FALSE.", call. = FALSE)
  }

  plain <- is.null(labels)
  if (plain) labels <- "selected"
  picker_check_labels(labels)
  cols <- picker_colours(labels, colours)

  if (is.null(active)) active <- labels[1]
  if (!is.character(active) || length(active) != 1 || !active %in% labels) {
    stop("`active` must be one of the labels: ", paste(labels, collapse = ", "), ".",
         call. = FALSE)
  }

  val <- picker_normalise_value(value, plate, labels, multiple, plain)
  lock <- picker_normalise_wells(locked, plate, "locked")
  bg <- picker_normalise_background(background, plate)

  x <- list(
    plate = plate,
    nrow = info$rowmax,
    ncol = info$colmax,
    rowLabels = I(info$rows),
    labels = I(labels),
    colours = I(unname(cols)),
    active = active,
    multiple = multiple,
    toolbar = toolbar,
    theme = theme,
    exportButtons = if (is.null(export_buttons)) "auto" else export_buttons,
    value = picker_value_list(val),
    background = bg,
    locked = I(lock)
  )

  htmlwidgets::createWidget(
    name = "plate_picker",
    x = x,
    width = width,
    height = height,
    package = "mtpR",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%", defaultHeight = "auto",
      padding = 0, browser.fill = FALSE, viewer.fill = FALSE, knitr.figure = FALSE
    )
  )
}

#' Shiny bindings for plate_picker
#'
#' @param outputId Output variable to read from.
#' @param width,height CSS sizes (a string such as `"100%"` or a number of
#'   pixels). The default height, `"auto"`, follows the plate's shape.
#' @param expr An expression that returns a [plate_picker()] widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression?
#' @return `plate_pickerOutput()` returns a Shiny output element and
#'   `renderPlate_picker()` a render function.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(plate_pickerOutput("plate"))
#'   server <- function(input, output, session) {
#'     output$plate <- renderPlate_picker(plate_picker(384))
#'   }
#'   shinyApp(ui, server)
#' }
#' @name plate_picker-shiny
NULL

#' @rdname plate_picker-shiny
#' @export
plate_pickerOutput <- function(outputId, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(outputId, "plate_picker", width, height,
                                 package = "mtpR")
}

#' @rdname plate_picker-shiny
#' @export
renderPlate_picker <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, plate_pickerOutput, env, quoted = TRUE)
}

#' Change a plate_picker from the server without re-rendering it
#'
#' Sends the new state to the browser, which redraws only what changed (the
#' painting can be undone with the widget's Undo). The widget then reports the
#' new assignments through `input$<id>` as usual. `NULL` leaves a setting alone;
#' to clear something pass an empty value (`character()`, or a data frame with
#' no rows).
#'
#' @param session The Shiny session.
#' @param id The output id of the widget (not namespaced; the session's
#'   namespace is added).
#' @param value New assignments, as for [plate_picker()]. `NULL` leaves the
#'   painting alone, an empty data frame clears it.
#' @param active A label to make the active brush.
#' @param locked New locked wells (`character()` unlocks everything).
#' @param background New background data frame (see [plate_picker()]).
#' @return Called for its side effect; returns `NULL` invisibly.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(plate_pickerOutput("plate"),
#'                   actionButton("fill", "Blank first column"))
#'   server <- function(input, output, session) {
#'     output$plate <- renderPlate_picker(plate_picker(96, c("Blank", "Standard")))
#'     observeEvent(input$fill, {
#'       update_plate_picker(session, "plate",
#'         value = data.frame(well = sprintf("%s01", LETTERS[1:8]), label = "Blank"))
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#' @export
update_plate_picker <- function(session, id, value = NULL, active = NULL,
                                locked = NULL, background = NULL) {
  msg <- list(id = session$ns(id))
  # The size and labels are not known here, so wells are only normalised and
  # checked for shape; the browser ignores wells that are not on the plate.
  if (!is.null(value)) {
    msg$value <- picker_value_list(picker_normalise_value(value, NULL, NULL, TRUE, FALSE))
  }
  if (!is.null(active)) {
    if (!is.character(active) || length(active) != 1) {
      stop("`active` must be a single label.", call. = FALSE)
    }
    msg$active <- active
  }
  if (!is.null(locked)) msg$locked <- I(picker_normalise_wells(locked, NULL, "locked"))
  if (!is.null(background)) msg$background <- picker_normalise_background(background, NULL)
  session$sendCustomMessage("mtpr_plate_picker", msg)
  invisible(NULL)
}

#' Turn the assignments a plate_picker reports into a data frame
#'
#' In Shiny, `input$<id>` of a [plate_picker()] arrives as a list with the
#' elements `well` and `label`, each a list of strings. This returns a data
#' frame with columns `well` and `label`, sorted in plate order (a well with
#' several labels gets one row per label).
#'
#' @param x The input value: that list, a data frame with `well` and `label`,
#'   or `NULL` (no wells painted).
#' @return A data frame with character columns `well` and `label`.
#' @examples
#' plate_picker_value(list(well = list("B02", "A01"), label = list("Blank", "Blank")))
#' @export
plate_picker_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(picker_empty())
  if (is.data.frame(x)) x <- as.list(x)
  if (!is.list(x) || !all(c("well", "label") %in% names(x))) {
    stop("`x` must be a list or data frame with `well` and `label`.", call. = FALSE)
  }
  well <- as.character(unlist(x$well))
  label <- as.character(unlist(x$label))
  if (length(well) != length(label)) {
    stop("`well` and `label` must have the same length.", call. = FALSE)
  }
  if (length(well) == 0) return(picker_empty())
  out <- data.frame(well = pad_wells(well), label = label,
                    stringsAsFactors = FALSE)
  out[picker_plate_order(out$well), , drop = FALSE] |> picker_rownames()
}

#' Read assignments saved by a plate_picker
#'
#' Reads either a CSV downloaded with the widget's Download CSV button or a page
#' saved with its Save page button (the painting is kept in a
#' `<script type="application/json" id="mtpr-picker-state">` block).
#'
#' @param file Path to the CSV or HTML file.
#' @return A data frame with character columns `well` and `label`, in plate
#'   order.
#' @examples
#' f <- tempfile(fileext = ".html")
#' plate_picker_html(plate_picker(96, "Blank", value = c(A01 = "Blank")), f)
#' read_plate_picker(f)
#' @export
read_plate_picker <- function(file) {
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    stop("`file` must be the path of an existing file.", call. = FALSE)
  }
  txt <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (grepl("^\\s*<", txt)) {
    m <- regmatches(txt, regexec(
      "<script[^>]*id=\"mtpr-picker-state\"[^>]*>(.*?)</script>", txt, perl = TRUE))[[1]]
    if (length(m) < 2) {
      stop("No plate picker state found in `", file, "`.", call. = FALSE)
    }
    st <- jsonlite::fromJSON(m[2], simplifyVector = FALSE)
    return(plate_picker_value(list(well = st$assignments$well,
                                   label = st$assignments$label)))
  }
  d <- utils::read.csv(text = txt, colClasses = "character", stringsAsFactors = FALSE)
  names(d) <- tolower(names(d))
  if (!all(c("well", "label") %in% names(d))) {
    stop("The CSV needs the columns `well` and `label`.", call. = FALSE)
  }
  plate_picker_value(d[c("well", "label")])
}

#' Write a plate_picker as one self-contained HTML file
#'
#' Unlike `htmlwidgets::saveWidget(selfcontained = TRUE)` this does not need
#' pandoc: the widget's JavaScript and CSS and the 'htmlwidgets' runtime are
#' written into the page. The file opens offline without R. In it, Copy rows,
#' Download CSV and Save page are available (Save page downloads a copy of the
#' page with the current painting, so reopening it restores the painting;
#' [read_plate_picker()] reads it back).
#'
#' @param widget A [plate_picker()] widget.
#' @param file Output path.
#' @param title Page title; also names the downloaded files. Default `NULL`
#'   gives the title "Plate picker".
#' @param ... Used internally by 'htmlwidgets', which builds the widget's
#'   container through a function of this name; leave empty.
#' @return `file`, invisibly.
#' @examples
#' f <- tempfile(fileext = ".html")
#' plate_picker_html(plate_picker(384, c("Blank", "Standard")), f, title = "Run 12")
#' @export
plate_picker_html <- function(widget, file, title = NULL, ...) {
  # htmlwidgets looks for a function called `<widget name>_html` to build the
  # widget's container tag and calls it with `id`, `style` and `class`. This
  # name is also the file writer, so answer that call here.
  dots <- list(...)
  if (!is.null(dots$id) && !is.null(dots$style)) {
    return(htmltools::tags$div(id = dots$id, class = dots$class, style = dots$style))
  }
  if (!inherits(widget, "plate_picker")) {
    stop("`widget` must be a plate_picker() widget.", call. = FALSE)
  }
  if (!is.null(title) && (!is.character(title) || length(title) != 1 || is.na(title))) {
    stop("`title` must be a single string or NULL.", call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1) {
    stop("`file` must be a single path.", call. = FALSE)
  }
  widget$x$title <- title
  if (is.null(widget$x$exportButtons) || identical(widget$x$exportButtons, "auto")) {
    widget$x$exportButtons <- TRUE
  }
  widget$elementId <- "mtpr-picker"

  rendered <- htmltools::renderTags(htmltools::as.tags(widget))
  deps <- htmltools::resolveDependencies(rendered$dependencies)

  read_files <- function(dep, field) {
    if (is.null(dep[[field]])) return(character())
    vapply(dep[[field]], function(f) {
      path <- file.path(dep$src$file, f)
      if (is.null(dep$src$file) || !file.exists(path)) {
        stop("Cannot inline the dependency file ", f, " of ", dep$name, ".", call. = FALSE)
      }
      paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }, character(1))
  }
  # A closing script tag inside inlined code would end the block early.
  safe_js <- function(s) gsub("</script", "<\\\\/script", s, ignore.case = TRUE)
  css <- unlist(lapply(deps, read_files, field = "stylesheet"))
  js <- unlist(lapply(deps, read_files, field = "script"))

  page_title <- if (is.null(title)) "Plate picker" else title
  state <- list(
    version = 1L, plate = widget$x$plate, labels = I(widget$x$labels),
    title = title, assignments = widget$x$value
  )
  state_json <- gsub("<", "\\u003c", as.character(jsonlite::toJSON(
    state, auto_unbox = TRUE, null = "null")), fixed = TRUE)

  page <- c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    paste0("<title>", htmltools::htmlEscape(page_title), "</title>"),
    "<style>",
    "html,body{margin:0;background:#fff;color:#1c1f24;font-family:system-ui,-apple-system,'Segoe UI',Roboto,sans-serif}",
    "body{padding:16px;max-width:1200px;margin:0 auto}",
    "h1{font-size:1.15rem;font-weight:600;margin:0 0 12px}",
    if (identical(widget$x$theme, "dark"))
      "html,body{background:#14161a;color:#e7e9ee}",
    if (identical(widget$x$theme, "auto"))
      "@media (prefers-color-scheme:dark){html,body{background:#14161a;color:#e7e9ee}}",
    css,
    "</style>",
    unlist(lapply(js, function(s) paste0("<script>", safe_js(s), "</script>"))),
    "</head>",
    "<body>",
    if (!is.null(title)) paste0("<h1>", htmltools::htmlEscape(title), "</h1>"),
    as.character(rendered$html),
    paste0("<script type=\"application/json\" id=\"mtpr-picker-state\">", state_json, "</script>"),
    "</body>",
    "</html>"
  )
  writeLines(page, file, useBytes = TRUE)
  invisible(file)
}

# ---- internal helpers -------------------------------------------------------

# Fixed categorical palette (Okabe-Ito without black), in a fixed order.
.picker_palette <- c("#0072B2", "#E69F00", "#009E73", "#D55E00",
                     "#56B4E9", "#CC79A7", "#F0E442", "#7F7F7F")

picker_empty <- function() {
  data.frame(well = character(), label = character(), stringsAsFactors = FALSE)
}

picker_rownames <- function(d) {
  rownames(d) <- NULL
  d
}

picker_check_labels <- function(labels) {
  if (!is.character(labels) || length(labels) < 1 || anyNA(labels) ||
      any(!nzchar(labels)) || anyDuplicated(labels)) {
    stop("`labels` must be a character vector of unique, non-empty names.", call. = FALSE)
  }
}

# Colours as #RRGGBB, so they can be put into the page safely.
picker_hex <- function(x, what) {
  rgb <- tryCatch(grDevices::col2rgb(x), error = function(e) {
    stop("`", what, "` contains a value that is not a valid colour.", call. = FALSE)
  })
  grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255)
}

picker_colours <- function(labels, colours) {
  if (is.null(colours)) {
    if (length(labels) > length(.picker_palette)) {
      stop("The default palette has ", length(.picker_palette), " colours but there are ",
           length(labels), " labels. Use fewer labels or give `colours` for every label.",
           call. = FALSE)
    }
    return(stats::setNames(.picker_palette[seq_along(labels)], labels))
  }
  if (is.null(names(colours)) || !all(labels %in% names(colours))) {
    stop("`colours` must be a named vector with a colour for every label.", call. = FALSE)
  }
  stats::setNames(picker_hex(as.character(colours[labels]), "colours"), labels)
}

# Wells normalised, and (when the plate is known) checked against it.
picker_normalise_wells <- function(x, plate, what) {
  if (length(x) == 0) return(character())
  if (!is.character(x) || anyNA(x)) {
    stop("`", what, "` must be a character vector of wells.", call. = FALSE)
  }
  w <- pad_wells(x)
  bad <- !grepl("^[A-Z]+[0-9]{2,}$", w)
  if (!is.null(plate)) {
    info <- platesize_check(plate)
    row <- sub("[0-9]+$", "", w)
    col <- suppressWarnings(as.integer(sub("^[A-Z]+", "", w)))
    bad <- bad | !row %in% info$rows | is.na(col) | col < 1 | col > info$colmax
  }
  if (any(bad)) {
    stop("`", what, "` has wells that are not on the plate: ",
         paste(utils::head(unique(x[bad]), 5), collapse = ", "), ".", call. = FALSE)
  }
  unique(w)
}

# Order of wells on the plate: by row (A, B, ..., Z, AA, ...), then column.
picker_plate_order <- function(wells) {
  row <- sub("[0-9]+$", "", wells)
  col <- suppressWarnings(as.integer(sub("^[A-Z]+", "", wells)))
  order(nchar(row), row, col)
}

# `value` as a data frame of well and label.
picker_normalise_value <- function(value, plate, labels, multiple, plain) {
  if (is.null(value) || length(value) == 0) return(picker_empty())
  if (is.data.frame(value)) {
    if (!"well" %in% names(value)) {
      stop("`value` needs a `well` column.", call. = FALSE)
    }
    well <- as.character(value$well)
    if ("label" %in% names(value)) {
      label <- as.character(value$label)
    } else if (plain) {
      label <- rep("selected", length(well))
    } else {
      stop("`value` needs `well` and `label` columns.", call. = FALSE)
    }
  } else if (is.character(value) && !is.null(names(value))) {
    well <- names(value)
    label <- unname(value)
  } else if (is.character(value) && plain) {
    well <- value
    label <- rep("selected", length(well))
  } else {
    stop("`value` must be a data frame with `well` and `label`, or a named ",
         "character vector (well = label).", call. = FALSE)
  }
  if (anyNA(well) || anyNA(label)) stop("`value` must not contain NA.", call. = FALSE)
  well <- picker_normalise_wells_keep(well, plate, "value")
  if (!is.null(labels) && !all(label %in% labels)) {
    stop("`value` has labels that are not in `labels`: ",
         paste(utils::head(unique(setdiff(label, labels)), 5), collapse = ", "), ".",
         call. = FALSE)
  }
  out <- data.frame(well = well, label = label, stringsAsFactors = FALSE)
  out <- out[!duplicated(out), , drop = FALSE]
  if (!multiple && anyDuplicated(out$well)) {
    stop("`value` gives a well more than one label; set `multiple = TRUE`.", call. = FALSE)
  }
  picker_rownames(out[picker_plate_order(out$well), , drop = FALSE])
}

# Like picker_normalise_wells() but keeps duplicates and order.
picker_normalise_wells_keep <- function(x, plate, what) {
  w <- pad_wells(x)
  picker_normalise_wells(unique(x), plate, what)
  w
}

# Column-wise lists keep length-one values as arrays in the JSON.
picker_value_list <- function(d) {
  list(well = I(d$well), label = I(d$label))
}

picker_normalise_background <- function(background, plate) {
  if (is.null(background)) return(NULL)
  if (!is.data.frame(background) || !all(c("well", "fill") %in% names(background))) {
    stop("`background` must be a data frame with columns `well` and `fill`.", call. = FALSE)
  }
  well <- picker_normalise_wells_keep(as.character(background$well), plate, "background")
  if (anyDuplicated(well)) stop("`background` has a well more than once.", call. = FALSE)
  out <- list(well = I(well))
  out$fill <- if (length(well)) I(picker_hex(as.character(background$fill), "background$fill")) else I(character())
  if ("text" %in% names(background)) out$text <- I(as.character(background$text))
  if ("tooltip" %in% names(background)) out$tooltip <- I(as.character(background$tooltip))
  out
}

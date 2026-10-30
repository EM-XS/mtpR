#' Reserve wells for controls
#'
#' Reserves wells on a plate before reactions are placed. The result is the
#' whole plate, one row per well, with a `Role` column: `"control"` for the
#' reserved wells and `"empty"` for the rest. Pass it to
#' [replicate_reactions()] as `reserved` so that reactions flow around the
#' controls, then use [plate_roles()] to mark the wells that reactions took.
#'
#' A count (`n`) is always a number of wells, and the place is given
#' separately. Choose where the controls go with `method`:
#'
#' * `"fixed"`: the wells listed in `wells`. Single wells (`"A01"` or `"A1"`)
#'   and rectangular ranges (`"A01:H01"`, `"A01:B12"`) can be mixed.
#' * `"lines"`: whole columns (or rows, with `by = "row"`), e.g.
#'   `lines = c(1, 24)`. Rows can be given as letters (`c("A", "P")`) or numbers.
#' * `"edge"`: the outer ring of the plate; `depth = 2` reserves the outer two
#'   rings, and so on.
#' * `"spread"`: `n` wells spread evenly (see below).
#' * `"random"`: `n` wells drawn at random; the same `seed` gives the same wells.
#' * `"quadrant"`: every well in quadrant(s) `quadrant` (1-4) of the 2x2
#'   squares that tile the plate; see [well_quadrant()]. Several quadrants can
#'   be given.
#' * `"every"`: deprecated, and will be removed in mtpR 0.5.0. Use `"lines"` for
#'   whole columns or rows, or `"spread"` for `n` wells spread out.
#'
#' Calls can be chained: give an earlier result as `plate` to reserve more
#' wells. If a later call takes a well that an earlier call already reserved,
#' a warning says so and the later call wins (its `name` is kept).
#'
#' @section Spreading controls among reactions:
#' `"spread"` puts `n` controls at evenly spaced places among the reactions, in
#' fill order. Take the block of `n + among` free wells that starts at `start`
#' (free means not already a control; the plate is read in the direction `wise`).
#' Control number `k` (1 to `n`) is the one that follows the
#' `floor(k * among / n)`-th reaction. So 8 controls among 40 reactions fall
#' after every 5th reaction, and the block ends on a control:
#'
#' ```
#' R R R R R C R R R R R C ...   (5 reactions, then a control)
#' ```
#'
#' With `among = NULL`, the controls are spread over all free wells from `start`
#' to the end of the plate (default: the whole plate). To place controls among
#' reactions in one go, use `controls_spread` in [replicate_reactions()], which
#' fills in `among` and `start` for you.
#'
#' @param plate Plate size (see [platesize_check()]), or the result of an
#'   earlier `place_controls()` call to add more controls to.
#' @param method `"fixed"`, `"lines"`, `"edge"`, `"spread"`, `"random"`,
#'   `"quadrant"` or (deprecated) `"every"`.
#' @param wells Well labels and ranges for `method = "fixed"`.
#' @param lines Columns (or rows) for `method = "lines"`: numbers, or row letters
#'   when `by = "row"`.
#' @param every Spacing for `method = "every"`: 2 reserves every second column.
#' @param by `"column"` or `"row"`, for `method = "lines"` and `"every"`.
#' @param offset First column (row) reserved by `method = "every"`.
#' @param depth Number of rings for `method = "edge"` (1 = outermost ring only).
#' @param n Number of wells for `method = "spread"` and `"random"`.
#' @param among For `method = "spread"`: the number of reaction wells the
#'   controls are spread among. `NULL` spreads over the whole plate from `start`.
#' @param start For `method = "spread"`: the well (label or position number in
#'   fill order) where the block starts. Default: the first well.
#' @param quadrant Quadrant(s), 1 to 4, for `method = "quadrant"`.
#' @param seed Random seed for `method = "random"`. If `NULL` one is drawn; the
#'   seed used is stored in `attr(result, "seed")`.
#' @param name Name of the control, stored in the `Control` column. One value,
#'   or one per well for `method = "fixed"`.
#' @param wise `"row"` or `"col"`: the direction `Well.Index` runs along the
#'   plate, which is also the fill direction for `method = "spread"`.
#'
#' @return A data frame of class `mtpr_plate` with one row per well and the
#'   columns `Well`, `Row`, `Column`, `Well.Index`, `Role` and `Control`
#'   (`NA` for wells that are not controls).
#'
#' @examples
#' place_controls(96, wells = c("A01", "A12", "H01", "H12"))
#' place_controls(96, "fixed", wells = c("A01:H01", "A12"))
#' edge <- place_controls(96, "edge", name = "buffer")
#' table(edge$Role)
#' place_controls(384, "edge", depth = 2)
#' place_controls(384, "lines", lines = c(1, 24), by = "column")
#' place_controls(96, "lines", lines = c("A", "H"), by = "row")
#' place_controls(384, "quadrant", quadrant = 4)
#' place_controls(96, "random", n = 8, seed = 42)
#'
#' # 8 controls among 40 reactions: one after every 5th reaction
#' ctl <- place_controls(96, "spread", n = 8, among = 40)
#' ctl$Well[ctl$Role == "control"]
#' @export
place_controls <- function(plate = 96,
                           method = c("fixed", "lines", "edge", "spread", "random",
                                      "quadrant", "every"),
                           wells = NULL,
                           lines = NULL,
                           every = NULL,
                           by = c("column", "row"),
                           offset = 1,
                           depth = 1,
                           n = NULL,
                           among = NULL,
                           start = 1,
                           quadrant = NULL,
                           seed = NULL,
                           name = "control",
                           wise = "row") {
  if (missing(method) && !is.null(wells)) method <- "fixed"
  method <- match.arg(method)
  by <- match.arg(by)
  if (!wise %in% c("row", "col")) {
    stop("The 'wise' parameter must be either 'row' or 'col'.", call. = FALSE)
  }

  if (inherits(plate, "mtpr_plate")) {
    map <- plate
    size <- attr(plate, "plate")
    wise <- attr(plate, "wise")
  } else {
    size <- plate
    map <- NULL
  }
  info <- platesize_check(size)
  n_wells <- info$rowmax * info$colmax

  if (is.null(map)) map <- empty_plate_map(size, wise)

  chosen <- switch(
    method,
    fixed = {
      if (is.null(wells) || length(wells) == 0) {
        stop("`wells` is required for method = \"fixed\".", call. = FALSE)
      }
      expand_wells(wells, size)
    },
    lines = {
      if (is.null(lines) || length(lines) == 0) {
        stop("`lines` is required for method = \"lines\".", call. = FALSE)
      }
      nums <- resolve_lines(lines, by, info)
      map$Well[(if (by == "column") map$Column else map$Row) %in% nums]
    },
    edge = {
      if (length(depth) != 1 || is.na(depth) || depth < 1 || depth != round(depth)) {
        stop("`depth` must be a whole number of at least 1.", call. = FALSE)
      }
      map$Well[map$Row <= depth | map$Row > info$rowmax - depth |
                 map$Column <= depth | map$Column > info$colmax - depth]
    },
    spread = {
      if (is.null(n) || length(n) != 1 || is.na(n) || n < 1 || n != round(n)) {
        stop("`n` must be a whole number of at least 1 for method = \"spread\".", call. = FALSE)
      }
      if (!is.null(among) &&
          (length(among) != 1 || is.na(among) || among < 0 || among != round(among))) {
        stop("`among` must be a whole number of at least 0.", call. = FALSE)
      }
      free <- map[map$Role != "control", , drop = FALSE]
      free <- free[order(free$Well.Index), , drop = FALSE]
      start_idx <- resolve_start(start, size, wise)
      free <- free[free$Well.Index >= start_idx, , drop = FALSE]
      if (is.null(among)) among <- nrow(free) - n
      if (among < 0 || n + among > nrow(free)) {
        stop("Cannot spread ", n, " controls among ", among, " reactions: only ",
             nrow(free), " free wells from the start well on.", call. = FALSE)
      }
      free$Well[spread_index(n, among)]
    },
    random = {
      if (is.null(n) || length(n) != 1 || n < 1 || n != round(n)) {
        stop("`n` must be a whole number of at least 1 for method = \"random\".", call. = FALSE)
      }
      free <- map$Well[map$Role != "control"]
      if (n > length(free)) {
        stop("Cannot reserve ", n, " wells: only ", length(free), " are free.", call. = FALSE)
      }
      if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
      free[sample_with_seed(length(free), seed)[seq_len(n)]]
    },
    quadrant = {
      if (is.null(quadrant) || length(quadrant) == 0 || anyNA(quadrant) ||
          !all(quadrant %in% 1:4)) {
        stop("`quadrant` must be one or more of 1, 2, 3, 4 for method = \"quadrant\".",
             call. = FALSE)
      }
      check_quadrant_plate(size)
      q <- 1 + (map$Row %% 2 == 0) * 2 + (map$Column %% 2 == 0)
      map$Well[q %in% quadrant]
    },
    every = {
      lifecycle_warn_every()
      if (is.null(every) || length(every) != 1 || every < 1 || every != round(every)) {
        stop("`every` must be a whole number of at least 1.", call. = FALSE)
      }
      line <- if (by == "column") map$Column else map$Row
      top <- if (by == "column") info$colmax else info$rowmax
      if (offset < 1 || offset > top) {
        stop("`offset` must be between 1 and ", top, " for by = \"", by, "\".", call. = FALSE)
      }
      map$Well[line %in% seq(offset, top, by = every)]
    }
  )

  if (length(name) != 1 && length(name) != length(chosen)) {
    stop("`name` must be one value or one per reserved well.", call. = FALSE)
  }
  hit <- match(chosen, map$Well)
  again <- unique(map$Well[hit[map$Role[hit] == "control"]])
  if (length(again)) {
    warning(length(again), " well", if (length(again) > 1) "s were" else " was",
            " already reserved by an earlier call and ", if (length(again) > 1) "are" else "is",
            " now taken by this one: ", paste(utils::head(again, 5), collapse = ", "),
            if (length(again) > 5) ", ...", ".", call. = FALSE)
  }
  map$Role[hit] <- "control"
  map$Control[hit] <- rep_len(as.character(name), length(chosen))

  if (method == "random") attr(map, "seed") <- seed
  attr(map, "plate") <- size
  attr(map, "wise") <- wise
  class(map) <- unique(c("mtpr_plate", class(map)))
  map
}

# A plate with every well empty.
empty_plate_map <- function(size, wise = "row") {
  info <- platesize_check(size)
  idx <- seq_len(info$rowmax * info$colmax)
  map <- data.frame(
    Well = convert_well(idx, "to_well", plate = size, wise = wise),
    stringsAsFactors = FALSE
  )
  map$Row <- match(sub("[0-9]+$", "", map$Well), info$rows)
  map$Column <- as.numeric(sub("^[A-Z]+", "", map$Well))
  map$Well.Index <- idx
  map$Role <- "empty"
  map$Control <- NA_character_
  map <- map[order(map$Row, map$Column), ]
  rownames(map) <- NULL
  attr(map, "plate") <- size
  attr(map, "wise") <- wise
  class(map) <- unique(c("mtpr_plate", class(map)))
  map
}

# Single wells ("A1", "A01") and rectangular ranges ("A01:B12") -> well labels.
expand_wells <- function(wells, size) {
  info <- platesize_check(size)
  w <- gsub("\\s+", "", as.character(wells))
  to_rc <- function(x) {
    num <- convert_well(x, "to_num", plate = size, wise = "row")
    list(row = (num - 1) %/% info$colmax + 1, col = (num - 1) %% info$colmax + 1)
  }
  out <- lapply(w, function(x) {
    parts <- strsplit(x, ":", fixed = TRUE)[[1]]
    if (length(parts) > 2 || length(parts) == 0 || any(parts == "") || grepl(":$", x)) {
      stop("Cannot read '", x, "' as a well or a range like \"A01:H01\".", call. = FALSE)
    }
    if (length(parts) == 1) {
      a <- to_rc(parts)
      return(convert_well((a$row - 1) * info$colmax + a$col, "to_well", plate = size, wise = "row"))
    }
    a <- to_rc(parts[1])
    b <- to_rc(parts[2])
    rows <- seq(min(a$row, b$row), max(a$row, b$row))
    cols <- seq(min(a$col, b$col), max(a$col, b$col))
    grid <- expand.grid(col = cols, row = rows)  # row by row
    convert_well((grid$row - 1) * info$colmax + grid$col, "to_well", plate = size, wise = "row")
  })
  unlist(out, use.names = FALSE)
}

# Column numbers or row numbers (letters or numbers) -> line numbers.
resolve_lines <- function(lines, by, info) {
  top <- if (by == "column") info$colmax else info$rowmax
  if (is.character(lines)) {
    lines <- toupper(trimws(lines))
    as_num <- suppressWarnings(as.numeric(lines))
    if (by == "row") {
      num <- ifelse(!is.na(as_num), as_num, match(lines, info$rows))
    } else {
      num <- as_num
    }
  } else if (is.numeric(lines)) {
    num <- lines
  } else {
    stop("`lines` must be numbers or, for rows, row letters.", call. = FALSE)
  }
  if (anyNA(num) || any(num != round(num)) || any(num < 1 | num > top)) {
    stop("`lines` must be ", if (by == "column") "columns" else "rows (letters or numbers)",
         " between ", if (by == "column") "1" else "A (1)", " and ",
         if (by == "column") top else paste0(info$rows[top], " (", top, ")"), ".", call. = FALSE)
  }
  unique(num)
}

# Start well (label or position number) -> position number in fill order.
resolve_start <- function(start, size, wise) {
  if (is.character(start)) {
    return(convert_well(start, "to_num", plate = size, wise = wise))
  }
  info <- platesize_check(size)
  if (length(start) != 1 || is.na(start) || start < 1 || start > info$rowmax * info$colmax) {
    stop("`start` must be a well label or a position on the plate.", call. = FALSE)
  }
  start
}

# Indices (into a block of n + among wells) of n controls spread evenly: control
# k follows the floor(k * among / n)-th reaction.
spread_index <- function(n, among) {
  k <- seq_len(n)
  k + floor(k * among / n)
}

lifecycle_warn_every <- function() {
  rlang::warn(
    c("`place_controls(method = \"every\")` is deprecated and will be removed in mtpR 0.5.0.",
      i = "Use method = \"lines\" for whole columns or rows (e.g. lines = c(1, 5, 9)),",
      i = "or method = \"spread\" for n wells spread out."),
    .frequency = "once", .frequency_id = "mtpR_place_controls_every"
  )
}

#' Mark the wells that reactions occupy
#'
#' Updates the `Role` column of a plate from [place_controls()]: wells given in
#' `reactions` become `"reaction"`; the rest stay `"control"` or `"empty"`. Use
#' it after [replicate_reactions()] and [convert_well()] have given each
#' reaction a well, so the plate can be drawn by role or counted.
#'
#' @param plate A plate from [place_controls()].
#' @param reactions A vector of well labels, or a data frame with a column of
#'   well labels named by `well_id`.
#' @param well_id Column of `reactions` with the well labels, when `reactions`
#'   is a data frame.
#'
#' @return The plate, with `Role` updated.
#' @examples
#' ctl <- place_controls(96, wells = c("A01", "H12"))
#' plate_roles(ctl, c("A02", "A03"))
#' @export
plate_roles <- function(plate, reactions, well_id = "Well.Position") {
  if (!inherits(plate, "mtpr_plate")) {
    stop("`plate` must come from place_controls().", call. = FALSE)
  }
  if (is.data.frame(reactions)) {
    if (!well_id %in% names(reactions)) {
      stop("Column '", well_id, "' not found in `reactions`.", call. = FALSE)
    }
    reactions <- reactions[[well_id]]
  }
  size <- attr(plate, "plate")
  w <- clean_wells(as.character(reactions), plate = size)
  clash <- plate$Well[plate$Role == "control" & plate$Well %in% w]
  if (length(clash)) {
    stop("Reactions are placed on reserved wells: ",
         paste(utils::head(clash, 5), collapse = ", "), ".", call. = FALSE)
  }
  plate$Role[plate$Well %in% w] <- "reaction"
  plate
}

# Reserved wells as position numbers on one plate, plus the plate size.
reserved_positions <- function(reserved, plate = NULL, wise = "row") {
  if (inherits(reserved, "mtpr_plate")) {
    size <- attr(reserved, "plate")
    if (!is.null(plate) && plate != size) {
      stop("`plate` (", plate, ") does not match the reserved plate (", size, ").",
           call. = FALSE)
    }
    wells <- reserved$Well[reserved$Role == "control"]
    pos <- if (length(wells)) convert_well(wells, "to_num", plate = size, wise = wise) else numeric(0)
    return(list(positions = pos, plate_size = size))
  }
  if (is.null(plate)) {
    stop("`plate` is required when `reserved` is not from place_controls().", call. = FALSE)
  }
  size <- platesize_check(plate)
  size <- size$rowmax * size$colmax
  if (is.character(reserved)) {
    pos <- convert_well(reserved, "to_num", plate = plate, wise = wise)
  } else if (is.numeric(reserved)) {
    if (any(reserved < 1 | reserved > size)) {
      stop("Reserved positions must be between 1 and ", size, ".", call. = FALSE)
    }
    pos <- reserved
  } else {
    stop("`reserved` must be a plate from place_controls(), well labels or positions.",
         call. = FALSE)
  }
  list(positions = unique(pos), plate_size = size)
}

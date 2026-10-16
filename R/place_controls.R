#' Reserve wells for controls
#'
#' Reserves wells on a plate before reactions are placed. The result is the
#' whole plate, one row per well, with a `Role` column: `"control"` for the
#' reserved wells and `"empty"` for the rest. Pass it to
#' [replicate_reactions()] as `reserved` so that reactions flow around the
#' controls, then use [plate_roles()] to mark the wells that reactions took.
#'
#' Choose where the controls go with `method`:
#'
#' * `"fixed"`: the wells listed in `wells`, e.g. `c("A01", "H12")`.
#' * `"edge"`: every well on the outer ring of the plate.
#' * `"every"`: every `every`-th column (or row, with `by = "row"`), starting at
#'   column (row) `offset`. All wells of the chosen columns are reserved.
#' * `"random"`: `n` wells drawn at random; the same `seed` gives the same wells.
#'
#' Calls can be chained: give an earlier result as `plate` to reserve more wells.
#'
#' @param plate Plate size (see [platesize_check()]), or the result of an
#'   earlier `place_controls()` call to add more controls to.
#' @param method `"fixed"`, `"edge"`, `"every"` or `"random"`.
#' @param wells Well labels for `method = "fixed"`.
#' @param every Spacing for `method = "every"`: 2 reserves every second column.
#' @param by `"column"` or `"row"`, for `method = "every"`.
#' @param offset First column (row) reserved by `method = "every"`.
#' @param n Number of wells for `method = "random"`.
#' @param seed Random seed for `method = "random"`. If `NULL` one is drawn; the
#'   seed used is stored in `attr(result, "seed")`.
#' @param name Name of the control, stored in the `Control` column. One value,
#'   or one per well for `method = "fixed"`.
#' @param wise `"row"` or `"col"`: the direction `Well.Index` runs along the plate.
#'
#' @return A data frame of class `mtpr_plate` with one row per well and the
#'   columns `Well`, `Row`, `Column`, `Well.Index`, `Role` and `Control`
#'   (`NA` for wells that are not controls).
#'
#' @examples
#' place_controls(96, wells = c("A01", "A12", "H01", "H12"))
#' edge <- place_controls(96, "edge", name = "buffer")
#' table(edge$Role)
#' place_controls(384, "every", every = 6, by = "column")
#' place_controls(96, "random", n = 8, seed = 42)
#' @export
place_controls <- function(plate = 96,
                           method = c("fixed", "edge", "every", "random"),
                           wells = NULL,
                           every = NULL,
                           by = c("column", "row"),
                           offset = 1,
                           n = NULL,
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

  if (is.null(map)) {
    idx <- seq_len(n_wells)
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
  }

  chosen <- switch(
    method,
    fixed = {
      if (is.null(wells) || length(wells) == 0) {
        stop("`wells` is required for method = \"fixed\".", call. = FALSE)
      }
      w <- toupper(as.character(wells))
      # accept A1 as well as A01
      num <- convert_well(w, "to_num", plate = size, wise = "row")
      convert_well(num, "to_well", plate = size, wise = "row")
    },
    edge = map$Well[map$Row %in% c(1, info$rowmax) | map$Column %in% c(1, info$colmax)],
    every = {
      if (is.null(every) || length(every) != 1 || every < 1 || every != round(every)) {
        stop("`every` must be a whole number of at least 1.", call. = FALSE)
      }
      line <- if (by == "column") map$Column else map$Row
      top <- if (by == "column") info$colmax else info$rowmax
      if (offset < 1 || offset > top) {
        stop("`offset` must be between 1 and ", top, " for by = \"", by, "\".", call. = FALSE)
      }
      map$Well[line %in% seq(offset, top, by = every)]
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
    }
  )

  if (length(name) != 1 && length(name) != length(chosen)) {
    stop("`name` must be one value or one per reserved well.", call. = FALSE)
  }
  hit <- match(chosen, map$Well)
  map$Role[hit] <- "control"
  map$Control[hit] <- rep_len(as.character(name), length(chosen))

  if (method == "random") attr(map, "seed") <- seed
  attr(map, "plate") <- size
  attr(map, "wise") <- wise
  class(map) <- unique(c("mtpr_plate", class(map)))
  map
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
  w <- toupper(as.character(reactions))
  num <- convert_well(w, "to_num", plate = size, wise = "row")
  w <- convert_well(num, "to_well", plate = size, wise = "row")
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
    pos <- convert_well(toupper(reserved), "to_num", plate = plate, wise = wise)
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

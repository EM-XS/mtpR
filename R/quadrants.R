#' Quadrant (1-4) of a well
#'
#' The plate is tiled into 2x2 squares on a fixed grid starting at A01 (rows
#' A-B, C-D, ...; columns 1-2, 3-4, ...). Inside each square the wells are
#' numbered
#'
#' ```
#' 1 2
#' 3 4
#' ```
#'
#' and "quadrant k" is every well in position k. On a 384-well plate quadrant 1
#' (odd rows, odd columns) is a 96-well grid; on a 96-well plate each quadrant
#' is 4 x 6 = 24 wells. This is the layout used by `pattern = "quadrants"` in
#' [replicate_reactions()] and `method = "quadrant"` in [place_controls()].
#'
#' Needs a plate with an even number of rows and columns (24, 48, 96, 384 and
#' 1536 wells).
#'
#' @param well Well labels, e.g. `"A01"` or `"A1"`.
#' @param plate Plate size (see [platesize_check()]).
#'
#' @return An integer vector, 1 to 4, one per well.
#' @examples
#' well_quadrant(c("A01", "A02", "B01", "B02", "A03"), plate = 96)
#' @export
well_quadrant <- function(well, plate = 96) {
  check_quadrant_plate(plate)
  num <- convert_well(toupper(as.character(well)), "to_num", plate = plate, wise = "row")
  info <- platesize_check(plate)
  row <- (num - 1) %/% info$colmax + 1
  col <- (num - 1) %% info$colmax + 1
  as.integer(1 + (row %% 2 == 0) * 2 + (col %% 2 == 0))
}

# Quadrants need whole 2x2 squares.
check_quadrant_plate <- function(plate) {
  info <- platesize_check(plate)
  if (info$rowmax %% 2 != 0 || info$colmax %% 2 != 0) {
    stop("Quadrants need a plate with an even number of rows and columns; a ",
         plate, "-well plate has ", info$rowmax, " x ", info$colmax, ".", call. = FALSE)
  }
  invisible(info)
}

# Position (in fill order, `wise`) of well k (1-4) of square s (1-based, counted
# in fill order and running on over further plates).
square_positions <- function(square, k, plate, wise) {
  info <- platesize_check(plate)
  size <- info$rowmax * info$colmax
  nsq_r <- info$rowmax / 2
  nsq_c <- info$colmax / 2
  per_plate <- nsq_r * nsq_c
  plate_no <- (square - 1) %/% per_plate
  s <- (square - 1) %% per_plate
  if (wise == "row") {
    sr <- s %/% nsq_c
    sc <- s %% nsq_c
  } else {
    sc <- s %/% nsq_r
    sr <- s %% nsq_r
  }
  row <- 2 * sr + 1 + (k > 2)
  col <- 2 * sc + 1 + (k %% 2 == 0)
  pos <- if (wise == "row") (row - 1) * info$colmax + col else (col - 1) * info$rowmax + row
  pos + plate_no * size
}

# Square (1-based, running over plates) that contains `position`, and whether
# the position is that square's top-left well.
square_of_position <- function(position, plate, wise) {
  info <- platesize_check(plate)
  size <- info$rowmax * info$colmax
  per_plate <- size / 4
  plate_no <- (position - 1) %/% size
  p <- (position - 1) %% size
  if (wise == "row") {
    row <- p %/% info$colmax + 1
    col <- p %% info$colmax + 1
  } else {
    col <- p %/% info$rowmax + 1
    row <- p %% info$rowmax + 1
  }
  sr <- (row - 1) %/% 2
  sc <- (col - 1) %/% 2
  s <- if (wise == "row") sr * (info$colmax / 2) + sc else sc * (info$rowmax / 2) + sr
  list(square = plate_no * per_plate + s + 1,
       top_left = row %% 2 == 1 && col %% 2 == 1)
}

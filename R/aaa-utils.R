#' Generate spreadsheet-style row labels
#'
#' Returns the first `n` spreadsheet-style column labels (`A`, `B`, ..., `Z`,
#' `AA`, `AB`, ...), used throughout the package to name microplate rows. This
#' keeps row naming consistent between [platesize_check()], [convert_well()] and
#' [to_rows_columns()], including for 1536-well plates (32 rows: `A`..`Z`,
#' `AA`..`AF`).
#'
#' @param n Number of labels to generate (positive integer).
#'
#' @return A character vector of length `n`.
#' @keywords internal
#' @noRd
plate_row_labels <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("n must be a single positive integer.", call. = FALSE)
  }
  n <- as.integer(n)
  single <- LETTERS
  double <- paste0(rep(LETTERS, each = 26L), LETTERS)
  labels <- c(single, double)
  if (n > length(labels)) {
    stop("plate_row_labels() supports at most ", length(labels), " rows.", call. = FALSE)
  }
  labels[seq_len(n)]
}

#' Well labels for a plate, in fill order
#'
#' Helper for the plate designer UI: the full set of well labels for `plate`,
#' ordered by fill direction so the "starting well" dropdown lists them the way
#' the plate will actually be filled.
#'
#' @param plate Plate size (see [platesize_check()]).
#' @param wise `"row"` or `"col"`.
#'
#' @return A character vector of every well label on the plate.
#' @keywords internal
#' @noRd
start_well_choices <- function(plate, wise = "row") {
  plate <- as.numeric(plate)
  convert_well(seq_len(plate), direction = "to_well", plate = plate, wise = wise)
}

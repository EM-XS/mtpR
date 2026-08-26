#' Check allowable plate size and return dimensions
#'
#' This function validates if the entered plate size is supported and returns the
#' corresponding rows and columns dimensions.
#'
#' @param plate_size Numeric, the number of wells in the plate.
#'
#' @return A list containing the information of plate dimensions:
#'           - rows: A character vector of row identifiers.
#'           - columns: A numeric vector of column numbers.
#'           - rowmax: The total number of rows in the plate.
#'           - colmax: The total number of columns in the plate.
#' @export
platesize_check <- function(plate_size) {
  if (!plate_size %in% c(6, 12, 24, 48, 96, 384, 1536)) {
    stop("Unsupported plate size: ", plate_size,
         ". Allowed sizes are 6, 12, 24, 48, 96, 384, and 1536.", call. = FALSE)
  }

  # Define the row and column maximum counts for standard plate sizes
  plate_dimensions <- list(
    "6" = list(rowmax = 2, colmax = 3),
    "12" = list(rowmax = 3, colmax = 4),
    "24" = list(rowmax = 4, colmax = 6),
    "48" = list(rowmax = 6, colmax = 8),
    "96" = list(rowmax = 8, colmax = 12),
    "384" = list(rowmax = 16, colmax = 24),
    "1536" = list(rowmax = 32, colmax = 48)
  )

  plate_info <- plate_dimensions[[as.character(plate_size)]]

  # Spreadsheet-style row labels (A..Z, AA..AF, ...) so that plates with more
  # than 26 rows (e.g. 1536-well) are named consistently everywhere.
  rows <- plate_row_labels(plate_info$rowmax)

  columns <- 1:plate_info$colmax
  return(list("rows" = rows, "columns" = columns, "rowmax" = plate_info$rowmax, "colmax" = plate_info$colmax))
}

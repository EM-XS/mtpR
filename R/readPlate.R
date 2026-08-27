#' Read plate-shaped data into long format
#'
#' Reshape a rectangular, plate-shaped table (a leading column of row letters
#' followed by one column per plate column) into a tidy two-column data frame of
#' well labels and values.
#'
#' The first column of `dat` is treated as the row labels (`A`, `B`, ...). Every
#' remaining column is treated as a plate column; its name only needs to
#' *contain* the column number (so `"1"`, `"col1"` and `"X1"` all work). The
#' number of data columns must match the chosen plate size.
#'
#' @param dat A data frame of plate-shaped data: column 1 holds the row letters,
#'   the remaining columns hold one plate column each.
#' @param plate.size The plate size for this set of wells (see [platesize_check()]).
#' @param value_name Name to give the values column in the result.
#'
#' @return A tibble with two columns: `well` (e.g. `"A1"`) and the values column
#'   named by `value_name` (default `"value"`).
#'
#' @examples
#' sample_plate <- readRDS(
#'   system.file("extdata", "SamplePlateShapedData.rds", package = "mtpR")
#' )
#' readPlate(sample_plate, plate.size = 96)
#'
#' @export
readPlate <- function(dat, plate.size = 96, value_name = "value") {

  if (!is.data.frame(dat)) {
    stop("`dat` must be a data frame of plate-shaped data.", call. = FALSE)
  }
  if (ncol(dat) < 2) {
    stop("`dat` must have a row-label column plus at least one data column.", call. = FALSE)
  }
  if (!is.character(value_name) || length(value_name) != 1) {
    stop("`value_name` must be a single character string.", call. = FALSE)
  }

  plate_info <- platesize_check(plate.size)

  n_data_cols <- ncol(dat) - 1L
  if (n_data_cols != plate_info$colmax) {
    stop("`dat` has ", n_data_cols, " data column(s) after the row-label column, ",
         "but a ", plate.size, "-well plate has ", plate_info$colmax, " columns.",
         call. = FALSE)
  }

  row_label_col <- names(dat)[1]
  data_cols <- names(dat)[-1]

  row_labels <- toupper(trimws(as.character(dat[[row_label_col]])))
  if (!all(row_labels %in% plate_info$rows)) {
    bad <- unique(row_labels[!row_labels %in% plate_info$rows])
    stop("Unexpected row label(s) in the first column: ",
         paste(bad, collapse = ", "),
         ". A ", plate.size, "-well plate has rows ",
         plate_info$rows[1], "-", plate_info$rows[plate_info$rowmax], ".",
         call. = FALSE)
  }

  long <- tidyr::pivot_longer(
    dat,
    cols = tidyr::all_of(data_cols),
    names_to = ".plate_column",
    values_to = value_name
  )

  col_numbers <- as.integer(stringr::str_extract(long[[".plate_column"]], "[0-9]+"))
  if (any(is.na(col_numbers)) || any(col_numbers < 1) || any(col_numbers > plate_info$colmax)) {
    stop("Data column names must contain a number in the range 1-", plate_info$colmax, ".",
         call. = FALSE)
  }

  long[["well"]] <- paste0(
    toupper(trimws(as.character(long[[row_label_col]]))),
    col_numbers
  )

  long[c("well", value_name)]
}

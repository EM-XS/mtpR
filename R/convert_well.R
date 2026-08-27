#' Convert between well labels and numeric indices
#'
#' @param input A vector of well labels (as character) or numeric indices (as numeric) to be converted.
#' @param direction Character; specifies the conversion direction, either "to_num" or "to_well".
#' @param plate The size of the well plate (96, 384, or 1536 are supported).
#' @param wise Character; specifies direction the plate is filled ("row" or "col" wise).
#'             Applicable for both "to_num" and "to_well" directions.
#' @return A vector of converted well labels or numeric indices.
#' @export
convert_well <- function(input, direction, plate = 96, wise = "row") {
  # Check the validity of the wise parameter
  if (!wise %in% c("row", "col")) {
    stop("The 'wise' parameter must be either 'row' or 'col'.")
  }

  # Fetch plate size information
  plate_size <- platesize_check(plate)

  # Convert well label to numeric index
  if (direction == "to_num") {
    # Validate the input is a character vector for well-to-number conversion
    if (!is.character(input)) {
      stop("Input for 'to_num' conversion must be a character vector.", call. = FALSE)
    }

    # Extract the alphabetic row and numeric column parts of the well
    # identifiers (handles multi-letter rows such as "AA1" on 1536-well plates).
    rows <- toupper(stringr::str_extract(input, "^[A-Za-z]+"))
    cols <- as.numeric(stringr::str_extract(input, "[0-9]+$"))

    if (any(is.na(rows)) || any(is.na(cols))) {
      stop("Some well labels are not in the form <letters><digits> (e.g. \"A01\").", call. = FALSE)
    }

    # Validate that the row part is within the plate bounds
    if (!all(rows %in% plate_size$rows)) {
      stop("Some input rows are invalid based on the plate size. Valid row letters for this plate are ", paste(plate_size$rows, collapse=", "), ".", call. = FALSE)
    }

    # Validate that the column part is within the plate bounds
    if (any(is.na(cols), cols < 1, cols > plate_size$colmax)) {
      stop("Some input columns are invalid. Columns must be numeric and within the range 1-", plate_size$colmax, ".", call. = FALSE)
    }

    row_indices <- match(rows, plate_size$rows)
    if (wise == "row") {
      return((row_indices - 1) * plate_size$colmax + cols)
    } else {
      return((cols - 1) * plate_size$rowmax + row_indices)
    }
  } else if (direction == "to_well") {
    # Convert numeric index to well label
    if (!is.numeric(input)) {
      stop("Input for 'to_well' conversion must be a numeric vector.")
    }
    # Make sure every index is within plate bounds
    max_index <- plate_size$rowmax * plate_size$colmax
    if (any(input < 1) || any(input > max_index)) {
      stop("Numeric indices are out of bounds.")
    }

    if (wise == "row") {
      rows <- ((input - 1) %/% plate_size$colmax) + 1
      cols <- ((input - 1) %% plate_size$colmax) + 1
    } else {
      cols <- ((input - 1) %/% plate_size$rowmax) + 1
      rows <- ((input - 1) %% plate_size$rowmax) + 1
    }

    well_labels <- paste0(plate_size$rows[rows], sprintf("%02d", cols))
    return(well_labels)
  } else {
    stop("Invalid direction specified. Please use 'to_num' or 'to_well'.")
  }
}

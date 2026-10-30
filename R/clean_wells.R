#' Standardise well labels
#'
#' Rewrites well labels in the standard form used across the package: upper-case
#' row letters and a column of at least two digits (`"A01"`, `"AA12"`). Run it
#' once on labels from an instrument or a spreadsheet, so that they join and
#' compare with labels from [convert_well()] and [read_plate()].
#'
#' `mode` sets how much is accepted:
#'
#' * `"strict"` takes only labels already in the standard form. Use it to check
#'   that a file follows the convention.
#' * `"common"`, the default, also takes other case and padding, surrounding
#'   whitespace and one separator between row and column: `"a1"`, `"A001"`,
#'   `" A1"`, `"A 1"`, `"A-1"`, `"A_1"`, `"A:1"`. Functions across the package
#'   read well labels by this rule.
#' * `"guess"` also takes column-first labels (`"1A"`), row and column numbers
#'   (`"R1C1"`, `"R01C02"`, `"1,1"`, read as row then column), and a well inside
#'   other text (`"Well A1"`, `"Plate1_A01"`).
#'
#' `"guess"` needs one format to read the whole of `x`, and it stops rather than
#' choosing when formats disagree. A label inside other text must be the only
#' part of that text that is a well on this plate, so `"P1_A01"` is read on a
#' 96-well plate but is an error on a 384-well plate, which has a row P. It says
#' what it changed, in a message and in the `"mapping"` attribute of the result.
#' Check that mapping before relying on a guess.
#'
#' In every mode, a label that cannot be read, a well that is not on the plate
#' and a missing label are errors. Row letters are never shortened: `"AA01"` is
#' row 27 of a 1536-well plate and not `"A01"`. Bare numbers such as `5` are also
#' errors, because they depend on the fill direction; use [convert_well()] for
#' those. Repeated wells are kept, because a picklist can use a well many times.
#'
#' @param x A character vector or factor of well labels.
#' @param plate The size of the well plate (see [platesize_check()]).
#' @param mode How much to accept: `"common"`, `"strict"` or `"guess"`.
#'
#' @return A character vector of well labels, the same length as `x`. With
#'   `mode = "guess"` it has a `"mapping"` attribute: a data frame of each
#'   distinct `input` and the `well` it became.
#' @examples
#' clean_wells(c("a1", "A01", "B-12"), plate = 96)
#' clean_wells("A01", plate = 96, mode = "strict")
#' clean_wells(c("R1C1", "R2C12"), plate = 96, mode = "guess")
#' @export
clean_wells <- function(x, plate = 96, mode = c("common", "strict", "guess")) {
  mode <- match.arg(mode)
  info <- platesize_check(plate)
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) {
    stop("`x` must be a character vector of well labels.", call. = FALSE)
  }
  if (anyNA(x)) stop("`x` has missing well labels.", call. = FALSE)
  if (length(x) == 0) return(character())

  if (mode == "guess") return(guess_wells(x, info, plate))

  if (mode == "strict" && !all(grepl("^[A-Z]+[0-9]{2}$", x))) {
    stop_wells("are not in the standard form (e.g. \"A01\")",
               x[!grepl("^[A-Z]+[0-9]{2}$", x)])
  }
  parsed <- parse_wells(x)
  if (anyNA(parsed$row)) {
    stop_wells("are not in the form <letters><digits> (e.g. \"A01\")", x[is.na(parsed$row)])
  }
  on_plate <- wells_on_plate(parsed, info)
  if (!all(on_plate)) {
    stop_wells(paste0("are not on a ", plate, "-well plate"), x[!on_plate])
  }
  format_wells(parsed$row, parsed$col)
}

stop_wells <- function(problem, x) {
  bad <- unique(x)
  stop("Some well labels ", problem, ": ", paste(utils::head(bad, 5), collapse = ", "),
       if (length(bad) > 5) ", ..." else ".", call. = FALSE)
}

wells_on_plate <- function(parsed, info) {
  parsed$row %in% info$rows & !is.na(parsed$col) & parsed$col >= 1 & parsed$col <= info$colmax
}

# Row letters for row numbers; numbers that are not on the plate give NA.
row_letters <- function(num, info) {
  info$rows[as.integer(ifelse(num >= 1 & num <= info$rowmax, num, NA))]
}

# Formats `mode = "guess"` tries. Each returns the row letters and column numbers
# of every label, with NA where it cannot read one.
well_formats <- list(
  "A1" = function(x, info) parse_wells(x),
  "1A" = function(x, info) {
    m <- stringr::str_match(x, "^\\s*([0-9]+)[\\s_:-]?([A-Za-z]+)\\s*$")
    list(row = toupper(m[, 3]), col = as.numeric(m[, 2]))
  },
  "R1C1" = function(x, info) {
    m <- stringr::str_match(x, "(?i)^\\s*R([0-9]+)[\\s_:-]?C([0-9]+)\\s*$")
    list(row = row_letters(as.numeric(m[, 2]), info), col = as.numeric(m[, 3]))
  },
  "row,column" = function(x, info) {
    m <- stringr::str_match(x, "^\\s*\\(?\\s*([0-9]+)\\s*[,;:/_\\s-]\\s*([0-9]+)\\s*\\)?\\s*$")
    list(row = row_letters(as.numeric(m[, 2]), info), col = as.numeric(m[, 3]))
  },
  "well inside text" = function(x, info) {
    row <- rep(NA_character_, length(x))
    col <- rep(NA_real_, length(x))
    for (i in seq_along(x)) {
      # "A1.5" is a number with a decimal point, not a well and a stray 5.
      if (grepl("[0-9]\\.[0-9]", x[i])) next
      parsed <- parse_wells(strsplit(x[i], "[^A-Za-z0-9]+")[[1]])
      hit <- wells_on_plate(parsed, info)
      if (sum(hit) == 1) {
        row[i] <- parsed$row[hit]
        col[i] <- parsed$col[hit]
      }
    }
    list(row = row, col = col)
  }
)

guess_wells <- function(x, info, plate) {
  reads <- lapply(well_formats, function(read) read(x, info))
  ok <- lapply(reads, wells_on_plate, info = info)
  fits <- vapply(ok, all, logical(1))
  if (!any(fits)) {
    best <- ok[[which.max(vapply(ok, sum, integer(1)))]]
    stop_wells(paste0("cannot be read in one format on a ", plate, "-well plate"), x[!best])
  }
  wells <- lapply(reads[fits], function(p) format_wells(p$row, p$col))
  if (length(unique(wells)) > 1) {
    stop("These well labels can be read in more than one way (",
         paste(names(wells), collapse = " or "), "), so they were not converted.",
         call. = FALSE)
  }
  out <- wells[[1]]
  changed <- x != out
  if (any(changed)) {
    message("Interpreted ", sum(changed), " label(s) using format \"", names(wells)[1], "\": ",
            x[changed][1], " -> ", out[changed][1], if (sum(changed) > 1) ", ..." else ".")
  }
  attr(out, "mapping") <- unique(data.frame(input = x, well = out, stringsAsFactors = FALSE))
  out
}

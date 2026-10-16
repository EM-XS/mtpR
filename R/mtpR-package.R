#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

# Column names used in non-standard evaluation inside dplyr/ggplot2 verbs.
# Declaring them here keeps `R CMD check` quiet about "no visible binding".
utils::globalVariables(c(
  "Row", "Column", "fill", ".well", ".tooltip", ".label",
  "Plate.Index", "Well.Index", "Well.Position"
))

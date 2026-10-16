#' How many wells a layout needs, and whether it fits
#'
#' Counts the wells used by `n_reactions` reactions with `replicates` replicates
#' each, repeated `copies` times, plus the wells reserved for controls, and
#' checks them against the plates available.
#'
#' Each copy of the layout starts on a fresh plate and may run over onto more
#' plates. Controls are reserved on every plate that is used, so a layout that
#' spills onto a second plate loses the control wells there too. With
#' `spacing` above 1, every reaction well is followed by `spacing - 1` unused
#' wells, which count as space needed but not as wells used.
#'
#' @param n_reactions Number of reactions (one number).
#' @param replicates Replicates per reaction: one number, or a vector with one
#'   entry per reaction (then `n_reactions` may be left out).
#' @param copies Number of copies of the whole layout (plate replicates).
#' @param plate Plate size (see [platesize_check()]).
#' @param controls Wells reserved on each plate: a number, or a plate from
#'   [place_controls()].
#' @param spacing Distance between neighbouring wells in the layout (1 = no gaps).
#' @param plates Number of plates available.
#' @param error If `TRUE` (default) stop with an explanation when the layout does
#'   not fit; if `FALSE` return `FALSE` instead.
#'
#' @return `FALSE` if the layout does not fit and `error = FALSE`. Otherwise a
#'   list with
#'   * `plate`, `wells_per_plate`, `plates_available`
#'   * `used`: reaction wells over all copies
#'   * `reserved`: control wells over all plates used
#'   * `left`: wells still free across the available plates (negative when short)
#'   * `plates_needed`
#'   * `fits`: `TRUE`
#'
#' @examples
#' plate_capacity(100, replicates = 3, plate = 384, controls = 12)
#' plate_capacity(100, replicates = 3, plate = 96, error = FALSE)
#' plate_capacity(100, replicates = 3, copies = 2, plate = 384, plates = 2)
#' @export
plate_capacity <- function(n_reactions = NULL,
                           replicates = 1,
                           copies = 1,
                           plate = 96,
                           controls = 0,
                           spacing = 1,
                           plates = 1,
                           error = TRUE) {
  info <- platesize_check(plate)
  per_plate <- info$rowmax * info$colmax

  if (length(replicates) > 1) {
    if (!is.null(n_reactions) && n_reactions != length(replicates)) {
      stop("`replicates` has ", length(replicates), " entries but `n_reactions` is ",
           n_reactions, ".", call. = FALSE)
    }
    n_reactions <- length(replicates)
  }
  if (is.null(n_reactions) || length(n_reactions) != 1 || n_reactions < 0) {
    stop("`n_reactions` must be a single non-negative number.", call. = FALSE)
  }
  if (anyNA(replicates) || any(replicates < 1)) {
    stop("`replicates` must be at least 1.", call. = FALSE)
  }
  for (a in c("copies", "plates", "spacing")) {
    v <- get(a)
    if (length(v) != 1 || is.na(v) || v < 1) {
      stop("`", a, "` must be a single number of at least 1.", call. = FALSE)
    }
  }

  n_controls <- if (inherits(controls, "mtpr_plate")) {
    if (attr(controls, "plate") != plate) {
      stop("`controls` is for a ", attr(controls, "plate"), "-well plate, not ", plate, ".",
           call. = FALSE)
    }
    sum(controls$Role == "control")
  } else {
    controls
  }
  if (length(n_controls) != 1 || is.na(n_controls) || n_controls < 0) {
    stop("`controls` must be a number or a plate from place_controls().", call. = FALSE)
  }

  wells_one_copy <- sum(if (length(replicates) == 1) n_reactions * replicates else replicates)
  free_per_plate <- per_plate - n_controls
  # slots needed on the plate: each reaction well plus the gaps between them
  slots_one_copy <- if (wells_one_copy > 0) (wells_one_copy - 1) * spacing + 1 else 0

  fail <- function(msg) {
    if (isTRUE(error)) stop(msg, call. = FALSE)
    FALSE
  }

  if (free_per_plate < 1) {
    return(fail(paste0("All ", per_plate, " wells of the plate are reserved for controls.")))
  }

  plates_per_copy <- max(1, ceiling(slots_one_copy / free_per_plate))
  plates_needed <- plates_per_copy * copies
  used <- wells_one_copy * copies
  reserved <- n_controls * plates_needed
  left <- plates * per_plate - used - reserved

  if (plates_needed > plates) {
    msg <- paste0(
      "The layout does not fit: ", n_reactions, " reactions x ",
      if (length(replicates) == 1) replicates else "per-reaction", " replicates",
      if (copies > 1) paste0(" x ", copies, " copies"),
      " needs ", used, " wells",
      if (n_controls > 0) paste0(" plus ", n_controls, " control wells per plate"),
      ", which takes ", plates_needed, " plate", if (plates_needed > 1) "s",
      " of ", per_plate, " wells, but ", plates, if (plates == 1) " plate is" else " plates are",
      " available."
    )
    return(fail(msg))
  }

  list(
    plate = plate,
    wells_per_plate = per_plate,
    plates_available = plates,
    used = used,
    reserved = reserved,
    left = left,
    plates_needed = plates_needed,
    fits = TRUE
  )
}

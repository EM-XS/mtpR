#' Replicate Reactions and Assign Unique Numeric Positions
#'
#' Takes a data frame of reactions and a number of replicates, and assigns a
#' unique numeric position to every reaction replicate, as the `Well.Index` column:
#' the well's number in fill order, as in [place_controls()] and [convert_well()].
#' A layout that runs past one plate gets numbers above the plate size;
#' [add_plates()] splits those into `Plate.Index` and a `Well.Index` on each plate.
#'
#' The layout is chosen with `pattern`. For three reactions (1, 2, 3) and three
#' replicates the order along the plate is:
#'
#' * `"grouped"`: `111 222 333`. Replicates of a reaction sit side by side.
#'   Spacing: `intra_spacing` between replicates of one reaction and
#'   `inter_spacing` between one reaction and the next.
#' * `"dealt"`: `123 123 123`. The reactions are dealt out like cards, once per
#'   replicate. Spacing: `inter_spacing` between reactions inside a round and
#'   `intra_spacing` from one round to the next.
#' * `"scattered"`: replicates and reactions in random order over the same
#'   run of positions (spaced `inter_spacing` apart). Reproducible with `seed`.
#' * `"quadrants"`: see the section below.
#'
#' The `priority` argument is superseded by `pattern`. It keeps working and there
#' is no plan to remove it, but new code should use `pattern`: `"Reaction"` is
#' `"grouped"` and `"Replicate"` is `"dealt"`, with identical positions. `pattern`
#' also accepts the names `"Together"` (grouped) and `"EveryOther"` (dealt).
#'
#' @section Reserved wells:
#' When `reserved` is given, those wells are skipped. Positions are first worked
#' out as if no wells were reserved, and the n-th position counted from
#' `start_position` is then moved to the n-th free well (so spacing is counted in
#' free wells, and the layout flows around controls and onto the next plate).
#' The reserved wells repeat on every plate.
#'
#' @section Quadrants:
#' `pattern = "quadrants"` tiles the plate into 2x2 squares on a fixed grid from
#' A01 (rows A-B, C-D, ...; columns 1-2, 3-4, ...). Inside a square the
#' positions are numbered
#'
#' ```
#' 1 2
#' 3 4
#' ```
#'
#' Reaction `i` takes square `i` and replicate `k` goes in position `k` (see
#' [well_quadrant()]), so the replicates of every reaction sit in quadrants 1, 2,
#' 3 and 4 of the plate. This needs a plate with an even number of rows and
#' columns (24, 48, 96, 384, 1536) and a known plate size: give `plate`, or
#' `reserved` from [place_controls()].
#'
#' * Squares are filled in `fill_wise` order: `"row"` goes across the rows of
#'   squares, `"col"` down the columns of squares.
#' * With fewer than 4 replicates the unused positions of the square stay empty
#'   (replicate `k` always sits in quadrant `k`).
#' * With more than 4, the reaction takes further squares: replicates 5 to 8 go
#'   in the next square, positions 1 to 4, and so on. A reaction with 6
#'   replicates takes 2 squares, and positions 3 and 4 of the second are empty.
#'   Different replicate counts per reaction work as for the other patterns.
#' * The squares sit on the fixed grid. If `start_position` is not the top-left
#'   well of a square (odd row and odd column) the start moves forward to the next
#'   square's top-left well, and a message says so.
#' * `inter_spacing` counts whole squares: 2 leaves one empty square between
#'   reactions. `intra_spacing` does not apply (replicates have fixed
#'   positions); a value other than 1 is ignored with a warning.
#' * A square that contains any reserved well is skipped whole, so replicate
#'   `k` stays in quadrant `k`. The squares left over by a reserved well are not used.
#' * The output has a `Quadrant` column (1 to 4).
#' * `Well.Index` runs in `fill_wise` order, like for the other patterns, so convert
#'   it with `convert_well(Well.Index, "to_well", plate, wise = fill_wise)`.
#'
#' @section Spreading controls among reactions:
#' `controls_spread = n` reserves `n` extra wells for controls, spread evenly
#' among the reactions, and lays out the reactions around them, all in one call.
#' The block of free wells from `start_position` that the layout covers (the
#' reaction wells and any gaps from the spacing) gets the controls as described
#' for `method = "spread"` in [place_controls()]: with 8 controls and 40 reaction
#' wells there is a control after every 5th reaction. Controls already in
#' `reserved` are kept and are not counted as free wells. Everything must fit on
#' the first plate (the controls would otherwise repeat on every plate); if it
#' does not, call [place_controls()] yourself and pass it as `reserved`. The
#' plate map with the controls and the reactions is returned as
#' `attr(result, "plate_map")` (draw it with [plot_plate()]). Not available for
#' `pattern = "quadrants"`.
#'
#' @param rxn_df A df of reactions. Its other columns are carried over to every
#'   replicate of the reaction. It cannot have columns named `replicate` or
#'   `Well.Index` (or `Quadrant` for `pattern = "quadrants"`), which the result adds.
#' @param reaction_col_name The column of unique reaction identifiers to be replicated.
#' @param num_replicates The number of replicates. One number for all
#'   reactions, a vector with one entry per row of `rxn_df`, or the name of a
#'   column of `rxn_df` holding the count for each reaction. (`replicates` is a
#'   synonym.)
#' @param priority Superseded by `pattern`. `"Reaction"` (grouped) or
#'   `"Replicate"` (dealt). Ignored when `pattern` is given.
#' @param inter_spacing The numeric distance between the sets, either different reactions or replicates.
#' @param intra_spacing The numeric distance between replicates within a set.
#' @param start_position The starting position for the first entry: a position
#'   number, or a well label such as `"B01"` (then `plate`, or `reserved` from
#'   [place_controls()], is needed, and `fill_wise` sets how it is numbered).
#' @param pattern `"grouped"`, `"dealt"`, `"scattered"` or `"quadrants"` (see
#'   Details). Also `"Together"` and `"EveryOther"`.
#' @param seed Random seed for `"scattered"`. If `NULL` one is drawn; either
#'   way the seed used is stored in `attr(result, "seed")`.
#' @param replicates Synonym for `num_replicates`.
#' @param reserved Wells to keep free: the output of [place_controls()], a
#'   vector of well labels (e.g. `c("A01", "H12")`) or a vector of position
#'   numbers on one plate. `NULL` (default) reserves nothing.
#' @param plate Plate size. Needed when `reserved` is a vector, and for
#'   `pattern = "quadrants"` and `controls_spread`; read from `reserved` when it
#'   comes from [place_controls()].
#' @param fill_wise `"row"` or `"col"`: the direction positions run along the
#'   plate. Used to turn reserved well labels into positions, and to lay out
#'   `pattern = "quadrants"`.
#' @param controls_spread Number of control wells to spread evenly among the
#'   reactions (see the section above). `NULL` (default) adds none.
#' @param controls_name Name of these controls in the `Control` column of
#'   `attr(result, "plate_map")`.
#'
#' @return A data frame with the columns 'reaction', 'replicate', and 'Well.Index'
#'         (plus 'Quadrant' for `pattern = "quadrants"`).
#'         Each row corresponds to a reaction replicate with a unique assigned position.
#'         With `controls_spread`, `attr(, "plate_map")` is the plate with the
#'         controls and the reaction wells marked.
#'
#' @examples
#' data <- data.frame(rxn = 1:3)
#' grouped <- replicate_reactions(data, "rxn", num_replicates = 3, pattern = "grouped",
#'                                 inter_spacing = 10, intra_spacing = 1, start_position = 1)
#' dealt <- replicate_reactions(data, "rxn", num_replicates = 3, pattern = "dealt",
#'                              inter_spacing = 5, intra_spacing = 1, start_position = 1)
#'
#' rxns <- data.frame(rxn = 1:3, n = c(2, 3, 1))
#' replicate_reactions(rxns, "rxn", num_replicates = "n", pattern = "grouped")
#' replicate_reactions(rxns, "rxn", num_replicates = 3, pattern = "scattered", seed = 1)
#'
#' # Keep the four corners of a 96-well plate free
#' corners <- place_controls(96, wells = c("A01", "A12", "H01", "H12"))
#' replicate_reactions(rxns, "rxn", 3, pattern = "grouped", reserved = corners)
#'
#' # 8 controls spread among 40 reactions: a control after every 5th reaction
#' out <- replicate_reactions(data.frame(rxn = 1:40), "rxn", 1, pattern = "grouped",
#'                            plate = 96, controls_spread = 8)
#' table(attr(out, "plate_map")$Role)
#'
#' # Replicate k of each reaction sits in quadrant k of a 2x2 square
#' q <- replicate_reactions(rxns, "rxn", 3, pattern = "quadrants", plate = 96)
#' q$Well <- convert_well(q$Well.Index, "to_well", plate = 96)
#' q[, c("rxn", "replicate", "Quadrant", "Well")]
#'
#' @export
replicate_reactions <- function(rxn_df,
                                reaction_col_name,
                                num_replicates = replicates,
                                priority = NULL,
                                inter_spacing = 1,
                                intra_spacing = 1,
                                start_position = 1,
                                pattern = NULL,
                                seed = NULL,
                                replicates = NULL,
                                reserved = NULL,
                                plate = NULL,
                                fill_wise = "row",
                                controls_spread = NULL,
                                controls_name = "control") {

  pattern <- resolve_pattern(pattern, priority)
  if (!fill_wise %in% c("row", "col")) {
    stop("The 'fill_wise' parameter must be either 'row' or 'col'.", call. = FALSE)
  }

  if (!reaction_col_name %in% names(rxn_df)) {
    stop("The specified reaction column does not exist in the data frame", call. = FALSE)
  }
  added <- c("replicate", "Well.Index", if (pattern == "quadrants") "Quadrant")
  clash <- intersect(names(rxn_df), added)
  if (length(clash)) {
    stop("`rxn_df` has column(s) that the result adds: ",
         paste(sprintf("`%s`", clash), collapse = ", "), ". Rename them first.", call. = FALSE)
  }
  if (!missing(num_replicates) && !is.null(replicates)) {
    stop("Give either `num_replicates` or `replicates`, not both.", call. = FALSE)
  }
  if (is.null(num_replicates)) {
    stop("`num_replicates` (or `replicates`) is required.", call. = FALSE)
  }

  if (is.character(start_position)) {
    size <- if (inherits(reserved, "mtpr_plate")) attr(reserved, "plate") else plate
    if (is.null(size)) {
      stop("`plate` is required when `start_position` is a well label.", call. = FALSE)
    }
    if (length(start_position) != 1) {
      stop("`start_position` must be a single well label or position.", call. = FALSE)
    }
    start_position <- resolve_start(start_position, size, fill_wise)
  }

  reactions_vector <- rxn_df[[reaction_col_name]]
  if (anyNA(reactions_vector)) {
    stop("Reaction identifiers in column '", reaction_col_name,
         "' must not be NA.", call. = FALSE)
  }
  if (anyDuplicated(reactions_vector)) {
    stop("Reaction identifiers in column '", reaction_col_name,
         "' must be unique.", call. = FALSE)
  }

  # Reactions are laid out in sorted order, as they always have been.
  u <- sort(unique(reactions_vector))
  n_rep <- resolve_replicates(num_replicates, rxn_df, reactions_vector, u)

  df <- data.frame(reaction = rep(u, n_rep),
                   replicate = sequence(n_rep),
                   stringsAsFactors = FALSE)
  ridx <- rep(seq_along(u), n_rep)  # reaction index of each row of df

  seed_used <- NULL
  plate_map <- NULL
  if (pattern == "quadrants") {
    if (!is.null(controls_spread)) {
      stop("`controls_spread` is not available for pattern = \"quadrants\"; ",
           "reserve a quadrant or squares with place_controls() instead.", call. = FALSE)
    }
    q <- layout_quadrants(n_rep, ridx, start_position, inter_spacing, intra_spacing,
                          reserved, plate, fill_wise)
    df$Well.Index <- q$position
    df$Quadrant <- q$quadrant
  } else {
    offsets <- switch(
      pattern,
      grouped   = offsets_grouped(n_rep, inter_spacing, intra_spacing),
      dealt     = offsets_dealt(n_rep, inter_spacing, intra_spacing),
      scattered = offsets_scattered(nrow(df), inter_spacing, seed)
    )
    seed_used <- attr(offsets, "seed")
    attributes(offsets) <- NULL

    if (!is.null(controls_spread)) {
      reserved <- spread_into_reserved(reserved, plate, fill_wise, controls_spread,
                                       controls_name, start_position, offsets)
    }
    if (is.null(reserved)) {
      df$Well.Index <- start_position + offsets
    } else {
      res <- reserved_positions(reserved, plate, fill_wise)
      df$Well.Index <- flow_around_reserved(start_position, offsets,
                                          res$positions, res$plate_size)
    }
    if (!is.null(controls_spread)) {
      wells <- convert_well(df$Well.Index, "to_well", plate = res$plate_size, wise = fill_wise)
      plate_map <- plate_roles(reserved, wells)
    }
  }

  # Add the input df information
  names(df)[names(df) == "reaction"] <- reaction_col_name
  df <- dplyr::left_join(df, rxn_df, by = reaction_col_name)

  if (!is.null(seed_used)) attr(df, "seed") <- seed_used
  attr(df, "pattern") <- pattern
  if (!is.null(plate_map)) attr(df, "plate_map") <- plate_map

  return(df)
}

# Turn `pattern` / `priority` into one of "grouped", "dealt", "scattered",
# "quadrants".
resolve_pattern <- function(pattern, priority) {
  from_priority <- NULL
  if (!is.null(priority)) {
    if (!is.character(priority) || length(priority) != 1 ||
        !priority %in% c("Reaction", "Replicate")) {
      stop("Priority must be either 'Reaction' or 'Replicate'", call. = FALSE)
    }
    from_priority <- c(Reaction = "grouped", Replicate = "dealt")[[priority]]
  }
  if (is.null(pattern)) {
    if (is.null(from_priority)) {
      stop("Choose a layout with `pattern` (\"grouped\", \"dealt\", \"scattered\" or ",
           "\"quadrants\").", call. = FALSE)
    }
    return(from_priority)
  }
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("`pattern` must be one of \"grouped\", \"dealt\", \"scattered\" or \"quadrants\".",
         call. = FALSE)
  }
  aliases <- c(grouped = "grouped", dealt = "dealt", scattered = "scattered",
               quadrants = "quadrants", quadrant = "quadrants",
               together = "grouped", everyother = "dealt")
  out <- aliases[tolower(pattern)]
  if (is.na(out)) {
    stop("`pattern` must be one of \"grouped\", \"dealt\", \"scattered\" or ",
         "\"quadrants\" (got \"", pattern, "\").", call. = FALSE)
  }
  unname(out)
}

# Replicate count for each reaction in `u` (sorted unique reaction ids).
resolve_replicates <- function(x, rxn_df, reactions_vector, u) {
  if (is.character(x) && length(x) == 1) {
    if (!x %in% names(rxn_df)) {
      stop("Replicate column '", x, "' does not exist in the data frame", call. = FALSE)
    }
    x <- rxn_df[[x]]
  }
  if (!is.numeric(x)) {
    stop("`num_replicates` must be a number, a numeric vector with one entry per ",
         "reaction, or the name of a column holding the counts.", call. = FALSE)
  }
  if (length(x) == 1) {
    x <- rep(x, length(u))
  } else if (length(x) == length(reactions_vector)) {
    x <- x[match(u, reactions_vector)]
  } else {
    stop("`num_replicates` has ", length(x), " entries but there are ",
         length(reactions_vector), " reactions.", call. = FALSE)
  }
  if (anyNA(x) || any(x < 1) || any(x != round(x))) {
    stop("Replicate counts must be whole numbers of at least 1.", call. = FALSE)
  }
  as.integer(x)
}

# Offsets (from the start position) in the order of the output rows, which run
# reaction by reaction, replicate by replicate.
offsets_grouped <- function(n_rep, inter, intra) {
  out <- numeric(sum(n_rep))
  cur <- 0
  k <- 0L
  for (i in seq_along(n_rep)) {
    for (rep in seq_len(n_rep[i])) {
      k <- k + 1L
      out[k] <- cur
      cur <- cur + intra
    }
    cur <- cur + inter - intra
  }
  out
}

offsets_dealt <- function(n_rep, inter, intra) {
  start_row <- cumsum(c(0L, n_rep))[seq_along(n_rep)]  # first output row of each reaction
  out <- numeric(sum(n_rep))
  cur <- 0
  for (rep in seq_len(max(n_rep))) {
    for (i in which(n_rep >= rep)) {
      out[start_row[i] + rep] <- cur
      cur <- cur + inter
    }
    cur <- cur + intra - inter
  }
  out
}

offsets_scattered <- function(n, inter, seed) {
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1)
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    stop("`seed` must be a single number.", call. = FALSE)
  }
  out <- (sample_with_seed(n, seed) - 1) * inter
  attr(out, "seed") <- seed
  out
}

# sample.int(n) with a fixed seed, leaving the caller's RNG state untouched.
sample_with_seed <- function(n, seed) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  sample.int(n)
}

# Move raw offsets (counted from `start`) onto free wells: offset d goes to the
# (d + 1)-th free well at or after `start`. Reserved wells repeat on each plate.
flow_around_reserved <- function(start, offsets, reserved, plate_size) {
  if (any(offsets != round(offsets))) {
    stop("Spacing must be whole numbers when wells are reserved.", call. = FALSE)
  }
  free_in_plate <- setdiff(seq_len(plate_size), reserved)
  if (length(free_in_plate) == 0) {
    stop("Every well on the plate is reserved.", call. = FALSE)
  }
  need <- max(offsets) + 1
  first_plate <- (start - 1) %/% plate_size
  n_plates <- ceiling(need / length(free_in_plate)) + 2
  idx <- as.vector(outer(free_in_plate, (first_plate + seq_len(n_plates) - 1) * plate_size, `+`))
  idx <- idx[idx >= start]
  idx[offsets + 1]
}

# Reserve `n` controls spread among the reactions' wells and return the plate
# (as from place_controls()) to give to the flow as `reserved`.
spread_into_reserved <- function(reserved, plate, wise, n, name, start, offsets) {
  if (any(offsets != round(offsets))) {
    stop("Spacing must be whole numbers when wells are reserved.", call. = FALSE)
  }
  if (inherits(reserved, "mtpr_plate")) {
    map <- reserved
    size <- attr(map, "plate")
    if (!is.null(plate) && plate != size) {
      stop("`plate` (", plate, ") does not match the reserved plate (", size, ").",
           call. = FALSE)
    }
    if (attr(map, "wise") != wise) {  # Well.Index must follow the fill direction
      map$Well.Index <- convert_well(map$Well, "to_num", plate = size, wise = wise)
      attr(map, "wise") <- wise
    }
  } else {
    if (is.null(plate)) {
      stop("`plate` is required for `controls_spread`.", call. = FALSE)
    }
    size <- plate
    map <- empty_plate_map(size, wise)
    if (!is.null(reserved)) {
      pos <- reserved_positions(reserved, plate, wise)$positions
      map <- place_controls(map, "fixed",
                            wells = convert_well(pos, "to_well", plate = size, wise = wise))
    }
  }
  span <- max(offsets) + 1
  place_controls(map, "spread", n = n, among = span, start = start, name = name)
}

# Positions for pattern = "quadrants": reaction i takes the squares it needs,
# replicate j goes in position ((j - 1) %% 4) + 1 of square (j - 1) %/% 4.
layout_quadrants <- function(n_rep, ridx, start, inter, intra, reserved, plate, wise) {
  if (inherits(reserved, "mtpr_plate")) {
    res <- reserved_positions(reserved, plate, wise)
    size <- res$plate_size
  } else {
    if (is.null(plate)) {
      stop("`plate` is required for pattern = \"quadrants\".", call. = FALSE)
    }
    res <- if (is.null(reserved)) list(positions = numeric(0)) else
      reserved_positions(reserved, plate, wise)
    size <- plate
  }
  check_quadrant_plate(size)
  if (length(inter) != 1 || is.na(inter) || inter < 1 || inter != round(inter)) {
    stop("For pattern = \"quadrants\", `inter_spacing` counts squares and must be a ",
         "whole number of at least 1.", call. = FALSE)
  }
  if (!identical(as.numeric(intra), 1)) {
    warning("`intra_spacing` does not apply to pattern = \"quadrants\" and is ignored.",
            call. = FALSE)
  }
  if (length(start) != 1 || is.na(start) || start < 1 || start != round(start)) {
    stop("`start_position` must be a whole number of at least 1.", call. = FALSE)
  }

  sq <- square_of_position(start, size, wise)
  start_sq <- sq$square
  if (!sq$top_left) {
    start_sq <- start_sq + 1
    label <- function(pos) convert_well(((pos - 1) %% size) + 1, "to_well", plate = size, wise = wise)
    message("Quadrants start on a square's top-left well: moved start from ",
            label(start), " to ", label(square_positions(start_sq, 1, size, wise)), ".")
  }

  units <- ceiling(n_rep / 4)
  first_sq <- cumsum(c(0, units + inter - 1))[seq_along(n_rep)]  # offset of each reaction's first square
  rep_no <- sequence(n_rep)
  offsets <- first_sq[ridx] + (rep_no - 1) %/% 4
  k <- (rep_no - 1) %% 4 + 1

  per_plate <- size / 4
  reserved_sq <- if (length(res$positions)) {
    unique(square_of_position(res$positions, size, wise)$square)
  } else {
    integer(0)
  }
  free_sq <- setdiff(seq_len(per_plate), reserved_sq)
  if (length(free_sq) == 0) {
    stop("Every square on the plate contains a reserved well.", call. = FALSE)
  }
  first_plate <- (start_sq - 1) %/% per_plate
  n_plates <- ceiling((max(offsets) + 1) / length(free_sq)) + 2
  idx <- as.vector(outer(free_sq, (first_plate + seq_len(n_plates) - 1) * per_plate, `+`))
  idx <- idx[idx >= start_sq]
  list(position = square_positions(idx[offsets + 1], k, size, wise), quadrant = as.integer(k))
}

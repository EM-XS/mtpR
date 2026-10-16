#' Replicate Reactions and Assign Unique Numeric Positions
#'
#' Takes a data frame of reactions and a number of replicates, and assigns a
#' unique numeric position to every reaction replicate.
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
#'
#' The older `priority` argument still works: `"Reaction"` is `"grouped"` and
#' `"Replicate"` is `"dealt"`, with identical positions. `pattern` also accepts
#' the names `"Together"` (grouped) and `"EveryOther"` (dealt).
#'
#' @section Reserved wells:
#' When `reserved` is given, those wells are skipped. Positions are first worked
#' out as if no wells were reserved, and the n-th position counted from
#' `start_position` is then moved to the n-th free well (so spacing is counted in
#' free wells, and the layout flows around controls and onto the next plate).
#' The reserved wells repeat on every plate.
#'
#' @param rxn_df A df of reactions.
#' @param reaction_col_name The column of unique reaction identifiers to be replicated.
#' @param num_replicates The number of replicates. One number for all
#'   reactions, a vector with one entry per row of `rxn_df`, or the name of a
#'   column of `rxn_df` holding the count for each reaction. (`replicates` is a
#'   synonym.)
#' @param priority Older way to choose the layout: `"Reaction"` (grouped) or
#'   `"Replicate"` (dealt). Ignored when `pattern` is given.
#' @param inter_spacing The numeric distance between the sets, either different reactions or replicates.
#' @param intra_spacing The numeric distance between replicates within a set.
#' @param start_position The starting numeric position for the first entry.
#' @param pattern `"grouped"`, `"dealt"` or `"scattered"` (see Details). Also
#'   `"Together"` and `"EveryOther"`.
#' @param seed Random seed for `"scattered"`. If `NULL` one is drawn; either
#'   way the seed used is stored in `attr(result, "seed")`.
#' @param replicates Synonym for `num_replicates`.
#' @param reserved Wells to keep free: the output of [place_controls()], a
#'   vector of well labels (e.g. `c("A01", "H12")`) or a vector of position
#'   numbers on one plate. `NULL` (default) reserves nothing.
#' @param plate Plate size. Needed when `reserved` is a vector; read from
#'   `reserved` when it comes from [place_controls()].
#' @param fill_wise `"row"` or `"col"`: the direction positions run along the
#'   plate. Only used to turn reserved well labels into positions.
#'
#' @return A data frame with the columns 'reaction', 'replicate', and 'position'.
#'         Each row corresponds to a reaction replicate with a unique assigned position.
#'
#' @examples
#' data <- data.frame(rxn = 1:3)
#' data <- replicate_reactions(data, "rxn", num_replicates = 3, priority = "Reaction",
#'                             inter_spacing = 10, intra_spacing = 1, start_position = 1)
#' data <- replicate_reactions(data, "rxn", num_replicates = 3, priority = "Replicate",
#'                             inter_spacing = 5, intra_spacing = 1, start_position = 1)
#'
#' rxns <- data.frame(rxn = 1:3, n = c(2, 3, 1))
#' replicate_reactions(rxns, "rxn", num_replicates = "n", pattern = "grouped")
#' replicate_reactions(rxns, "rxn", num_replicates = 3, pattern = "scattered", seed = 1)
#'
#' # Keep the four corners of a 96-well plate free
#' corners <- place_controls(96, wells = c("A01", "A12", "H01", "H12"))
#' replicate_reactions(rxns, "rxn", 3, pattern = "grouped", reserved = corners)
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
                                fill_wise = "row") {

  pattern <- resolve_pattern(pattern, priority)

  if (!reaction_col_name %in% names(rxn_df)) {
    stop("The specified reaction column does not exist in the data frame", call. = FALSE)
  }
  if (!missing(num_replicates) && !is.null(replicates)) {
    stop("Give either `num_replicates` or `replicates`, not both.", call. = FALSE)
  }
  if (is.null(num_replicates)) {
    stop("`num_replicates` (or `replicates`) is required.", call. = FALSE)
  }

  reactions_vector <- rxn_df[[reaction_col_name]]
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

  offsets <- switch(
    pattern,
    grouped   = offsets_grouped(n_rep, inter_spacing, intra_spacing),
    dealt     = offsets_dealt(n_rep, inter_spacing, intra_spacing),
    scattered = offsets_scattered(nrow(df), inter_spacing, seed)
  )
  seed_used <- attr(offsets, "seed")
  attributes(offsets) <- NULL

  if (is.null(reserved)) {
    df$position <- start_position + offsets
  } else {
    res <- reserved_positions(reserved, plate, fill_wise)
    df$position <- flow_around_reserved(start_position, offsets,
                                        res$positions, res$plate_size)
  }

  # Add the input df information
  df <- dplyr::left_join(df, rxn_df, by = stats::setNames(reaction_col_name, "reaction"))
  names(df)[names(df) == "reaction"] <- reaction_col_name

  if (!is.null(seed_used)) attr(df, "seed") <- seed_used
  attr(df, "pattern") <- pattern

  return(df)
}

# Turn `pattern` / `priority` into one of "grouped", "dealt", "scattered".
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
      stop("Choose a layout with `pattern` (\"grouped\", \"dealt\" or \"scattered\").",
           call. = FALSE)
    }
    return(from_priority)
  }
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("`pattern` must be one of \"grouped\", \"dealt\" or \"scattered\".", call. = FALSE)
  }
  aliases <- c(grouped = "grouped", dealt = "dealt", scattered = "scattered",
               together = "grouped", everyother = "dealt")
  out <- aliases[tolower(pattern)]
  if (is.na(out)) {
    stop("`pattern` must be one of \"grouped\", \"dealt\" or \"scattered\" (got \"",
         pattern, "\").", call. = FALSE)
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

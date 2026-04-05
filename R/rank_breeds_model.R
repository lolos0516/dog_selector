# Breed ranking model for user preference matching
# -----------------------------------------------
# Input: merged.csv with columns
#   breed, Median.Survival, energy_raw, temperament_score,
#   friendliness_score, trainability_score, space_score, space_category
#
# Main function:
#   rank_breeds(...)
#
# Preference encoding:
#   For each continuous trait, choose one of:
#     "high"  = prefer higher values
#     "low"   = prefer lower values
#     "none"  = no concern; trait ignored in ranking
#
# Space handling:
#   space_category is used as a FILTER, not a ranking variable.
#   Allowed categories in increasing space demand:
#     apartment_friendly < apartment_ok < small_house < large_house < outdoor_compulsory
#
# Example:
#   rank_breeds(
#     data = dogs,
#     space_filter = "large_house",
#     lifespan_pref = "high",
#     energy_pref = "low",
#     temperament_pref = "high",
#     friendliness_pref = "high",
#     trainability_pref = "high"
#   )
#
#   rank_breeds(
#     data = dogs,
#     space_filter = "large_house",
#     lifespan_pref = "none",
#     energy_pref = "high",
#     temperament_pref = "high",
#     friendliness_pref = "low",
#     trainability_pref = "high"
#   )

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# Read your merged data
read_breed_data <- function(path = "merged.csv") {
  read_csv(path, show_col_types = FALSE)
}

# Internal helper: rescale numeric vector to 0-1
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) {
    return(rep(NA_real_, length(x)))
  }
  if (diff(rng) == 0) {
    return(rep(0.5, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

# Internal helper: convert preference into desirability score
# pref = "high" means larger values are better
# pref = "low" means smaller values are better
# pref = "none" means ignored later by weight = 0
make_desirability <- function(x, pref = c("high", "low", "none")) {
  pref <- match.arg(pref)
  z <- rescale01(x)
  if (pref == "high") return(z)
  if (pref == "low") return(1 - z)
  rep(0.5, length(x))
}

# Internal helper: map space categories to ordered levels
space_rank <- function(space_category) {
  levels <- c(
    "apartment_friendly",
    "apartment_ok",
    "small_house",
    "large_house",
    "outdoor_compulsory"
  )
  match(space_category, levels) - 1
}

# Main ranking function
rank_breeds <- function(
  data,
  space_filter = c(
    "apartment_friendly",
    "apartment_ok",
    "small_house",
    "large_house",
    "outdoor_compulsory"
  ),
  lifespan_pref = c("high", "low", "none"),
  energy_pref = c("high", "low", "none"),
  temperament_pref = c("high", "low", "none"),
  friendliness_pref = c("high", "low", "none"),
  trainability_pref = c("high", "low", "none"),
  weights = c(
    lifespan = 1,
    energy = 1,
    temperament = 1,
    friendliness = 1,
    trainability = 1
  ),
  top_n = NULL
) {
  # Match arguments
  space_filter <- match.arg(space_filter)
  lifespan_pref <- match.arg(lifespan_pref)
  energy_pref <- match.arg(energy_pref)
  temperament_pref <- match.arg(temperament_pref)
  friendliness_pref <- match.arg(friendliness_pref)
  trainability_pref <- match.arg(trainability_pref)

  # Basic checks
  required_cols <- c(
    "breed", "Median.Survival", "energy_raw", "temperament_score",
    "friendliness_score", "trainability_score", "space_category"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter by allowable accommodation
  max_space <- space_rank(space_filter)
  out <- data %>%
    mutate(.space_rank = space_rank(space_category)) %>%
    filter(!is.na(.space_rank), .space_rank <= max_space)

  # Create per-trait desirability
  out <- out %>%
    mutate(
      lifespan_match = make_desirability(Median.Survival, lifespan_pref),
      energy_match = make_desirability(energy_raw, energy_pref),
      temperament_match = make_desirability(temperament_score, temperament_pref),
      friendliness_match = make_desirability(friendliness_score, friendliness_pref),
      trainability_match = make_desirability(trainability_score, trainability_pref)
    )

  # Turn "none" traits off by setting effective weight = 0
  effective_weights <- c(
    lifespan = ifelse(lifespan_pref == "none", 0, weights[["lifespan"]]),
    energy = ifelse(energy_pref == "none", 0, weights[["energy"]]),
    temperament = ifelse(temperament_pref == "none", 0, weights[["temperament"]]),
    friendliness = ifelse(friendliness_pref == "none", 0, weights[["friendliness"]]),
    trainability = ifelse(trainability_pref == "none", 0, weights[["trainability"]])
  )

  total_weight <- sum(effective_weights)

  if (total_weight == 0) {
    # If every continuous trait is "none", all filtered breeds tie
    out <- out %>%
      mutate(overall_score = 1)
  } else {
    out <- out %>%
      mutate(
        overall_score =
          (
            effective_weights[["lifespan"]] * lifespan_match +
            effective_weights[["energy"]] * energy_match +
            effective_weights[["temperament"]] * temperament_match +
            effective_weights[["friendliness"]] * friendliness_match +
            effective_weights[["trainability"]] * trainability_match
          ) / total_weight
      )
  }

  out <- out %>%
    arrange(desc(overall_score), breed) %>%
    mutate(rank = row_number()) %>%
    select(
      rank, breed, space_category,
      Median.Survival, energy_raw,
      temperament_score, friendliness_score, trainability_score,
      lifespan_match, energy_match, temperament_match,
      friendliness_match, trainability_match,
      overall_score
    )

  if (!is.null(top_n)) {
    out <- out %>% slice_head(n = top_n)
  }

  out
}

# Convenience wrapper to read merged.csv and rank immediately
rank_breeds_from_file <- function(
  path = "merged.csv",
  space_filter = "small_house",
  lifespan_pref = "high",
  energy_pref = "low",
  temperament_pref = "high",
  friendliness_pref = "high",
  trainability_pref = "high",
  weights = c(lifespan = 1, energy = 1, temperament = 1, friendliness = 1, trainability = 1),
  top_n = 20
) {
  dat <- read_breed_data(path)
  rank_breeds(
    data = dat,
    space_filter = space_filter,
    lifespan_pref = lifespan_pref,
    energy_pref = energy_pref,
    temperament_pref = temperament_pref,
    friendliness_pref = friendliness_pref,
    trainability_pref = trainability_pref,
    weights = weights,
    top_n = top_n
  )
}

# Example usage
# dogs <- read_breed_data("merged.csv")
#
# Example 1:
# longer lifespan, lower energy, calm, friendly, trainable, apartment
# rank_breeds(
#   data = dogs,
#   space_filter = "apartment_ok",
#   lifespan_pref = "high",
#   energy_pref = "low",
#   temperament_pref = "high",
#   friendliness_pref = "high",
#   trainability_pref = "high",
#   top_n = 15
# )
#
# Example 2:
# high energy, outdoor, calm, unfriendly, trainable, no concern for lifespan
# rank_breeds(
#   data = dogs,
#   space_filter = "outdoor_compulsory",
#   lifespan_pref = "none",
#   energy_pref = "high",
#   temperament_pref = "high",
#   friendliness_pref = "low",
#   trainability_pref = "high",
#   top_n = 15
# )

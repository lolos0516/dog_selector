# Build proxy dog breed trait scores
# ---------------------------------
# Inputs:
#   - breed_trait_archetypes.csv
#   - trait_archetype_scores.csv
#   - trait_breed_overrides.csv
#
# Output:
#   - dog_breed_trait_scores.csv
#
# Scores are on a 1-5 scale:
#   temperament_score: calm/steady = high, anxious/reactive = low
#   friendliness_score: friendly to children/other pets/strangers = high
#   trainability_score: easier to teach / more biddable = high

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

breed_map <- read_csv("breed_trait_archetypes.csv", show_col_types = FALSE)
arch <- read_csv("trait_archetype_scores.csv", show_col_types = FALSE)
ovr <- read_csv("trait_breed_overrides.csv", show_col_types = FALSE)

out <- breed_map %>%
  left_join(arch %>% select(-archetype_note), by = "archetype") %>%
  left_join(ovr %>% select(-override_note), by = "breed") %>%
  mutate(
    across(ends_with("_adj"), ~tidyr::replace_na(.x, 0)),
    temperament_score = pmin(5, pmax(1, round(temperament_base + temperament_adj, 1))),
    friendliness_score = pmin(5, pmax(1, round(friendliness_base + friendliness_adj, 1))),
    trainability_score = pmin(5, pmax(1, round(trainability_base + trainability_adj, 1)))
  ) %>%
  select(breed, temperament_score, friendliness_score, trainability_score) %>%
  arrange(breed)

write_csv(out, "dog_breed_trait_scores.csv")
cat("Wrote dog_breed_trait_scores.csv\n")

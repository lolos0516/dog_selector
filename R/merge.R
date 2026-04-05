library(dplyr)

#### read ####
energy <- read.csv("data_app/breed_energy_from_cbarq.csv", header = T)
# breed energy_raw energy_score
# 1       Crossbreed          3         0.50
# 2    Affenpinscher          2         0.25
# 3     Afghan Hound          3         0.50
lifespan <- read.csv("data/dog_lifespan_table_fixed.csv", header = T)
names(lifespan)[1] <- "breed"
# Breed  Alive Deaths Median.Survival Lower.95..CI Upper.95..CI       HR..95..CI. p.value
# 1       Crossbreed 111053  72445            12.0         12.0         12.1                 —       —
# 2    Affenpinscher    338    204             9.3          7.4         10.8  1.7 (1.48, 1.94)  <0.001
# 3     Afghan Hound    380    238            11.1         10.5         11.8 1.35 (1.19, 1.53)  <0.001
personality <-read.csv("data_app/dog_breed_trait_scores.csv", header = T)
# breed temperament_score friendliness_score trainability_score
# 1           Affenpinscher               3.8                4.1                3.4
# 2            Afghan Hound               4.1                3.6                2.8
# 3        Airedale Terrier               2.6                3.2                3.1
space <- read.csv("data/dog_space_requirements.csv", header = T)
# breed space_score space_category
# 1       Crossbreed           2    small_house
# 2    Affenpinscher           2    small_house
# 3     Afghan Hound           2    small_house

#156 breeds in total

#### create table ####
# Merge the three tables together by breed
merged_data <- lifespan %>%
  select(breed, Median.Survival) %>%
  inner_join(energy %>% select(-energy_score), by = "breed") %>%
  inner_join(personality, by = "breed") %>% 
  inner_join(space, by = "breed")

head(merged_data,3)
# breed Median.Survival energy_raw temperament_score friendliness_score trainability_score space_score
# Crossbreed            12.0         3               3.5                3.6                3.5           2
# Affenpinscher             9.3         2               3.8                4.1                3.4           2
# Afghan Hound            11.1         3               4.1                3.6                2.8           2
# space_category
#   small_house
#   small_house
#   small_house

summary(merged_data)
cor(merged_data[,2:7])

write.csv(merged_data, "data_app/merged.csv", row.names = F, quote = F)

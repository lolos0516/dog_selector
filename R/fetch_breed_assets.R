suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(httr2)
  library(jsonlite)
})

# Build local breed summaries and image references for the app.
# Input:
#   data_app/merged.csv
# Output:
#   data_app/breed_info/<breed>.json
#   images/<breed>.jpg  (when available)

read_breed_data <- function(path = "data_app/merged.csv") {
  read_csv(path, show_col_types = FALSE)
}

# dir.create("data_app", showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("data_app", "breed_info"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("www", "images"), showWarnings = FALSE, recursive = TRUE)

safe_file_name <- function(x) {
  # keep readable names matching breed values used in app
  x
}

fetch_wikipedia_info <- function(breed) {
  breed_query <- URLencode(breed, reserved = TRUE)
  summary_url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", breed_query)

  out <- list(
    breed = breed,
    title = breed,
    summary = NULL,
    image_url = NULL,
    image_file = paste0(safe_file_name(breed), ".jpg"),
    source = "generated_fallback"
  )

  ok <- FALSE

  try({
    resp <- request(summary_url) |>
      req_user_agent("shiny-breed-app-preprocess") |>
      req_timeout(20) |>
      req_perform()

    js <- jsonlite::fromJSON(resp_body_string(resp))
    if (!is.null(js$title) && nzchar(js$title)) out$title <- js$title
    if (!is.null(js$extract) && nzchar(js$extract)) out$summary <- js$extract
    if (!is.null(js$thumbnail$source) && nzchar(js$thumbnail$source)) out$image_url <- js$thumbnail$source
    out$source <- "wikipedia"
    ok <- TRUE
  }, silent = TRUE)

  list(result = out, ok = ok)
}

build_fallback_summary <- function(row_df) {
  energy_label <- dplyr::case_when(
    is.na(row_df$energy_raw) ~ "unknown",
    row_df$energy_raw <= 1.5 ~ "very low",
    row_df$energy_raw <= 2.5 ~ "low",
    row_df$energy_raw <= 3.5 ~ "moderate",
    row_df$energy_raw <= 4.5 ~ "high",
    TRUE ~ "very high"
  )

  trait_label <- function(x) dplyr::case_when(
    is.na(x) ~ "unknown",
    x < 2 ~ "low",
    x < 3 ~ "low–moderate",
    x < 4 ~ "moderate",
    x < 4.5 ~ "high",
    TRUE ~ "very high"
  )

  space_label <- dplyr::case_when(
    row_df$space_category == "apartment_friendly" ~ "Apartment no balcony",
    row_df$space_category == "apartment_ok" ~ "Apartment with a balcony",
    row_df$space_category == "small_house" ~ "Small house and back yard",
    row_df$space_category == "large_house" ~ "Large house and back yard",
    row_df$space_category == "outdoor_compulsory" ~ "Ample outdoor space",
    TRUE ~ as.character(row_df$space_category)
  )

  paste0(
    row_df$breed, " has a median lifespan of about ", sprintf("%.1f", row_df$Median.Survival),
    " years. Its daily energy requirement is ", energy_label,
    " (", sprintf("%.1f", row_df$energy_raw), " on this app's scale). ",
    "Its calmness/steadiness is ", trait_label(row_df$temperament_score),
    ", friendliness is ", trait_label(row_df$friendliness_score),
    ", and trainability is ", trait_label(row_df$trainability_score),
    ". Its accommodation category is ", space_label, "."
  )
}

download_image <- function(url, dest) {
  try({
    resp <- request(url) |>
      req_user_agent("shiny-breed-app-preprocess") |>
      req_timeout(30) |>
      req_perform()
    writeBin(resp_body_raw(resp), dest)
    TRUE
  }, silent = TRUE)
}

dogs <- read_breed_data("data_app/merged.csv")

for (i in seq_len(nrow(dogs))) {
  row <- dogs[i, ]
  breed <- row$breed[[1]]
  fetched <- fetch_wikipedia_info(breed)
  info <- fetched$result

  if (is.null(info$summary) || !nzchar(info$summary)) {
    info$summary <- build_fallback_summary(row)
  }

  img_dest <- file.path("www", "images", paste0(safe_file_name(breed), ".jpg"))
  if (!file.exists(img_dest) && !is.null(info$image_url) && nzchar(info$image_url)) {
    ok <- download_image(info$image_url, img_dest)
    if (!isTRUE(ok) && file.exists(img_dest)) unlink(img_dest)
  }

  if (file.exists(img_dest)) {
    info$image_file <- basename(img_dest)
  } else {
    info$image_file <- NA_character_
  }

  jsonlite::write_json(
    info,
    path = file.path("data_app", "breed_info", paste0(safe_file_name(breed), ".json")),
    pretty = TRUE,
    auto_unbox = TRUE
  )
}

cat("Saved breed info JSON files to data_app/breed_info/\n")
cat("Saved images to images/ when available\n")

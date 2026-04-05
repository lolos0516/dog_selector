# Build breed-level dog energy scores from a C-BARQ-derived dataset
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(janitor)
  library(tools)
})


input_path <- "data/cbarq_proxy_energy.csv"
output_path <- file.path("data_app", "breed_energy_from_cbarq.csv")

# ---------- read file ----------
read_any <- function(path) {
  ext <- tolower(file_ext(path))
  if (ext == "csv") return(read_csv(path, show_col_types = FALSE))
  if (ext %in% c("tsv", "txt")) return(read_tsv(path, show_col_types = FALSE))
  if (ext %in% c("xlsx", "xls")) return(read_excel(path))
  stop("Unsupported format")
}

dat <- read_any(input_path) %>%
  janitor::clean_names()

# ---------- detect columns ----------
find_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NA)
  hit[[1]]
}

breed_col <- find_col(dat, c("breed","breed_name","dog_breed"))
energy_col <- find_col(dat, c("energy_level","energy","cbarq_energy_level"))

if (is.na(breed_col)) stop("No breed column found")
if (is.na(energy_col)) stop("No energy column found")

# ---------- build dataset ----------
df <- dat %>%
  transmute(
    breed = .data[[breed_col]],
    energy_raw = as.numeric(.data[[energy_col]])
  ) %>%
  filter(!is.na(breed), !is.na(energy_raw)) %>%
  distinct(breed, .keep_all = TRUE)

# ---------- normalize to 0–1 ----------
rng <- range(df$energy_raw, na.rm = TRUE)

if (diff(rng) == 0) {
  df$energy_score <- 0.5
} else {
  df$energy_score <- (df$energy_raw - rng[1]) / (rng[2] - rng[1])
}

# ---------- output ----------
write_csv(df, output_path)

cat("Saved:", output_path, "\n")

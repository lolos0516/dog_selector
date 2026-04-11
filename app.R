suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(glue)
  library(httr2)
  library(jsonlite)
  library(scales)
})

# =========================
# Data + ranking functions
# =========================

addResourcePath(
  "breed_images",
  normalizePath("www/images", winslash = "/", mustWork = TRUE)
)

read_breed_data <- function(path = "merged.csv") {
  read_csv(path, show_col_types = FALSE)
}

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) return(rep(NA_real_, length(x)))
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

make_desirability <- function(x, pref = c("high", "low", "none")) {
  pref <- match.arg(pref)
  z <- rescale01(x)
  if (pref == "high") return(z)
  if (pref == "low") return(1 - z)
  rep(0.5, length(x))
}

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
  space_filter <- match.arg(space_filter)
  lifespan_pref <- match.arg(lifespan_pref)
  energy_pref <- match.arg(energy_pref)
  temperament_pref <- match.arg(temperament_pref)
  friendliness_pref <- match.arg(friendliness_pref)
  trainability_pref <- match.arg(trainability_pref)
  
  required_cols <- c(
    "breed", "Median.Survival", "energy_raw", "temperament_score",
    "friendliness_score", "trainability_score", "space_category"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  max_space <- space_rank(space_filter)
  
  out <- data %>%
    mutate(.space_rank = space_rank(space_category)) %>%
    filter(!is.na(.space_rank), .space_rank <= max_space)
  
  out <- out %>%
    mutate(
      lifespan_match = make_desirability(Median.Survival, lifespan_pref),
      energy_match = make_desirability(energy_raw, energy_pref),
      temperament_match = make_desirability(temperament_score, temperament_pref),
      friendliness_match = make_desirability(friendliness_score, friendliness_pref),
      trainability_match = make_desirability(trainability_score, trainability_pref)
    )
  
  effective_weights <- c(
    lifespan = ifelse(lifespan_pref == "none", 0, weights[["lifespan"]]),
    energy = ifelse(energy_pref == "none", 0, weights[["energy"]]),
    temperament = ifelse(temperament_pref == "none", 0, weights[["temperament"]]),
    friendliness = ifelse(friendliness_pref == "none", 0, weights[["friendliness"]]),
    trainability = ifelse(trainability_pref == "none", 0, weights[["trainability"]])
  )
  
  total_weight <- sum(effective_weights)
  
  if (total_weight == 0) {
    out <- out %>% mutate(overall_score = 1)
  } else {
    out <- out %>%
      mutate(
        overall_score = (
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
    mutate(rank = row_number())
  
  if (!is.null(top_n)) out <- out %>% slice_head(n = top_n)
  out
}

# =========================
# Helper functions
# =========================

space_label <- function(x) {
  dplyr::case_when(
    x == "apartment_friendly" ~ "Apartment no balcony",
    x == "apartment_ok" ~ "Apartment with a balcony",
    x == "small_house" ~ "Small house and a yard",
    x == "large_house" ~ "Large house and a yard",
    x == "outdoor_compulsory" ~ "Ample outdoor space",
    TRUE ~ as.character(x)
  )
}

energy_label <- function(x) {
  case_when(
    is.na(x) ~ "unknown",
    x <= 1.5 ~ "very low",
    x <= 2.5 ~ "low",
    x <= 3.5 ~ "moderate",
    x <= 4.5 ~ "high",
    TRUE ~ "very high"
  )
}

trait_label <- function(x) {
  case_when(
    is.na(x) ~ "unknown",
    x < 2 ~ "low",
    x < 3 ~ "low–moderate",
    x < 4 ~ "moderate",
    x < 4.5 ~ "high",
    TRUE ~ "very high"
  )
}

fmt1 <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f", x))
fmt2 <- function(x) ifelse(is.na(x), "NA", sprintf("%.2f", x))


radar_plot <- function(row_df) {
  actual_vals <- c(
    row_df$Median.Survival / max(20, row_df$Median.Survival, na.rm = TRUE),
    row_df$energy_raw / 5,
    row_df$temperament_score / 5,
    row_df$friendliness_score / 5,
    row_df$trainability_score / 5
  )
  
  plot_dat <- tibble::tibble(
    trait = c("Lifespan", "Energy", "Temperament", "Friendliness", "Trainability"),
    value = actual_vals
  ) %>%
    mutate(
      value = tidyr::replace_na(value, 0.5),
      idx = row_number(),
      angle = pi / 2 - (idx - 1) * 2 * pi / n(),
      x = value * cos(angle),
      y = value * sin(angle),
      label_x = 1.18 * cos(angle),
      label_y = 1.18 * sin(angle)
    )
  
  grid_levels <- c(0.25, 0.5, 0.75, 1.0)
  
  grid_dat <- purrr::map_dfr(grid_levels, function(g) {
    tibble::tibble(
      level = g,
      idx = seq_len(nrow(plot_dat)),
      angle = plot_dat$angle,
      x = g * cos(angle),
      y = g * sin(angle)
    )
  })
  
  axis_dat <- tibble::tibble(
    x = 0,
    y = 0,
    xend = cos(plot_dat$angle),
    yend = sin(plot_dat$angle)
  )
  
  ggplot() +
    geom_polygon(
      data = grid_dat,
      aes(x = x, y = y, group = level),
      fill = NA,
      colour = "grey82",
      linewidth = 0.4
    ) +
    geom_segment(
      data = axis_dat,
      aes(x = x, y = y, xend = xend, yend = yend),
      colour = "grey82",
      linewidth = 0.4
    ) +
    geom_polygon(
      data = plot_dat,
      aes(x = x, y = y),
      fill = scales::alpha("#f3969a", 0.5),
      colour = "#f3969a",
      linewidth = 1
    ) +
    geom_point(
      data = plot_dat,
      aes(x = x, y = y),
      colour = "#f3969a",
      size = 2.2
    ) +
    geom_text(
      data = plot_dat,
      aes(x = label_x, y = label_y, label = trait),
      size = 3.3
    ) +
    coord_equal() +
    theme_void()
}

make_intro_text <- function(row_df, wiki_summary = NULL) {
  as.character(glue(
    "{row_df$breed} is ranked #{row_df$rank} for the current preference profile. ",
    "It has a median lifespan of about {fmt1(row_df$Median.Survival)} years, ",
    "{energy_label(row_df$energy_raw)} daily energy requirement ",
    "(energy score {fmt1(row_df$energy_raw)} on the app scale), ",
    "{trait_label(row_df$temperament_score)} calmness/steadiness, ",
    "{trait_label(row_df$friendliness_score)} friendliness, and ",
    "{trait_label(row_df$trainability_score)} trainability. ",
    "Its accommodation category is {space_label(row_df$space_category)}.",
    if (!is.null(wiki_summary) && nzchar(wiki_summary)) paste0("\n\n", wiki_summary) else ""
  ))
}

# Local breed assets / summaries
get_breed_assets <- function(breed) {
  info_path <- file.path("data_app", "breed_info", paste0(breed, ".json"))
  
  default_disk <- file.path("www", "images", paste0(breed, ".jpg"))
  default_src  <- paste0("breed_images/", URLencode(paste0(breed, ".jpg"), reserved = TRUE))
  
  out <- list(
    title = breed,
    summary = NULL,
    image = if (file.exists(default_disk)) default_src else "https://placehold.co/640x420?text=No+image"
  )
  
  if (file.exists(info_path)) {
    js <- jsonlite::read_json(info_path, simplifyVector = TRUE)
    
    if (!is.null(js$title) && nzchar(js$title)) out$title <- js$title
    if (!is.null(js$summary) && nzchar(js$summary)) out$summary <- js$summary
    
    if (!is.null(js$image_file) && nzchar(js$image_file)) {
      img_disk <- file.path("www", "images", js$image_file)
      img_src  <- paste0("breed_images/", URLencode(js$image_file, reserved = TRUE))
      
      if (file.exists(img_disk)) out$image <- img_src
    }
  }
  
  out
}

# =========================
# UI
# =========================

theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)


make_choice <- function(prefix) {
  choices <- c("high", "none", "low")
  
  if (prefix == "lifespan") {
    names(choices) <- c("Long", "No preference", "Short")
  } else if (prefix == "energy") {
    names(choices) <- c("High", "No preference", "Low")
  } else if (prefix == "temperament") {
    names(choices) <- c("Calm", "No preference", "Anxious")
  } else if (prefix == "friendliness") {
    names(choices) <- c("Friendly to people and pets", "No preference", "Less friendly")
  } else if (prefix == "trainability") {
    names(choices) <- c("Easy", "No preference", "Hard")
  }
  
  choices
}


pref_left_label <- function(prefix) {
  if (prefix == "lifespan") return("Short")
  if (prefix == "energy") return("Low")
  if (prefix == "temperament") return("Anxious")
  if (prefix == "friendliness") return("Less friendly")
  if (prefix == "trainability") return("Hard")
  "Low"
}

pref_right_label <- function(prefix) {
  if (prefix == "lifespan") return("Long")
  if (prefix == "energy") return("High")
  if (prefix == "temperament") return("Calm")
  if (prefix == "friendliness") return("Friendly")
  if (prefix == "trainability") return("Easy")
  "High"
}

slider_pref_to_choice <- function(x) {
  if (is.null(x) || is.na(x) || x == 0) return("none")
  if (x > 0) return("high")
  "low"
}

trait_controls <- function(prefix, label) {
  div(
    class = "trait-row bg-dark-subtle border border-secondary-subtle",
    div(
      class = "trait-head",
      div(class = "trait-title text-light", label),
      div(
        class = "form-check form-switch trait-switch",
        tags$input(class = "form-check-input", type = "checkbox", role = "switch", id = paste0(prefix, "_important")),
        tags$label(class = "form-check-label small", `for` = paste0(prefix, "_important"), "Important")
      )
    ),
    div(
      class = "trait-slider-wrap",
      sliderInput(
        inputId = paste0(prefix, "_pref"),
        label = NULL,
        min = -1,
        max = 1,
        value = 0,
        step = 1,
        ticks = FALSE,
        width = "100%"
      ),
      div(
        class = "trait-slider-labels small text-secondary-emphasis",
        span(pref_left_label(prefix)),
        span("No preference"),
        span(pref_right_label(prefix))
      )
    )
  )
}

ui <- page_fluid(class = "bg-dark text-light app-shell", `data-bs-theme` = "dark",
                 theme = theme,
                 tags$head(
                   tags$style(HTML("
      body.bg-dark { min-height: 100vh; }
      .app-shell { min-height: 100vh; }
      .app-title { font-weight: 800; margin-bottom: 0.2rem; color: #ffffff !important; }
      .breed-card { border-radius: 1.25rem; padding: 0.55rem 0.75rem; margin-bottom: 0.8rem; }
      .results-card { border-radius: 1.25rem; padding: 0.7rem 0.85rem; }
      .result-row { padding: 0.28rem 0; }
      .result-row + .result-row { border-top: 1px solid rgba(255,255,255,0.08); }
      .pref-card, .detail-panel { border-radius: 1.25rem; }
      .breed-pick-btn { width: 100%; text-align: left; border: none; background: transparent; padding: 0; }
      .result-pill-wrap { display: flex; align-items: center; gap: 0; }
      .result-avatar { width: 62px; height: 62px; border-radius: 50%; object-fit: cover; position: relative; z-index: 2; flex: 0 0 auto; }
      .result-bar { margin-left: -14px; flex: 1 1 auto; min-height: 62px; border-radius: 999px; display: flex; align-items: center; justify-content: space-between; padding: 0 10px 0 28px; }
      .breed-name { font-size: 1.02rem; font-weight: 700; margin: 0; line-height: 1.15; }
      .breed-meta { font-size: 0.8rem; opacity: 0.85; margin-top: 0.12rem; }
      .score-orb { width: 62px; height: 62px; border-radius: 50%; flex: 0 0 auto; margin-left: -12px; display: flex; flex-direction: column; align-items: center; justify-content: center; position: relative; z-index: 2; }
      .score-orb .score-rank { font-size: 1.15rem; font-weight: 800; line-height: 1; }
      .score-orb .score-small { font-size: 0.72rem; line-height: 1.05; opacity: 0.95; margin-top: 0.12rem; }
      .detail-panel { padding: 1rem; min-height: 640px; position: sticky; top: 12px; width: 100%; min-width: 0; }
      .detail-panel img { display: block; width: auto; max-width: 100%; height: auto; max-height: 300px; object-fit: contain; border-radius: 1rem; margin: 0 auto 0.85rem auto; }
      .top-note { font-size: 0.92rem; }
      .trait-grid { display: grid; grid-template-columns: 1fr; gap: 0.55rem; }
      .trait-row { display: grid; grid-template-columns: 1fr; gap: 0.25rem; align-items: start; padding: 0.38rem 0.55rem; border-radius: 0.9rem; }
      .trait-title { font-weight: 700; font-size: 1rem; line-height: 1.1; margin: 0; }
      .segmented-group { display: inline-flex; width: 100%; flex-wrap: nowrap; overflow: visible; }
      .segmented-group .btn { flex: 1 1 auto; }
      .form-check { margin-bottom: 0; }
      .result-section-title { font-weight: 700; }
      .trait-head { display: flex; align-items: center; justify-content: space-between; gap: 0.6rem; margin-bottom: 0.2rem; }
      .trait-switch { display: flex; align-items: center; gap: 0.35rem; margin-bottom: 0; white-space: nowrap; }
      .trait-switch .form-check-input { margin-top: 0; }
      .trait-switch .form-check-label { font-size: 0.74rem; line-height: 1; }
      .trait-slider-wrap .irs { max-width: 210px; margin: 0 auto; }
      .trait-slider-wrap .irs-line, .trait-slider-wrap .irs-bar { height: 6px; }
      .trait-slider-wrap .irs-handle { transform: scale(0.9); }
      .pref-card .form-select { max-width: 100%; }
      .radio-btn-group .btn { min-width: 88px; white-space: nowrap; }
      .sidebar-card .card-header, .detail-panel h3 { font-weight: 700; }
      .app-grid { display: grid; grid-template-columns: 430px minmax(340px, 1fr) minmax(360px, 1.1fr); gap: 1rem; align-items: start; min-width: 1210px; }
      .app-grid > div { min-width: 0; }
      .center-panel, .right-panel { min-width: 0; width: 100%; }
      .app-grid-scroll { width: 100%; overflow-x: auto; overflow-y: visible; padding-bottom: 0.25rem; }
      @media (max-width: 1400px) {
        .app-grid { grid-template-columns: 430px minmax(320px, 1fr) minmax(340px, 1fr); min-width: 1160px; }
      }
      @media (max-width: 1180px) {
        .app-grid { grid-template-columns: 400px minmax(300px, 1fr) minmax(320px, 1fr); min-width: 1080px; }
        .detail-panel { position: static; }
      }
      .sidebar-wrap { width: 100%; min-width: 0; }
      .sidebar-card { overflow: visible; }
      .pref-card { overflow: visible; }
      .trait-slider-wrap { min-width: 0; max-width: 210px; margin: 0 auto; }
      .accom-wrap { display: grid; gap: 0.4rem; margin-bottom: 0.75rem; }
      .pref-subtitle { font-weight: 700; font-size: 1rem; line-height: 1.1; margin-bottom: 0.08rem; }
      .trait-slider-wrap .form-group { margin-bottom: 0; }
      .trait-slider-wrap .irs, .trait-slider-wrap .js-irs-0 { margin-top: 0; }
      .trait-slider-labels { display: flex; justify-content: space-between; gap: 0.3rem; margin-top: 0.05rem; }
      .trait-slider-labels span:nth-child(2) { text-align: center; flex: 1; }
      .trait-slider-labels span:first-child, .trait-slider-labels span:last-child { min-width: 62px; }
    "))
                 ),
                 div(
                   class = "py-3",
                   h1("Dog breed recommender", class = "app-title"),
                   p("Set your accommodation, choose which characteristics you prefer high / low / none, and mark any that are very important.", class = "text-secondary-emphasis")
                 ),
                 div(
                   class = "app-grid-scroll",
                   div(
                     class = "app-grid",
                     
                     div(
                       class = "sidebar-wrap",
                       card(
                         class = "sidebar-card bg-dark text-light border-secondary-subtle",
                         card_header(class = "bg-primary text-white", "Preferences"),
                         card_body(
                           div(
                             class = "pref-card bg-dark text-light",
                             div(
                               class = "accom-wrap",
                               div(class = "pref-subtitle text-light", "Accommodation"),
                               selectInput(
                                 "space_filter",
                                 label = NULL,
                                 choices = c(
                                   "Apartment no balcony" = "apartment_friendly",
                                   "Apartment with a balcony" = "apartment_ok",
                                   "Small house and a yard" = "small_house",
                                   "Large house and a yard" = "large_house",
                                   "Ample outdoor space" = "outdoor_compulsory"
                                 ),
                                 selected = "small_house"
                               )
                             ),
                             div(
                               class = "trait-grid",
                               trait_controls("lifespan", "Lifespan"),
                               trait_controls("energy", "Energy"),
                               trait_controls("temperament", "Temperament"),
                               trait_controls("friendliness", "Friendliness"),
                               trait_controls("trainability", "Trainability")
                             ),
                             div(class = "d-grid mt-3", actionButton("submit_btn", "Find breeds", class = "btn btn-primary btn-lg w-100"))
                           )
                         )
                       )
                     ),
                     
                     div(
                       class = "center-panel",
                       p("Top ranked breeds are shown below. Click a breed card to view detailed information and the radar plot on the right.", class = "text-secondary-emphasis top-note"),
                       uiOutput("top5_ui"),
                       div(class = "d-grid gap-2 mb-3", uiOutput("expand_btn_ui")),
                       uiOutput("top610_ui")
                     ),
                     
                     div(
                       class = "right-panel",
                       div(
                         class = "detail-panel card bg-dark border-secondary-subtle text-light",
                         uiOutput("detail_ui")
                       )
                     )
                   )
                 )
)

# =========================
# Server
# =========================

server <- function(input, output, session) {
  dogs <- read_breed_data("data_app/merged.csv")
  selected_breed <- reactiveVal(NULL)
  show_more <- reactiveVal(FALSE)
  
  observeEvent(input$expand_btn, {
    show_more(!show_more())
  })
  
  build_weights <- reactive({
    c(
      lifespan = ifelse(isTRUE(input$lifespan_important), 2, 1),
      energy = ifelse(isTRUE(input$energy_important), 2, 1),
      temperament = ifelse(isTRUE(input$temperament_important), 2, 1),
      friendliness = ifelse(isTRUE(input$friendliness_important), 2, 1),
      trainability = ifelse(isTRUE(input$trainability_important), 2, 1)
    )
  })
  
  ranked <- eventReactive(input$submit_btn, {
    rank_breeds(
      data = dogs,
      space_filter = input$space_filter,
      lifespan_pref = slider_pref_to_choice(input$lifespan_pref),
      energy_pref = slider_pref_to_choice(input$energy_pref),
      temperament_pref = slider_pref_to_choice(input$temperament_pref),
      friendliness_pref = slider_pref_to_choice(input$friendliness_pref),
      trainability_pref = slider_pref_to_choice(input$trainability_pref),
      weights = build_weights(),
      top_n = 10
    )
  }, ignoreInit = FALSE)
  
  observeEvent(ranked(), {
    if (nrow(ranked()) > 0) selected_breed(ranked()$breed[[1]])
  })
  
  
  output$expand_btn_ui <- renderUI({
    if (input$submit_btn < 1) return(NULL)
    req(ranked())
    if (nrow(ranked()) <= 5) return(NULL)
    actionButton(
      "expand_btn",
      if (show_more()) "less" else "more",
      class = "btn btn-outline-secondary"
    )
  })
  
  make_card_ui <- function(df, id_prefix) {
    div(
      class = "results-card card bg-dark border-secondary-subtle text-light",
      tagList(
        lapply(seq_len(nrow(df)), function(i) {
          row <- df[i, , drop = FALSE]
          info <- get_breed_assets(row$breed[[1]])
          btnid <- paste0(id_prefix, "_pick_", i)
          
          row_ui <- div(
            class = "result-row",
            actionButton(
              inputId = btnid,
              label = HTML(sprintf(
                '<div class="result-pill-wrap"><img class="result-avatar border border-3 border-primary-subtle bg-dark-subtle" src="%s" alt="%s"><div class="result-bar bg-primary-subtle border border-primary-subtle text-light"><div><div class="breed-name">%s</div></div></div><div class="score-orb bg-primary text-white border border-3 border-primary-subtle"><div class="score-rank">%s</div><div class="score-small">%s</div></div></div>',
                info$image,
                row$breed[[1]],
                row$breed[[1]],
                row$rank[[1]],
                fmt2(row$overall_score[[1]])
              )),
              class = "breed-pick-btn"
            )
          )
          
          local({
            local_btnid <- btnid
            local_row <- row
            observeEvent(input[[local_btnid]], {
              selected_breed(local_row$breed[[1]])
            }, ignoreInit = TRUE)
          })
          
          row_ui
        })
      )
    )
  }
  
  output$top5_ui <- renderUI({
    req(ranked())
    top5 <- ranked() %>% slice(1:min(5, n()))
    make_card_ui(top5, "top")
  })
  
  output$top610_ui <- renderUI({
    req(ranked())
    if (!show_more()) return(NULL)
    more <- ranked() %>% slice(6:min(10, n()))
    if (nrow(more) == 0) return(NULL)
    tagList(
      make_card_ui(more, "more")
    )
  })
  
  
  output$detail_radar <- renderPlot({
    req(ranked())
    breed <- selected_breed()
    req(!is.null(breed))
    row <- ranked() %>% filter(breed == !!breed) %>% slice(1)
    req(nrow(row) == 1)
    radar_plot(row)
  })
  
  output$detail_ui <- renderUI({
    req(ranked())
    breed <- selected_breed()
    if (is.null(breed)) {
      return(tagList(
        h3("Breed details"),
        p("Click a breed name or picture to show its profile.")
      ))
    }
    
    row <- ranked() %>% filter(breed == !!breed) %>% slice(1)
    req(nrow(row) == 1)
    
    info <- get_breed_assets(breed)
    intro <- make_intro_text(row, info$summary)
    
    tagList(
      h3(row$breed[[1]]),
      tags$img(src = info$image, alt = row$breed[[1]]),
      plotOutput("detail_radar", height = "300px"),
      tags$br(),
      p(HTML(sprintf("<strong>Overall score:</strong> %s/1.00", fmt2(row$overall_score[[1]])))),
      tags$ul(
        tags$li(HTML(sprintf("<strong>Median lifespan:</strong> %s years", fmt1(row$Median.Survival[[1]])))),
        tags$li(HTML(sprintf("<strong>Energy:</strong> %s/5.0 (%s)", fmt1(row$energy_raw[[1]]), energy_label(row$energy_raw[[1]])))),
        tags$li(HTML(sprintf("<strong>Temperament:</strong> %s/5.0 (%s)", fmt1(row$temperament_score[[1]]), trait_label(row$temperament_score[[1]])))),
        tags$li(HTML(sprintf("<strong>Friendliness:</strong> %s/5.0 (%s)", fmt1(row$friendliness_score[[1]]), trait_label(row$friendliness_score[[1]])))),
        tags$li(HTML(sprintf("<strong>Trainability:</strong> %s/5.0 (%s)", fmt1(row$trainability_score[[1]]), trait_label(row$trainability_score[[1]])))),
        tags$li(HTML(sprintf("<strong>Accommodation:</strong> %s", space_label(row$space_category[[1]]))))
      ),
      HTML(paste0("<p>", gsub("\n", "<br><br>", as.character(intro)), "</p>"))
    )
  })
}

shinyApp(ui, server)

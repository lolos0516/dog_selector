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
    x == "apartment_friendly" ~ "Apartment-friendly",
    x == "apartment_ok" ~ "Apartment OK",
    x == "small_house" ~ "Small house + small yard",
    x == "large_house" ~ "Large house + large yard",
    x == "outdoor_compulsory" ~ "Outdoor compulsory",
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
  plot_dat <- tibble::tibble(
    trait = c("Lifespan", "Energy", "Temperament", "Friendliness", "Trainability"),
    value = c(
      row_df$lifespan_match,
      row_df$energy_match,
      row_df$temperament_match,
      row_df$friendliness_match,
      row_df$trainability_match
    )
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
      fill = scales::alpha("#0d6efd", 0.5),
      colour = "#0d6efd",
      linewidth = 1
    ) +
    geom_point(
      data = plot_dat,
      aes(x = x, y = y),
      colour = "#0d6efd",
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

trait_controls <- function(prefix, label) {
  tagList(
    div(class = "border rounded-4 p-3 mb-3 bg-white",
        h5(label, class = "mb-2"),
        selectInput(
          inputId = paste0(prefix, "_pref"),
          label = "Preference",
          choices = c("High" = "high", "None" = "none", "Low" = "low"),
          selected = "none"
        ),
        checkboxInput(
          inputId = paste0(prefix, "_important"),
          label = "Very important",
          value = FALSE
        )
    )
  )
}

ui <- page_fluid(
  theme = theme,
  tags$head(
    tags$style(HTML("
      body { background-color: #f7fafc; }
      .app-title { font-weight: 700; margin-bottom: 0.2rem; }
      .muted-note { color: #6c757d; }
      .breed-card { border: 1px solid #e9ecef; border-radius: 1.2rem; background: white; padding: 1rem; margin-bottom: 1rem; box-shadow: 0 0.25rem 0.8rem rgba(0,0,0,0.04); }
      .breed-pick-btn { width: 100%; text-align: left; border: none; background: transparent; padding: 0; }
      .breed-pick-btn img { display: block; width: auto; max-width: 100%; height: auto; max-height: 220px; object-fit: contain; border-radius: 1rem; margin-bottom: 0.75rem; margin-left: auto; margin-right: auto; }
      .breed-name { font-size: 1.15rem; font-weight: 700; color: #0d6efd; margin-bottom: 0.25rem; }
      .score-badge { display: inline-block; background: #eef6ff; color: #0d6efd; border-radius: 999px; padding: 0.2rem 0.65rem; font-weight: 600; margin-bottom: 0.5rem; }
      .detail-panel { border: 1px solid #e9ecef; border-radius: 1.2rem; background: white; padding: 1rem; min-height: 640px; box-shadow: 0 0.25rem 0.8rem rgba(0,0,0,0.04); }
      .detail-panel img { display: block; width: auto; max-width: 100%; height: auto; max-height: 320px; object-fit: contain; border-radius: 1rem; margin-bottom: 0.75rem; margin-left: auto; margin-right: auto; }
      .top-note { font-size: 0.95rem; }
    "))
  ),
  div(
    class = "py-3",
    h1("Dog breed recommender", class = "app-title"),
    p("Set your accommodation, choose which characteristics you prefer high / low / none, and mark any that are very important.", class = "muted-note")
  ),
  layout_columns(
    col_widths = c(3, 5, 4),
    
    # Left controls
    card(
      card_header("Preferences"),
      card_body(
        selectInput(
          "space_filter",
          "Accommodation",
          choices = c(
            "Apartment no balcony" = "apartment_friendly",
            "Apartment with a balcony" = "apartment_ok",
            "Small house and back yard" = "small_house",
            "Large house and back yard" = "large_house",
            "Ample outdoor space" = "outdoor_compulsory"
          ),
          selected = "small_house"
        ),
        trait_controls("lifespan", "Lifespan"),
        trait_controls("energy", "Energy"),
        trait_controls("temperament", "Temperament"),
        trait_controls("friendliness", "Friendliness"),
        trait_controls("trainability", "Trainability"),
        actionButton("submit_btn", "Find breeds", class = "btn-primary btn-lg w-100")
      )
    ),
    
    # Center results
    div(
      p("Top ranked breeds are shown below. The radar plot uses standardized match scores for your current preferences.", class = "muted-note top-note"),
      uiOutput("top5_ui"),
      div(class = "d-grid gap-2 mb-3", actionButton("expand_btn", "Expand to show ranks 6–10", class = "btn-outline-secondary")),
      uiOutput("top610_ui")
    ),
    
    # Right details
    div(
      class = "detail-panel",
      uiOutput("detail_ui")
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
      lifespan_pref = input$lifespan_pref,
      energy_pref = input$energy_pref,
      temperament_pref = input$temperament_pref,
      friendliness_pref = input$friendliness_pref,
      trainability_pref = input$trainability_pref,
      weights = build_weights(),
      top_n = 10
    )
  }, ignoreInit = FALSE)
  
  observeEvent(ranked(), {
    if (nrow(ranked()) > 0) selected_breed(ranked()$breed[[1]])
  })
  
  make_card_ui <- function(df, id_prefix) {
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        info <- get_breed_assets(row$breed[[1]])
        plotname <- paste0(id_prefix, "_plot_", i)
        btnid <- paste0(id_prefix, "_pick_", i)
        
        card_ui <- div(
          class = "breed-card",
          actionButton(
            inputId = btnid,
            label = HTML(sprintf(
              '<div style="height:220px;display:flex;align-items:center;justify-content:center;overflow:hidden;"><img src="%s" alt="%s"></div><div class="breed-name">%s</div>',
              info$image, row$breed[[1]], row$breed[[1]]
            )),
            class = "breed-pick-btn"
          ),
          div(class = "score-badge", paste("Overall score", fmt2(row$overall_score[[1]]))),
          p(
            HTML(sprintf(
              "<strong>Rank:</strong> %s &nbsp; | &nbsp; <strong>Space:</strong> %s",
              row$rank[[1]], space_label(row$space_category[[1]])
            ))
          ),
          plotOutput(plotname, height = "220px")
        )
        
        local({
          ii <- i
          local_plotname <- plotname
          local_btnid <- btnid
          local_row <- row
          
          output[[local_plotname]] <- renderPlot({
            radar_plot(local_row)
          })
          
          observeEvent(input[[local_btnid]], {
            selected_breed(local_row$breed[[1]])
          }, ignoreInit = TRUE)
        })
        
        card_ui
      })
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
      h4("More breeds", class = "mt-3 mb-3"),
      make_card_ui(more, "more")
    )
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
      p(HTML(sprintf("<strong>Overall score:</strong> %s", fmt2(row$overall_score[[1]])))),
      tags$ul(
        tags$li(HTML(sprintf("<strong>Median lifespan:</strong> %s years", fmt1(row$Median.Survival[[1]])))),
        tags$li(HTML(sprintf("<strong>Energy:</strong> %s (%s)", fmt1(row$energy_raw[[1]]), energy_label(row$energy_raw[[1]])))),
        tags$li(HTML(sprintf("<strong>Temperament:</strong> %s (%s)", fmt1(row$temperament_score[[1]]), trait_label(row$temperament_score[[1]])))),
        tags$li(HTML(sprintf("<strong>Friendliness:</strong> %s (%s)", fmt1(row$friendliness_score[[1]]), trait_label(row$friendliness_score[[1]])))),
        tags$li(HTML(sprintf("<strong>Trainability:</strong> %s (%s)", fmt1(row$trainability_score[[1]]), trait_label(row$trainability_score[[1]])))),
        tags$li(HTML(sprintf("<strong>Accommodation:</strong> %s", space_label(row$space_category[[1]]))))
      ),
      HTML(paste0("<p>", gsub("\n", "<br><br>", as.character(intro)), "</p>"))
    )
  })
}

shinyApp(ui, server)

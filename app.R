# app.R -----------------------------------------------------------------
# Health Progress in South Asia Dashboard (2000–2020)
#
# This Shiny app is aligned with the analysis described in:
#   1. Yearly Trends by Country
#   2. India's (or chosen country's) Value vs Regional IQR
#   3. Country vs South Asia Median
#   4. Distributions by Indicator
#   5. Immunization vs Child Mortality
#   6. Health Expenditure vs Life Expectancy
#   7. Regional Deviation Heatmap
#
# Dataset expectations:
#   - country column like Country.Name / country_name / country / country_code
#   - indicator column like Series.Name / series_name / indicator_name / indicator
#   - year columns named 2000..2025 OR x2000..x2025
#
# The app:
#   - loads your CSV (upload or auto-detect local .csv)
#   - reshapes to long tidy form: country_name / series_name / year / value
#   - interactive 3x3 indicator grids for Trends, IQR vs Region, Median vs Region
#   - distribution histograms by year
#   - scatter plots
#   - deviation heatmap vs region median

# ---- packages ----
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(readr)
library(stringr)
library(fontawesome)
library(patchwork)

theme_set(theme_minimal(base_size = 13))

# ---- helpers: detect, load, reshape -----------------------------------

guess_year_cols <- function(nms) {
  # detect "2001", "x2001", etc.
  nms[str_detect(nms, "^(x)?\\d{4}$")]
}

load_data_safely <- function(path) {
  if (is.null(path) || !length(path) || !file.exists(path)) return(NULL)
  
  out <- tryCatch(
    suppressMessages(readr::read_csv(path, show_col_types = FALSE)),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  out <- tryCatch(
    read.csv(path, check.names = FALSE),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  NULL
}

reshape_long_format <- function(df_wide) {
  # snake_case everything
  df_wide <- df_wide %>% clean_names()
  
  # rename x2001 -> 2001
  raw_year_like <- guess_year_cols(names(df_wide))
  if (length(raw_year_like) > 0) {
    df_wide <- df_wide %>%
      rename_with(~ str_replace(.x, "^x", ""), all_of(raw_year_like))
  }
  
  # pure 4-digit year cols
  yr_cols <- names(df_wide)[str_detect(names(df_wide), "^\\d{4}$")]
  
  # guess country column
  all_cols <- names(df_wide)
  country_col <- dplyr::case_when(
    "country_name"   %in% all_cols ~ "country_name",
    "country.name"   %in% all_cols ~ "country.name",
    "country"        %in% all_cols ~ "country",
    "country_code"   %in% all_cols ~ "country_code",
    TRUE                           ~ NA_character_
  )
  
  # guess indicator/series column
  series_col <- dplyr::case_when(
    "series_name"     %in% all_cols ~ "series_name",
    "series.name"     %in% all_cols ~ "series.name",
    "indicator_name"  %in% all_cols ~ "indicator_name",
    "indicator"       %in% all_cols ~ "indicator",
    TRUE                            ~ NA_character_
  )
  
  validate(
    need(!is.na(country_col),
         "Couldn't detect country column (expected something like Country.Name / country_name)."),
    need(!is.na(series_col),
         "Couldn't detect indicator/series column (expected Series.Name / series_name / indicator_name)."),
    need(length(yr_cols) > 0,
         "Couldn't detect year columns (expected 2000..2025 or x2000..x2025).")
  )
  
  # long tidy form
  df_long <- df_wide %>%
    pivot_longer(
      cols      = all_of(yr_cols),
      names_to  = "year",
      values_to = "value"
    ) %>%
    mutate(
      year  = suppressWarnings(as.numeric(year)),
      value = suppressWarnings(as.numeric(value))
    )
  
  # normalize final column names
  if (!"country_name" %in% names(df_long)) {
    df_long <- df_long %>%
      rename(country_name = !!rlang::sym(country_col))
  }
  if (!"series_name" %in% names(df_long)) {
    df_long <- df_long %>%
      rename(series_name = !!rlang::sym(series_col))
  }
  
  df_long %>%
    mutate(
      country_name = trimws(as.character(country_name)),
      series_name  = trimws(as.character(series_name))
    )
}

# ---- theme config -----------------------------------------------------

app_theme <- bs_theme(
  bootswatch   = "flatly",
  primary      = "#20a29a",
  secondary    = "#2c3e50",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter Tight"),
  code_font    = font_google("JetBrains Mono")
)

# ---- UI ---------------------------------------------------------------

ui <- page_navbar(
  theme = app_theme,
  title = tags$div(
    class = "d-flex align-items-center gap-2",
    tags$span(class = "fw-bold", "Health Progress in South Asia"),
    tags$span(style = "font-size:0.8rem;color:#89c9c5;", "2000–2020")
  ),
  
  nav(
    "Dashboard",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        
        # data load
        tags$div(
          class = "mb-3",
          tags$h5(
            class="fw-semibold mb-2",
            fa("database"),
            " Data"
          ),
          fileInput(
            "file",
            NULL,
            buttonLabel = "Upload CSV",
            placeholder = "No file selected",
            accept = ".csv"
          ),
          tags$small(
            class="text-muted",
            "If nothing uploaded, app tries:",
            tags$code("wider_cleaned_data.csv"), ", ",
            tags$code("cleaned_data.csv"),
            ", or first .csv in working directory."
          )
        ),
        
        tags$hr(),
        
        # country highlight selector
        tags$div(
          class = "mb-3",
          tags$h6(
            class="fw-semibold mb-2",
            fa("globe-asia"),
            " Country filters"
          ),
          uiOutput("country_ui"),
          checkboxInput(
            "show_all_countries",
            label = "Show all countries",
            value = TRUE
          ),
          tags$small(
            class="text-muted",
            "If off, only selected countries are shown in Trends."
          )
        ),
        
        # indicators selector
        tags$div(
          class = "mb-3",
          tags$h6(
            class="fw-semibold mb-2",
            fa("chart-line"),
            " Indicators"
          ),
          uiOutput("series_ui"),
          tags$small(
            class="text-muted",
            "Up to first 9 indicators are shown in Trends / Comparison grids."
          )
        ),
        
        tags$hr(),
        tags$div(
          class="text-muted small",
          fa("info-circle"),
          " Tip: collapse this sidebar for a wider plot area."
        )
      ),
      
      # main body
      tags$div(
        class="container-fluid",
        
        # summary cards row
        layout_columns(
          col_widths = c(4,8),
          
          # dataset overview card
          card(
            full_screen = TRUE,
            card_header(
              class="bg-primary text-white",
              tags$div(
                class="fw-semibold",
                fa("clipboard-list"),
                " Dataset overview"
              )
            ),
            card_body(
              htmlOutput("summary_html"),
              class = "p-2",
              style = "font-size:0.9rem; line-height:1.4; max-height:260px; overflow-y:auto;"
            )
          ),
          
          # preview card
          card(
            full_screen = TRUE,
            card_header(
              class="bg-primary text-white",
              tags$div(
                class="fw-semibold",
                fa("table"),
                " Preview (first 8 rows)"
              )
            ),
            card_body(
              div(
                style="max-height:260px; overflow-y:auto;",
                tableOutput("peek_tbl")
              ),
              class = "p-2"
            )
          )
        ),
        
        tags$br(),
        
        # plot tabs
        card(
          full_screen = TRUE,
          card_header(
            class="bg-primary text-white d-flex justify-content-between align-items-center flex-wrap gap-2",
            tags$div(
              class="fw-semibold",
              fa("chart-area"),
              " Charts"
            ),
            tags$span(
              class="text-white-50 small",
              "Time trends, comparisons & relationships"
            )
          ),
          
          card_body(
            navset_pill(
              id = "plot_tabs",
              
              # TAB 1: Yearly Trends by Country (Figure 1)
              nav_panel(
                fa("chart-line", fill=TRUE), "Trends (3x3 grid)",
                div(
                  class="text-muted small mb-2",
                  "Up to 9 indicators. Each mini-plot = line over time, color by country (if multiple)."
                ),
                plotOutput("plot_trend_grid", height = "900px")
              ),
              
              # TAB 2: Country vs Regional IQR (Figure 2)
              nav_panel(
                fa("layer-group"), "Country vs IQR",
                div(
                  class="text-muted small mb-2",
                  "Red line = selected country. Gray band = regional IQR (25th–75th). Dashed = regional median."
                ),
                fluidRow(
                  column(
                    width = 4,
                    uiOutput("iqr_country_ui")
                  ),
                  column(
                    width = 8,
                    plotOutput("plot_iqr_grid", height = "900px")
                  )
                )
              ),
              
              # TAB 3: Country vs Regional Median (Figure 3)
              nav_panel(
                fa("flag"), "Country vs Median",
                div(
                  class="text-muted small mb-2",
                  "Red line = selected country. Dashed gray = regional median. Up to 9 indicators."
                ),
                fluidRow(
                  column(
                    width = 4,
                    uiOutput("median_country_ui")
                  ),
                  column(
                    width = 8,
                    plotOutput("plot_median_grid", height = "900px")
                  )
                )
              ),
              
              # TAB 4: Distributions by Indicator (Figure 4)
              nav_panel(
                fa("chart-bar"), "Distribution",
                div(
                  class="text-muted small mb-2",
                  "Histogram across countries for a single year, for one indicator.
                   Helps see spread, outliers, inequality."
                ),
                fluidRow(
                  column(
                    width = 4,
                    uiOutput("dist_indicator_ui"),
                    uiOutput("dist_year_ui")
                  ),
                  column(
                    width = 8,
                    plotOutput("plot_dist_hist", height = "500px")
                  )
                )
              ),
              
              # TAB 5: Immunization vs Child Mortality (Figure 5)
              nav_panel(
                fa("syringe"), "Immunization vs Child Mortality",
                div(
                  class="text-muted small mb-2",
                  "Scatter: DPT immunization vs under-5 mortality. Linear fit shows negative relationship."
                ),
                plotOutput("plot_imm_mort", height = "500px")
              ),
              
              # TAB 6: Health Expenditure vs Life Expectancy (Figure 6)
              nav_panel(
                fa("heartbeat"), "Spend vs Life Expectancy",
                div(
                  class="text-muted small mb-2",
                  "Scatter: current health expenditure (% of GDP) vs life expectancy. Linear fit shown."
                ),
                plotOutput("plot_exp_le", height = "500px")
              ),
              
              # TAB 7: Regional Deviation Heatmap (Figure 7)
              nav_panel(
                fa("th"), "Deviation Heatmap",
                div(
                  class="text-muted small mb-2",
                  "For a selected year: how far each country is above/below regional median, for each indicator."
                ),
                fluidRow(
                  column(
                    width = 4,
                    uiOutput("heat_year_ui"),
                    tags$small(
                      class="text-muted",
                      "Red = below regional median, Blue = above."
                    )
                  ),
                  column(
                    width = 8,
                    plotOutput("plot_dev_heatmap", height = "650px")
                  )
                )
              )
            )
          )
        ),
        
        tags$footer(
          class="mt-4 text-center text-muted small",
          tags$hr(),
          "Health Progress in South Asia · Shiny + bslib · ",
          tags$span(style="opacity:0.7","2000–2020")
        )
      )
    )
  ),
  
  # ABOUT TAB
  nav(
    "About",
    card(
      full_screen = TRUE,
      card_header(
        class="bg-primary text-white",
        tags$div(
          class="fw-semibold d-flex align-items-center gap-2",
          fa("info-circle"),
          " About the data & methods"
        )
      ),
      card_body(
        class = "p-4",
        htmlOutput("about_html")
      )
    )
  )
)

# ---- SERVER -----------------------------------------------------------

server <- function(input, output, session) {
  
  # 1. LOAD DATA --------------------------------------------------------
  raw_data <- reactive({
    # 1. upload
    if (!is.null(input$file)) {
      dat_up <- load_data_safely(input$file$datapath)
      if (!is.null(dat_up)) {
        message("Loaded: uploaded CSV")
        return(dat_up)
      }
    }
    # 2. try common names
    if (file.exists("wider_cleaned_data.csv")) {
      dat_wide <- load_data_safely("wider_cleaned_data.csv")
      if (!is.null(dat_wide)) {
        message("Loaded: wider_cleaned_data.csv")
        return(dat_wide)
      }
    }
    if (file.exists("cleaned_data.csv")) {
      dat_clean <- load_data_safely("cleaned_data.csv")
      if (!is.null(dat_clean)) {
        message("Loaded: cleaned_data.csv")
        return(dat_clean)
      }
    }
    # 3. first .csv in directory
    csv_candidates <- list.files(pattern = "\\.csv$", ignore.case = TRUE)
    if (length(csv_candidates) > 0) {
      for (p in csv_candidates) {
        try_df <- load_data_safely(p)
        if (!is.null(try_df)) {
          message("Loaded fallback:", p)
          return(try_df)
        }
      }
    }
    # 4. interactive fallback if running locally
    if (interactive()) {
      chosen <- tryCatch(file.choose(), error = function(e) NULL)
      if (!is.null(chosen)) {
        picked <- load_data_safely(chosen)
        if (!is.null(picked)) {
          message("Loaded via file.choose():", chosen)
          return(picked)
        }
      }
    }
    NULL
  })
  
  data_long <- reactive({
    req(raw_data())
    reshape_long_format(raw_data())
  })
  
  multi_country <- reactive({
    req(data_long())
    dplyr::n_distinct(data_long()$country_name) > 1
  })
  
  all_indicators <- reactive({
    req(data_long())
    sort(unique(data_long()$series_name))
  })
  
  all_countries <- reactive({
    req(data_long())
    sort(unique(data_long()$country_name))
  })
  
  all_years <- reactive({
    req(data_long())
    sort(unique(data_long()$year))
  })
  
  # 2. SUMMARY / PREVIEW ------------------------------------------------
  output$summary_html <- renderUI({
    d <- data_long()
    
    n_rows <- nrow(d)
    n_cols <- ncol(d)
    
    all_countries_v <- sort(unique(d$country_name))
    n_ctry <- length(all_countries_v)
    ctry_show <- all_countries_v[1:min(10, n_ctry)]
    ctry_more <- if (n_ctry > 10) paste0(" +", n_ctry - 10, " more") else ""
    
    all_series_v <- sort(unique(d$series_name))
    n_series <- length(all_series_v)
    ser_show <- all_series_v[1:min(10, n_series)]
    ser_more <- if (n_series > 10) paste0(" +", n_series - 10, " more") else ""
    
    yr_rng <- range(d$year, na.rm = TRUE)
    yr_txt <- if (is.finite(yr_rng[1]) && is.finite(yr_rng[2])) {
      paste0(yr_rng[1], "–", yr_rng[2])
    } else {
      "NA"
    }
    
    val_stats <- summary(d$value)
    val_block <- paste(
      names(val_stats),
      format(round(as.numeric(val_stats), 3), nsmall = 3),
      collapse = "<br/>"
    )
    
    HTML(paste0(
      "<div>",
      "<div style='margin-bottom:0.5rem;'><b>Rows:</b> ", n_rows,
      " &nbsp; | &nbsp; <b>Columns:</b> ", n_cols, "</div>",
      "<div style='margin-bottom:0.5rem;'><b>Countries (", n_ctry, "):</b><br/>",
      paste(ctry_show, collapse = ", "), ctry_more, "</div>",
      "<div style='margin-bottom:0.5rem;'><b>Indicators (", n_series, "):</b><br/>",
      paste(ser_show, collapse = ", "), ser_more, "</div>",
      "<div style='margin-bottom:0.5rem;'><b>Year range in data:</b> ", yr_txt, "</div>",
      "<div style='margin-bottom:0.5rem;'><b>Value summary:</b><br/>", val_block, "</div>",
      "</div>"
    ))
  })
  
  output$peek_tbl <- renderTable({
    head(data_long(), 8)
  },
  striped  = TRUE,
  hover    = TRUE,
  bordered = TRUE)
  
  # 3. SIDEBAR INPUTS ---------------------------------------------------
  output$country_ui <- renderUI({
    d <- data_long()
    countries <- sort(unique(d$country_name))
    def <- if ("India" %in% countries) "India" else countries[1]
    selectInput(
      "country",
      label    = "Highlight countries (for Trends)",
      choices  = unique(c("India", countries)),
      selected = def,
      multiple = TRUE
    )
  })
  
  output$series_ui <- renderUI({
    sers <- all_indicators()
    selectizeInput(
      "series",
      "Indicators / variables",
      choices  = sers,
      selected = sers[1:min(6, length(sers))],
      multiple = TRUE,
      options = list(
        placeholder = 'Select indicators (top 9 used in grid)',
        plugins = list('remove_button')
      )
    )
  })
  
  # country selector for IQR tab
  output$iqr_country_ui <- renderUI({
    countries <- all_countries()
    def <- if ("India" %in% countries) "India" else countries[1]
    selectInput(
      "iqr_country",
      label    = "Country for IQR comparison:",
      choices  = countries,
      selected = def,
      multiple = FALSE
    )
  })
  
  # country selector for Median tab
  output$median_country_ui <- renderUI({
    countries <- all_countries()
    def <- if ("India" %in% countries) "India" else countries[1]
    selectInput(
      "median_country",
      label    = "Country for median comparison:",
      choices  = countries,
      selected = def,
      multiple = FALSE
    )
  })
  
  # distribution tab controls
  output$dist_indicator_ui <- renderUI({
    inds <- all_indicators()
    selectInput(
      "dist_indicator",
      label = "Indicator for distribution histogram:",
      choices = inds,
      selected = inds[1],
      multiple = FALSE
    )
  })
  
  output$dist_year_ui <- renderUI({
    yrs <- all_years()
    def_yr <- tail(yrs, 1)
    selectInput(
      "dist_year",
      label = "Year:",
      choices = yrs,
      selected = def_yr,
      multiple = FALSE
    )
  })
  
  # heatmap year selector
  output$heat_year_ui <- renderUI({
    yrs <- all_years()
    def_yr <- tail(yrs, 1)
    selectInput(
      "heat_year",
      label = "Select year for deviation heatmap:",
      choices = yrs,
      selected = def_yr,
      multiple = FALSE
    )
  })
  
  # 4. INTERNAL HELPERS FOR PLOT GRIDS ----------------------------------
  
  # first up to 9 indicators in the same order as user selected
  selected_9_indicators <- reactive({
    req(input$series)
    inds <- unique(input$series)
    inds[1:min(9, length(inds))]
  })
  
  # Trend mini-plot: raw lines per country
  make_trend_plot <- function(dat, indicator, multi_flag, show_legend = FALSE) {
    dd <- dat %>% filter(series_name == indicator)
    ggplot(
      dd,
      aes(
        x = year,
        y = value,
        color = if (multi_flag) country_name else NULL,
        group = country_name
      )
    ) +
      geom_line(linewidth = 0.9, alpha = 0.95, na.rm = TRUE) +
      labs(
        title = indicator,
        x = NULL,
        y = NULL,
        color = if (show_legend && multi_flag) "Country" else NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 11),
        legend.position = if (show_legend) "right" else "none",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank()
      )
  }
  
  # IQR mini-plot: ribbon (IQR), dashed median, red country
  make_iqr_plot <- function(dat, indicator, country_sel) {
    dd <- dat %>% filter(series_name == indicator)
    stats_df <- dd %>%
      group_by(year) %>%
      summarize(
        p25 = quantile(value, 0.25, na.rm = TRUE),
        p75 = quantile(value, 0.75, na.rm = TRUE),
        med = median(value, na.rm = TRUE),
        .groups = "drop"
      )
    cntry_df <- dd %>%
      filter(country_name == country_sel)
    
    ggplot() +
      geom_ribbon(
        data = stats_df,
        aes(x = year, ymin = p25, ymax = p75),
        fill = "gray70",
        alpha = 0.4
      ) +
      geom_line(
        data = stats_df,
        aes(x = year, y = med),
        linetype = "dashed",
        color = "gray30",
        linewidth = 0.8
      ) +
      geom_line(
        data = cntry_df,
        aes(x = year, y = value),
        color = "#e74c3c",
        linewidth = 0.9
      ) +
      labs(
        title = indicator,
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 11),
        legend.position = "none",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank()
      )
  }
  
  # Median mini-plot: dashed median, red country
  make_median_plot <- function(dat, indicator, country_sel) {
    dd <- dat %>% filter(series_name == indicator)
    stats_df <- dd %>%
      group_by(year) %>%
      summarize(
        med = median(value, na.rm = TRUE),
        .groups = "drop"
      )
    cntry_df <- dd %>%
      filter(country_name == country_sel)
    
    ggplot() +
      geom_line(
        data = stats_df,
        aes(x = year, y = med),
        linetype = "dashed",
        color = "gray30",
        linewidth = 0.8
      ) +
      geom_line(
        data = cntry_df,
        aes(x = year, y = value),
        color = "#e74c3c",
        linewidth = 0.9
      ) +
      labs(
        title = indicator,
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 11),
        legend.position = "none",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank()
      )
  }
  
  # 5. PLOTS ------------------------------------------------------------
  
  # FIGURE 1: Yearly Trends by Country (3x3 grid)
  output$plot_trend_grid <- renderPlot({
    d <- data_long()
    req(input$series)
    
    pd <- d %>%
      filter(series_name %in% input$series)
    
    # if user turned off "show all"
    if (!isTRUE(input$show_all_countries) && length(input$country) > 0) {
      pd <- pd %>% filter(country_name %in% input$country)
    }
    
    inds <- selected_9_indicators()
    validate(
      need(length(inds) > 0, "Select at least one indicator.")
    )
    
    plot_list <- lapply(seq_along(inds), function(i) {
      make_trend_plot(
        dat = pd,
        indicator = inds[i],
        multi_flag = multi_country(),
        show_legend = (i == 1)
      )
    })
    
    wrap_plots(plotlist = plot_list, ncol = 3)
  })
  
  # FIGURE 2: Country vs Regional IQR (3x3 grid)
  output$plot_iqr_grid <- renderPlot({
    d <- data_long()
    req(input$series, input$iqr_country)
    
    validate(
      need(multi_country(),
           "IQR comparison needs multiple countries in the data.")
    )
    
    inds <- selected_9_indicators()
    validate(
      need(length(inds) > 0, "Select at least one indicator.")
    )
    
    plot_list <- lapply(inds, function(ind) {
      make_iqr_plot(
        dat = d,
        indicator = ind,
        country_sel = input$iqr_country
      )
    })
    wrap_plots(plotlist = plot_list, ncol = 3)
  })
  
  # FIGURE 3: Country vs Regional Median (3x3 grid)
  output$plot_median_grid <- renderPlot({
    d <- data_long()
    req(input$series, input$median_country)
    
    validate(
      need(multi_country(),
           "Median comparison needs multiple countries in the data.")
    )
    
    inds <- selected_9_indicators()
    validate(
      need(length(inds) > 0, "Select at least one indicator.")
    )
    
    plot_list <- lapply(inds, function(ind) {
      make_median_plot(
        dat = d,
        indicator = ind,
        country_sel = input$median_country
      )
    })
    wrap_plots(plotlist = plot_list, ncol = 3)
  })
  
  # FIGURE 4: Distributions by Indicator (histogram for 1 yr)
  output$plot_dist_hist <- renderPlot({
    req(input$dist_indicator, input$dist_year)
    
    d <- data_long() %>%
      filter(
        series_name == input$dist_indicator,
        year == input$dist_year
      ) %>%
      filter(is.finite(value))
    
    validate(
      need(nrow(d) > 1,
           "Not enough data for that indicator/year to show distribution.")
    )
    
    ggplot(
      d,
      aes(x = value)
    ) +
      geom_histogram(
        bins = 20,
        fill = "#20a29a",
        alpha = 0.8,
        color = "white"
      ) +
      labs(
        title = paste0(
          "Distribution across countries (",
          input$dist_indicator,
          ", ",
          input$dist_year,
          ")"
        ),
        x = "Value",
        y = "Number of countries"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # FIGURE 5: Immunization vs Child Mortality
  output$plot_imm_mort <- renderPlot({
    d <- data_long()
    
    rel2 <- d %>%
      mutate(
        series_name  = trimws(series_name),
        country_name = trimws(country_name)
      ) %>%
      filter(series_name %in% c(
        "Immunization, DPT (% of children ages 12-23 months)",
        "Mortality rate, under-5 (per 1,000 live births)"
      )) %>%
      pivot_wider(
        id_cols     = c(country_name, year),
        names_from  = series_name,
        values_from = value
      ) %>%
      filter(
        !is.na(`Immunization, DPT (% of children ages 12-23 months)`),
        !is.na(`Mortality rate, under-5 (per 1,000 live births)`),
        is.finite(`Immunization, DPT (% of children ages 12-23 months)`),
        is.finite(`Mortality rate, under-5 (per 1,000 live births)`)
      )
    
    validate(
      need(nrow(rel2) > 0,
           "Required indicators not available in the dataset.")
    )
    
    ggplot(
      rel2,
      aes(
        x = `Immunization, DPT (% of children ages 12-23 months)`,
        y = `Mortality rate, under-5 (per 1,000 live births)`,
        color = country_name
      )
    ) +
      geom_point(size = 2.2, alpha = 0.85) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        linewidth = 1,
        color = "black"
      ) +
      labs(
        title = "Immunization vs under-5 mortality",
        x     = "DPT immunization rate (%)",
        y     = "Under-5 mortality (per 1,000 live births)",
        color = "Country"
      )
  })
  
  # FIGURE 6: Health Expenditure vs Life Expectancy
  output$plot_exp_le <- renderPlot({
    d <- data_long()
    
    rel <- d %>%
      filter(series_name %in% c(
        "Current health expenditure (% of GDP)",
        "Life expectancy at birth, total (years)"
      )) %>%
      pivot_wider(
        id_cols     = c(country_name, year),
        names_from  = series_name,
        values_from = value
      ) %>%
      filter(
        !is.na(`Current health expenditure (% of GDP)`),
        !is.na(`Life expectancy at birth, total (years)`),
        is.finite(`Current health expenditure (% of GDP)`),
        is.finite(`Life expectancy at birth, total (years)`)
      )
    
    validate(
      need(nrow(rel) > 0,
           "Required indicators not available in the dataset.")
    )
    
    ggplot(
      rel,
      aes(
        x = `Current health expenditure (% of GDP)`,
        y = `Life expectancy at birth, total (years)`,
        color = country_name
      )
    ) +
      geom_point(size = 2.2, alpha = 0.85) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        linewidth = 1,
        color = "black"
      ) +
      labs(
        title = "Health expenditure vs life expectancy",
        x     = "Health expenditure (% of GDP)",
        y     = "Life expectancy (years)",
        color = "Country"
      )
  })
  
  # FIGURE 7: Regional Deviation Heatmap
  # For selected year:
  #   deviation = country_value - regional_median (same indicator, same year)
  output$plot_dev_heatmap <- renderPlot({
    req(input$heat_year)
    
    d <- data_long() %>%
      filter(year == input$heat_year)
    
    validate(
      need(nrow(d) > 0,
           "No data for that year.")
    )
    
    # regional median for each indicator that year
    med_df <- d %>%
      group_by(series_name) %>%
      summarize(
        med_value = median(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    dev_df <- d %>%
      left_join(med_df, by = "series_name") %>%
      mutate(deviation = value - med_value) %>%
      filter(is.finite(deviation))
    
    validate(
      need(nrow(dev_df) > 0,
           "Not enough data to compute deviations for that year.")
    )
    
    # order factors for nicer display
    dev_df <- dev_df %>%
      mutate(
        country_name = factor(country_name, levels = sort(unique(country_name))),
        series_name  = factor(series_name,  levels = sort(unique(series_name)))
      )
    
    ggplot(
      dev_df,
      aes(
        x = series_name,
        y = country_name,
        fill = deviation
      )
    ) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "#b2182b",
        mid = "white",
        high = "#2166ac",
        midpoint = 0,
        na.value = "grey90"
      ) +
      labs(
        title = paste0("Deviation from regional median (", input$heat_year, ")"),
        x     = "Indicator",
        y     = "Country",
        fill  = "Deviation"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      coord_equal()
  })
  
  # 6. ABOUT TAB CONTENT -----------------------------------------------
  output$about_html <- renderUI({
    HTML(paste0(
      "<div style='max-width:900px; line-height:1.5;'>",
      
      "<h4 style='font-weight:600; margin-top:0;'>What this dashboard shows</h4>",
      "<p>This dashboard explores health indicators for South Asian countries from ~2000–2020. ",
      "It mirrors the analysis described in the project write-up: ",
      "trend lines over time, India's trajectory vs the region (including IQR bands), ",
      "distributions, scatter relationships, and a deviation heatmap that highlights ",
      "which countries are above/below the regional median in any given year.</p>",
      
      "<hr/>",
      
      "<h5 style='font-weight:600;'>Data structure</h5>",
      "<ul>",
      "<li><b>country_name</b>: Country or economy name (e.g. India).</li>",
      "<li><b>series_name</b>: Health indicator name (e.g. Life expectancy).</li>",
      "<li><b>year</b>: Year (numeric).</li>",
      "<li><b>value</b>: Numeric value for that indicator in that country and year.</li>",
      "</ul>",
      "<p>The CSV is expected to be in 'wide' format (columns like 2001, 2002, ...). ",
      "The app converts it to a tidy 'long' format automatically with ",
      "<code>pivot_longer()</code>. Column names are cleaned using ",
      "<code>janitor::clean_names()</code>, and columns like <code>x2005</code> ",
      "are normalized to <code>2005</code>.</p>",
      
      "<hr/>",
      
      "<h5 style='font-weight:600;'>Plot definitions</h5>",
      
      "<p><b>1. Trends (3x3 grid)</b><br/>",
      "Time-series lines by year for up to 9 indicators you select. ",
      "If multiple countries are loaded, each country is a different color in each panel, ",
      "similar to the 'Yearly Trends by Country' figure.</p>",
      
      "<p><b>2. Country vs IQR</b><br/>",
      "Red line = selected country (default India). ",
      "Gray band = regional IQR (25th–75th percentile across countries for each year). ",
      "Dashed line = regional median. ",
      "This matches the 'India’s Value vs Regional IQR' figure.</p>",
      
      "<p><b>3. Country vs Median</b><br/>",
      "Red line = selected country, dashed line = South Asia median per year. ",
      "Shows where the country is above/below typical regional levels.</p>",
      
      "<p><b>4. Distribution</b><br/>",
      "Histogram of one indicator in a single year across all available countries, ",
      "to highlight spread, outliers, and inequality.</p>",
      
      "<p><b>5. Immunization vs Mortality</b><br/>",
      "Scatter plot of DPT immunization (%) vs under-5 mortality, including a linear fit. ",
      "Typically shows a strong negative relationship: higher immunization, lower mortality.</p>",
      
      "<p><b>6. Spend vs Life Expectancy</b><br/>",
      "Scatter of health expenditure (% of GDP) vs life expectancy. ",
      "More spending tends to cluster with higher life expectancy overall, with outliers.</p>",
      
      "<p><b>7. Deviation Heatmap</b><br/>",
      "For a chosen year, we compute how far each country is above or below the regional ",
      "median value for each indicator. Blue = above median, red = below. ",
      "This is the 'Regional Deviation Heatmap' concept from the report.</p>",
      
      "<hr/>",
      
      "<h5 style='font-weight:600;'>Caveats</h5>",
      "<ul>",
      "<li>All medians and IQRs are computed only from whatever countries are in the loaded dataset here. ",
      "If your CSV is missing a country, it won't influence the regional baseline.</li>",
      "<li>Some years may have missing values or NA for an indicator in certain countries.</li>",
      "<li>Plots show correlation, not guaranteed causation.</li>",
      "</ul>",
      
      "<p class='text-muted' style='font-size:0.8rem;'>Built with Shiny · bslib · ggplot2 · dplyr · tidyr · patchwork</p>",
      
      "</div>"
    ))
  })
}

shinyApp(ui, server)

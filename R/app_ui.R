# UI: layout, bslib theme, baseline inputs/outputs

limits <- news_count_limits()
dr <- default_date_range()

app_ui <- function(request) {
  page_sidebar(
    title = tr("RUS", "app_title"),
    theme = bs_theme(
      version = 5,
      bootswatch = "zephyr",
      primary = "#0d6efd",
      "navbar-bg" = "#0b2239"
    ),
    sidebar = sidebar(
      width = 360,
      selectizeInput(
        inputId = "country",
        label = "Country",
        choices = country_choices,
        options = list(
          placeholder = "Country name, ISO2 (BR), or ISO3 (BRA)",
          maxOptions = length(country_choices)
        )
      ),
      dateRangeInput(
        inputId = "date_range",
        label = paste0("Period (Maximum span: ", max_period_months(), " months)"),
        start = dr[["start"]],
        end = dr[["end"]],
        max = Sys.Date(),
        format = ui_date_format(),
        separator = " - ",
        weekstart = 1
      ),
      actionButton("btn_last_6m", "Last 6 months", class = "btn-outline-secondary btn-sm w-100 mb-3"),
      checkboxInput("all_topics", "Search all topics", value = TRUE),
      conditionalPanel(
        condition = "input.all_topics == false",
        tagList(
          checkboxGroupInput(
            inputId = "topics",
            label = "Topics",
            choices = topic_choices,
            selected = all_topic_ids
          ),
          fluidRow(
            column(6, actionLink("topics_select_all", "Select all")),
            column(6, actionLink("topics_clear", "Clear all"))
          )
        )
      ),
      checkboxInput("followup_enabled", "Include follow-up topics", value = FALSE),
      conditionalPanel(
        condition = "input.followup_enabled == true",
        textAreaInput(
          inputId = "followup_topics",
          label = "Follow-up topics (one per line or separated by `;`)",
          rows = 4,
          placeholder = "e.g. banking stress; IMF talks; capital controls"
        )
      ),
      numericInput(
        inputId = "news_count",
        label = "Number of news items (N)",
        value = default_news_count(),
        min = limits$min,
        max = limits$max,
        step = 1L
      ),
      selectInput(
        inputId = "report_lang",
        label = "Output report language",
        choices = c("RUS", "ENG"),
        selected = "RUS"
      ),
      if (!openai_has_api_key()) {
        p(
          class = "text-warning small mb-2",
          "OPENAI_API_KEY is not set — “Find news” cannot load results until the key is configured."
        )
      },
      actionButton("run", "Find news", class = "btn-primary w-100")
    ),
    tags$head(
      tags$style(HTML(
        ".results-panel, .export-panel { min-height: 32rem; }\
         .export-panel {\
           position: sticky;\
           top: 1rem;\
           align-self: start;\
           max-height: calc(100vh - 2rem);\
         }\
         .status-panel-horizontal { min-height: 12rem; }\
         .status-scroll { max-height: 14rem; overflow-y: auto; }\
         .status-scroll pre { white-space: pre-wrap; word-break: break-word; }\
         .results-scroll { min-height: 26rem; max-height: 70vh; overflow-y: auto; }\
         .export-scroll { max-height: 70vh; overflow-y: auto; }\
         .export-actions-sticky {\
           position: sticky;\
           top: 0;\
           z-index: 5;\
           background: var(--bs-body-bg);\
           border-bottom: 1px solid var(--bs-border-color);\
           padding-bottom: .75rem;\
           margin-bottom: .75rem;\
         }\
         @media (max-width: 991.98px) {\
           .export-panel {\
             position: static;\
             max-height: none;\
           }\
           .export-actions-sticky {\
             position: sticky;\
             bottom: 0;\
             top: auto;\
             border-top: 1px solid var(--bs-border-color);\
             border-bottom: 0;\
             margin-top: .75rem;\
             margin-bottom: 0;\
             padding-top: .75rem;\
             padding-bottom: 0;\
           }\
         }\
         .selectize-control .selectize-dropdown .selectize-dropdown-content {\
           max-height: min(60vh, 24rem) !important;\
           overflow-y: auto !important;\
           overscroll-behavior: contain;\
         }"
      ))
    ),
    layout_columns(
      col_widths = c(9, 3),
      gap = "1rem",
      fill = FALSE,
      card(
        class = "results-panel",
        full_screen = TRUE,
        card_header(textOutput("results_header", inline = TRUE)),
        card_body(
          class = "results-scroll",
          uiOutput("results")
        )
      ),
      card(
        class = "export-panel",
        card_header(textOutput("export_header", inline = TRUE)),
        card_body(
          class = "export-scroll",
          uiOutput("export_controls")
        )
      )
    ),
    card(
      class = "status-panel-horizontal mt-3",
      full_screen = TRUE,
      card_header(textOutput("status_header", inline = TRUE)),
      card_body(
        class = "status-scroll",
        verbatimTextOutput("status", placeholder = TRUE)
      )
    )
  )
}

# UI: layout, bslib theme, baseline inputs/outputs

limits <- news_count_limits()
dr <- default_date_range()

app_ui <- function(request) {
  page_sidebar(
    title = "Sovereign risk — economic news",
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
          maxOptions = 20
        )
      ),
      dateRangeInput(
        inputId = "date_range",
        label = "Period",
        start = dr[["start"]],
        end = dr[["end"]],
        max = Sys.Date()
      ),
      p(
        class = "text-muted small",
        paste0(
          "Maximum span: ",
          max_period_months(),
          " months (~",
          max_period_days(),
          " days). Cache TTL: ",
          round(news_cache_ttl_sec() / 3600, 1),
          " h."
        )
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
      if (openai_has_api_key()) {
        p(class = "text-muted small mb-2", "OpenAI key is set — requests use the live API (may take 1–3 minutes).")
      } else {
        p(
          class = "text-warning small mb-2",
          "OPENAI_API_KEY is not set — “Find news” cannot load results until the key is configured."
        )
      },
      actionButton("run", "Find news", class = "btn-primary w-100")
    ),
    card(
      full_screen = TRUE,
      card_header("Status"),
      verbatimTextOutput("status", placeholder = TRUE)
    ),
    card(
      full_screen = TRUE,
      card_header("Results"),
      uiOutput("results")
    ),
    card(
      card_header("Export"),
      p(class = "text-muted small", "Download the last validated response (same data as the cards)."),
      fluidRow(
        column(4, downloadButton("export_json", "JSON", class = "btn-outline-secondary w-100")),
        column(4, downloadButton("export_csv", "CSV", class = "btn-outline-secondary w-100")),
        column(4, downloadButton("export_docx", "Word (DOCX)", class = "btn-outline-secondary w-100"))
      )
    )
  )
}
